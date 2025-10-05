{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Bloodhound.Client.Cluster
  ( module ReexportCompat,
    BH (..),
    BHEnv (..),
    MonadBH (..),
    BackendType (..),
    WithBackend,
    WithBackendType,
    StaticBH (..),
    SBackendType (..),
    emptyBody,
    mkBHEnv,
    performBHRequest,
    runBH,
    tryPerformBHRequest,
    unsafePerformBH,
    withDynamicBH,
  )
where

import Control.Monad.Catch
import Control.Monad.Except
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Kind
import qualified Data.Text as T
import Database.Bloodhound.Internal.Client.BHRequest
import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Versions.Common.Types.Bulk as ReexportCompat
import Database.Bloodhound.Internal.Versions.Common.Types.Count as ReexportCompat
import Database.Bloodhound.Internal.Versions.Common.Types.Indices as ReexportCompat
import Database.Bloodhound.Internal.Versions.Common.Types.Nodes as ReexportCompat
import Database.Bloodhound.Internal.Versions.Common.Types.Snapshots as ReexportCompat
import Database.Bloodhound.Internal.Versions.Common.Types.Units as ReexportCompat
import qualified Network.HTTP.Client as HTTP
import qualified Network.URI as URI

-- | Common environment for Elasticsearch calls. Connections will be
--   pipelined according to the provided HTTP connection manager.
data BHEnv = BHEnv
  { bhServer :: Server,
    bhManager :: HTTP.Manager,
    -- | Low-level hook that is run before every request is sent. Used to implement custom authentication strategies. Defaults to 'return' with 'mkBHEnv'.
    bhRequestHook :: HTTP.Request -> IO HTTP.Request
  }

-- | All API calls to Elasticsearch operate within
--   MonadBH
--   . The idea is that it can be easily embedded in your
--   own monad transformer stack. A default instance for a ReaderT and
--   alias 'BH' is provided for the simple case.
class (Functor m, Applicative m, MonadIO m, MonadCatch m) => MonadBH m where
  type Backend m :: BackendType
  dispatch :: BHRequest contextualized body -> m (BHResponse contextualized body)
  tryEsError :: m a -> m (Either EsError a)
  throwEsError :: EsError -> m a

-- | Backend (i.e. implementation) the queries are ran against
data BackendType
  = ElasticSearch7
  | OpenSearch1
  | OpenSearch2
  | -- | unknown, can be anything
    Dynamic

-- | Best-effort, by-passed for 'Dynamic', statically enforced implementation
type family WithBackendType (expected :: BackendType) (actual :: BackendType) :: Constraint where
  WithBackendType e 'Dynamic = ()
  WithBackendType e a = e ~ a

-- | Helper for 'WithBackendType'
type WithBackend (backend :: BackendType) (m :: Type -> Type) = WithBackendType backend (Backend m)

-- | Create a 'BHEnv' with all optional fields defaulted. HTTP hook
-- will be a noop. You can use the exported fields to customize
-- it further, e.g.:
--
-- >> (mkBHEnv myServer myManager) { bhRequestHook = customHook }
mkBHEnv :: Server -> HTTP.Manager -> BHEnv
mkBHEnv s m = BHEnv s m return

-- | Basic BH implementation
newtype BH m a = BH
  { unBH :: ReaderT BHEnv (ExceptT EsError m) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState s,
      MonadWriter w,
      Alternative,
      MonadPlus,
      MonadFix,
      MonadThrow,
      MonadCatch,
      MonadFail,
      MonadMask
    )

instance MonadTrans BH where
  lift = BH . lift . lift

instance (MonadReader r m) => MonadReader r (BH m) where
  ask = lift ask
  local f (BH (ReaderT m)) = BH $
    ReaderT $ \r ->
      local f (m r)

instance (Functor m, Applicative m, MonadIO m, MonadCatch m, MonadThrow m) => MonadBH (BH m) where
  type Backend (BH m) = 'Dynamic
  dispatch request = BH $ do
    env <- ask @BHEnv
    let url = getEndpoint (bhServer env) (bhRequestEndpoint request)
    initReq <- liftIO $ parseUrl' url
    let reqHook = bhRequestHook env
    let reqBody = HTTP.RequestBodyLBS $ fromMaybe emptyBody $ bhRequestBody request
    let setQueryStrings =
          case bhRequestQueryStrings request of
            [] -> id
            xs -> HTTP.setQueryString xs
    req <-
      liftIO $
        reqHook $
          HTTP.setRequestIgnoreStatus $
            setQueryStrings $
              initReq
                { HTTP.method = bhRequestMethod request,
                  HTTP.requestHeaders =
                    ("Content-Type", "application/json") : HTTP.requestHeaders initReq,
                  HTTP.requestBody = reqBody
                }

    let mgr = bhManager env
    BHResponse <$> liftIO (HTTP.httpLbs req mgr)
  tryEsError = try
  throwEsError = throwM

tryPerformBHRequest ::
  (MonadBH m, MonadThrow m, ParseBHResponse contextualized) =>
  BHRequest contextualized a ->
  m (ParsedEsResponse a)
tryPerformBHRequest req = dispatch req >>= either throwM return . bhRequestParser req

performBHRequest ::
  (MonadBH m, MonadThrow m, ParseBHResponse contextualized) =>
  BHRequest contextualized a ->
  m a
performBHRequest req = tryPerformBHRequest req >>= either throwEsError return

emptyBody :: L.ByteString
emptyBody = L.pack ""

parseUrl' :: (MonadThrow m) => Text -> m HTTP.Request
parseUrl' t = HTTP.parseRequest (URI.escapeURIString URI.isAllowedInURI (T.unpack t))

runBH :: BHEnv -> BH m a -> m (Either EsError a)
runBH e f = runExceptT $ runReaderT (unBH f) e

-- | Statically-type backend.
--
-- It's also an useful wrapper for 'DerivingVia'
newtype StaticBH (backend :: BackendType) m a = StaticBH
  {runStaticBH :: m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader r,
      MonadState s,
      MonadWriter w,
      Alternative,
      MonadPlus,
      MonadFix,
      MonadThrow,
      MonadCatch,
      MonadFail,
      MonadMask
    )

instance MonadTrans (StaticBH backend) where
  lift = StaticBH

instance (MonadBH m) => MonadBH (StaticBH backend m) where
  type Backend (StaticBH backend m) = backend
  dispatch = StaticBH . dispatch
  tryEsError = StaticBH . tryEsError . runStaticBH
  throwEsError = StaticBH . throwEsError

-- | Run a piece of code as-if we are on a given backend
unsafePerformBH :: StaticBH backend m a -> m a
unsafePerformBH = runStaticBH

-- | Dependently-typed version of 'BackendType'
data SBackendType :: BackendType -> Type where
  SElasticSearch7 :: SBackendType 'ElasticSearch7
  SOpenSearch1 :: SBackendType 'OpenSearch1
  SOpenSearch2 :: SBackendType 'OpenSearch2

-- | Run an action given an actual backend
withDynamicBH ::
  (MonadBH m) =>
  BackendType ->
  (forall backend. SBackendType backend -> StaticBH backend m a) ->
  m a
withDynamicBH backend f =
  case backend of
    ElasticSearch7 -> unsafePerformBH $ f SElasticSearch7
    OpenSearch1 -> unsafePerformBH $ f SOpenSearch1
    OpenSearch2 -> unsafePerformBH $ f SOpenSearch2
    Dynamic -> throwEsError $ EsError Nothing "Cannot perform on a 'Dynamic' backend"
