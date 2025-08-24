{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
    MkBH (..),
    emptyBody,
    mkBHEnv,
    performBHRequest,
    runBH,
    tryPerformBHRequest,
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
import Network.HTTP.Client
import qualified Network.URI as URI

-- | Common environment for Elasticsearch calls. Connections will be
--   pipelined according to the provided HTTP connection manager.
data BHEnv = BHEnv
  { bhServer :: Server,
    bhManager :: Manager,
    -- | Low-level hook that is run before every request is sent. Used to implement custom authentication strategies. Defaults to 'return' with 'mkBHEnv'.
    bhRequestHook :: Request -> IO Request
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
mkBHEnv :: Server -> Manager -> BHEnv
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
    let reqBody = RequestBodyLBS $ fromMaybe emptyBody $ bhRequestBody request
    req <-
      liftIO $
        reqHook $
          setRequestIgnoreStatus $
            initReq
              { method = bhRequestMethod request,
                requestHeaders =
                  ("Content-Type", "application/json") : requestHeaders initReq,
                requestBody = reqBody
              }

    let mgr = bhManager env
    BHResponse <$> liftIO (httpLbs req mgr)
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

parseUrl' :: (MonadThrow m) => Text -> m Request
parseUrl' t = parseRequest (URI.escapeURIString URI.isAllowedInURI (T.unpack t))

runBH :: BHEnv -> BH m a -> m (Either EsError a)
runBH e f = runExceptT $ runReaderT (unBH f) e

newtype MkBH (backend :: BackendType) m a = MkBH
  {runMkBH :: m a}
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

instance MonadTrans (MkBH backend) where
  lift = MkBH

instance (MonadBH m) => MonadBH (MkBH backend m) where
  type Backend (MkBH backend m) = backend
  dispatch = MkBH . dispatch
  tryEsError = MkBH . tryEsError . runMkBH
  throwEsError = MkBH . throwEsError
