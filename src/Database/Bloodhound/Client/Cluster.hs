{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Bloodhound.Client.Cluster
  ( module ReexportCompat,
    BH (..),
    BHEnv (..),
    MonadBH (..),
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
  dispatch :: BHRequest contextualized body -> m (BHResponse contextualized body)
  tryEsError :: m a -> m (Either EsError a)
  throwEsError :: EsError -> m a

-- | Create a 'BHEnv' with all optional fields defaulted. HTTP hook
-- will be a noop. You can use the exported fields to customize
-- it further, e.g.:
--
-- >> (mkBHEnv myServer myManager) { bhRequestHook = customHook }
mkBHEnv :: Server -> Manager -> BHEnv
mkBHEnv s m = BHEnv s m return

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
                  -- "application/x-ndjson" for bulk
                  ("Content-Type", "application/json") : requestHeaders initReq,
                requestBody = reqBody
              }
    -- req <- liftIO $ reqHook $ setRequestIgnoreStatus $ initReq { method = dMethod
    --                                                            , requestBody = reqBody }
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
