{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-------------------------------------------------------------------------------
-- |
-- Module : Database.Bloodhound.Types.Internal
-- Copyright : (C) 2014 Chris Allen
-- License : BSD-style (see the file LICENSE)
-- Maintainer : Chris Allen <cma@bitemyapp.com>
-- Stability : provisional
-- Portability : DeriveGeneric, RecordWildCards
--
-- Internal data types for Bloodhound. These types may change without
-- notice so import at your own risk.
-------------------------------------------------------------------------------
module Database.V5.Bloodhound.Types.Internal
    ( BHEnv(..)
    , Server(..)
    , MonadBH(..)
    ) where


import           Control.Applicative  as A
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Text            (Text)
import           Data.Typeable        (Typeable)
import           GHC.Generics         (Generic)
import           Network.HTTP.Client

{-| Common environment for Elasticsearch calls. Connections will be
    pipelined according to the provided HTTP connection manager.
-}
data BHEnv = BHEnv { bhServer      :: Server
                   , bhManager     :: Manager
                   , bhRequestHook :: Request -> IO Request
                   -- ^ Low-level hook that is run before every request is sent. Used to implement custom authentication strategies. Defaults to 'return' with 'mkBHEnv'.
                   }

instance (Functor m, Applicative m, MonadIO m) => MonadBH (ReaderT BHEnv m) where
  getBHEnv = ask

{-| 'Server' is used with the client functions to point at the ES instance
-}
newtype Server = Server Text deriving (Eq, Show, Generic, Typeable, FromJSON)

{-| All API calls to Elasticsearch operate within
    MonadBH
    . The idea is that it can be easily embedded in your
    own monad transformer stack. A default instance for a ReaderT and
    alias 'BH' is provided for the simple case.
-}
class (Functor m, A.Applicative m, MonadIO m) => MonadBH m where
  getBHEnv :: m BHEnv

