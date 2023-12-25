{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Task where

import Data.Aeson
import Data.Text (Text)
import Database.Bloodhound.Internal.Utils.Imports (optionsDerivingStrippingPrefix)
import Deriving.Aeson

data TaskResponse a = TaskResponse
  { taskResponseCompleted :: Bool,
    taskResponseTask :: Task a,
    taskResponseReponse :: Maybe a,
    taskResponseError :: Maybe Object
  }
  deriving (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (TaskResponse a) where
  parseJSON = genericParseJSON $ optionsDerivingStrippingPrefix "taskResponse"

data Task a = Task
  { taskNode :: Text,
    taskId :: Int,
    taskType :: Text,
    taskAction :: Text,
    taskStatus :: a,
    taskDescription :: Text,
    taskStartTimeInMillis :: Integer,
    taskRunningTimeInNanos :: Integer,
    taskCancellable :: Bool
  }
  deriving (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (Task a) where
  parseJSON = genericParseJSON $ optionsDerivingStrippingPrefix "task"

newtype TaskNodeId = TaskNodeId Text
  deriving (Show, Eq)

instance FromJSON TaskNodeId where
  parseJSON = withObject "TaskNodeId" $ \o -> TaskNodeId <$> o .: "task"
