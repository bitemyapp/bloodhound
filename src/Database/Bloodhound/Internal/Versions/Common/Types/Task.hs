{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Task where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data TaskResponse a = TaskResponse
  { taskResponseCompleted :: Bool,
    taskResponseTask :: Task a,
    taskResponseReponse :: Maybe a,
    taskResponseError :: Maybe Object
  }
  deriving (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (TaskResponse a) where
  parseJSON = withObject "TaskResponse" $ \v ->
    TaskResponse
      <$> v .: "completed"
      <*> v .: "task"
      <*> v .:? "reponse"
      <*> v .:? "error"

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
  parseJSON = withObject "Task" $ \v ->
    Task
      <$> v .: "node"
      <*> v .: "id"
      <*> v .: "type"
      <*> v .: "action"
      <*> v .: "status"
      <*> v .: "description"
      <*> v .: "start_time_in_millis"
      <*> v .: "running_time_in_nanos"
      <*> v .: "cancellable"

newtype TaskNodeId = TaskNodeId Text
  deriving (Show, Eq)

instance FromJSON TaskNodeId where
  parseJSON = withObject "TaskNodeId" $ \o -> TaskNodeId <$> o .: "task"
