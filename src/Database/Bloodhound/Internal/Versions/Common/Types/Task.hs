{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Task where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Optics.Lens

data TaskResponse a = TaskResponse
  { taskResponseCompleted :: Bool,
    taskResponseTask :: Task a,
    taskResponseReponse :: Maybe a,
    taskResponseError :: Maybe Object
  }
  deriving stock (Eq, Show, Generic)

instance (FromJSON a) => FromJSON (TaskResponse a) where
  parseJSON = withObject "TaskResponse" $ \v ->
    TaskResponse
      <$> v .: "completed"
      <*> v .: "task"
      <*> v .:? "reponse"
      <*> v .:? "error"

taskResponseCompletedLens :: Lens' (TaskResponse a) Bool
taskResponseCompletedLens = lens taskResponseCompleted (\x y -> x {taskResponseCompleted = y})

taskResponseTaskLens :: Lens' (TaskResponse a) (Task a)
taskResponseTaskLens = lens taskResponseTask (\x y -> x {taskResponseTask = y})

taskResponseReponseLens :: Lens' (TaskResponse a) (Maybe a)
taskResponseReponseLens = lens taskResponseReponse (\x y -> x {taskResponseReponse = y})

taskResponseErrorLens :: Lens' (TaskResponse a) (Maybe Object)
taskResponseErrorLens = lens taskResponseError (\x y -> x {taskResponseError = y})

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
  deriving stock (Eq, Show, Generic)

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
  deriving stock (Eq, Show)

instance FromJSON TaskNodeId where
  parseJSON = withObject "TaskNodeId" $ \o -> TaskNodeId <$> o .: "task"

taskNodeLens :: Lens' (Task a) Text
taskNodeLens = lens taskNode (\x y -> x {taskNode = y})

taskIdLens :: Lens' (Task a) Int
taskIdLens = lens taskId (\x y -> x {taskId = y})

taskTypeLens :: Lens' (Task a) Text
taskTypeLens = lens taskType (\x y -> x {taskType = y})

taskActionLens :: Lens' (Task a) Text
taskActionLens = lens taskAction (\x y -> x {taskAction = y})

taskStatusLens :: Lens' (Task a) a
taskStatusLens = lens taskStatus (\x y -> x {taskStatus = y})

taskDescriptionLens :: Lens' (Task a) Text
taskDescriptionLens = lens taskDescription (\x y -> x {taskDescription = y})

taskStartTimeInMillisLens :: Lens' (Task a) Integer
taskStartTimeInMillisLens = lens taskStartTimeInMillis (\x y -> x {taskStartTimeInMillis = y})

taskRunningTimeInNanosLens :: Lens' (Task a) Integer
taskRunningTimeInNanosLens = lens taskRunningTimeInNanos (\x y -> x {taskRunningTimeInNanos = y})

taskCancellableLens :: Lens' (Task a) Bool
taskCancellableLens = lens taskCancellable (\x y -> x {taskCancellable = y})
