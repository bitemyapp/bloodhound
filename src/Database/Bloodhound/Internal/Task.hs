{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Bloodhound.Internal.Task where

import           Data.Aeson
import           Data.Text      (Text)
import           Deriving.Aeson

data TaskResponse a = TaskResponse { taskResponseCompleted :: Bool
                                   , taskResponseTask      :: Task a
                                   , taskResponseReponse   :: a
                                   }
              deriving (Show, Eq, Generic)
              deriving (FromJSON)
              via CustomJSON '[ OmitNothingFields
                              , FieldLabelModifier (StripPrefix "taskResponse", CamelToSnake)
                              ] (TaskResponse a)

data Task a = Task { taskNode               :: Text
                   , taskId                 :: Int
                   , taskType               :: Text
                   , taskAction             :: Text
                   , taskStatus             :: a
                   , taskDescription        :: Text
                   , taskStartTimeInMillis  :: Integer
                   , taskRunningTimeInNanos :: Integer
                   , taskCancellable        :: Bool
                   }
              deriving (Show, Eq, Generic)
              deriving (FromJSON)
              via CustomJSON '[ OmitNothingFields
                              , FieldLabelModifier (StripPrefix "task", CamelToSnake)
                              ] (Task a)

newtype TaskNodeId = TaskNodeId Text
                   deriving (Show, Eq)

instance FromJSON TaskNodeId where
  parseJSON = withObject "TaskNodeId" $ \o -> TaskNodeId <$> o .: "task"
