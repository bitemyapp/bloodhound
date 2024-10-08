{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Bulk
  ( -- * Request
    BulkOperation (..),
    UpsertActionMetadata (..),
    UpsertPayload (..),
    buildUpsertActionMetadata,

    -- * Response
    BulkResponse (..),
    BulkActionItem (..),
    BulkItem (..),
    BulkAction (..),
    BulkError (..),

    -- * Optics
    bulkTookLens,
    bulkErrorsLens,
    bulkActionItemsLens,
    baiActionLens,
    baiItemLens,
    biIndexLens,
    biIdLens,
    biStatusLens,
    biErrorLens,
    beTypeLens,
    beReasonLens,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.Types as A
import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import Database.Bloodhound.Internal.Versions.Common.Types.Query

data UpsertActionMetadata
  = UA_RetryOnConflict Int
  | UA_Version Int
  deriving stock (Eq, Show)

buildUpsertActionMetadata :: UpsertActionMetadata -> Pair
buildUpsertActionMetadata (UA_RetryOnConflict i) = "retry_on_conflict" .= i
buildUpsertActionMetadata (UA_Version i) = "_version" .= i

data UpsertPayload
  = UpsertDoc Value
  | UpsertScript Bool Script Value
  deriving stock (Eq, Show)

-- | 'BulkOperation' is a sum type for expressing the four kinds of bulk
--   operation index, create, delete, and update. 'BulkIndex' behaves like an
--   "upsert", 'BulkCreate' will fail if a document already exists at the DocId.
--   Consult the <http://www.elastic.co/guide/en/elasticsearch/reference/current/docs-bulk.html#docs-bulk Bulk API documentation>
--   for further explanation.
--   Warning: Bulk operations suffixed with @Auto@ rely on Elasticsearch to
--   generate the id. Often, people use auto-generated identifiers when
--   Elasticsearch is the only place that their data is stored. Do not let
--   Elasticsearch be the only place your data is stored. It does not guarantee
--   durability, and it may silently discard data.
--   This <https://github.com/elastic/elasticsearch/issues/10708 issue> is
--   discussed further on github.
data BulkOperation
  = -- | Create the document, replacing it if it already exists.
    BulkIndex IndexName DocId Value
  | -- | Create a document with an autogenerated id.
    BulkIndexAuto IndexName Value
  | -- | Create a document with an autogenerated id. Use fast JSON encoding.
    BulkIndexEncodingAuto IndexName Encoding
  | -- | Create a document, failing if it already exists.
    BulkCreate IndexName DocId Value
  | -- | Create a document, failing if it already exists. Use fast JSON encoding.
    BulkCreateEncoding IndexName DocId Encoding
  | -- | Delete the document
    BulkDelete IndexName DocId
  | -- | Update the document, merging the new value with the existing one.
    BulkUpdate IndexName DocId Value
  | -- | Update the document if it already exists, otherwise insert it.
    BulkUpsert IndexName DocId UpsertPayload [UpsertActionMetadata]
  deriving stock (Eq, Show)

data BulkResponse = BulkResponse
  { bulkTook :: Int,
    bulkErrors :: Bool,
    bulkActionItems :: [BulkActionItem]
  }
  deriving stock (Eq, Show)

bulkTookLens :: Lens' BulkResponse Int
bulkTookLens = lens bulkTook (\x y -> x {bulkTook = y})

bulkErrorsLens :: Lens' BulkResponse Bool
bulkErrorsLens = lens bulkErrors (\x y -> x {bulkErrors = y})

bulkActionItemsLens :: Lens' BulkResponse [BulkActionItem]
bulkActionItemsLens = lens bulkActionItems (\x y -> x {bulkActionItems = y})

data BulkActionItem = BulkActionItem
  { baiAction :: BulkAction,
    baiItem :: BulkItem
  }
  deriving stock (Eq, Show)

baiActionLens :: Lens' BulkActionItem BulkAction
baiActionLens = lens baiAction (\x y -> x {baiAction = y})

baiItemLens :: Lens' BulkActionItem BulkItem
baiItemLens = lens baiItem (\x y -> x {baiItem = y})

data BulkItem = BulkItem
  { biIndex :: Text,
    biId :: Text,
    biStatus :: Maybe Int,
    biError :: Maybe BulkError
  }
  deriving stock (Eq, Show)

biIndexLens :: Lens' BulkItem Text
biIndexLens = lens biIndex (\x y -> x {biIndex = y})

biIdLens :: Lens' BulkItem Text
biIdLens = lens biId (\x y -> x {biId = y})

biStatusLens :: Lens' BulkItem (Maybe Int)
biStatusLens = lens biStatus (\x y -> x {biStatus = y})

biErrorLens :: Lens' BulkItem (Maybe BulkError)
biErrorLens = lens biError (\x y -> x {biError = y})

data BulkAction = Index | Create | Delete | Update
  deriving stock (Eq, Show)

data BulkError = BulkError
  { beType :: Text,
    beReason :: Text
  }
  deriving stock (Eq, Show)

beTypeLens :: Lens' BulkError Text
beTypeLens = lens beType (\x y -> x {beType = y})

beReasonLens :: Lens' BulkError Text
beReasonLens = lens beReason (\x y -> x {beReason = y})

instance FromJSON BulkResponse where
  parseJSON = withObject "BulkResponse" $ \o ->
    BulkResponse <$> o .: "took" <*> o .: "errors" <*> o .: "items"

instance FromJSON BulkActionItem where
  parseJSON j =
    parseItem Index j
      <|> parseItem Create j
      <|> parseItem Delete j
      <|> parseItem Update j
    where
      -- \| The object has a single key: value pair, where the key encodes
      -- the action.
      parseItem :: BulkAction -> A.Value -> A.Parser BulkActionItem
      parseItem action = withObject "BulkActionItem" $ \o -> do
        v <- o .: A.fromText actionText
        pure $! BulkActionItem {baiAction = action, baiItem = v}
        where
          actionText :: Text
          actionText = case action of
            Index -> "index"
            Create -> "create"
            Delete -> "delete"
            Update -> "update"

instance FromJSON BulkItem where
  parseJSON = withObject "BulkItem" $ \o ->
    BulkItem
      <$> o
        .: "_index"
      <*> o
        .: "_id"
      <*> o
        .:? "status" -- allegedly present but ES example shows a case where it is missing.. so..
      <*> o
        .:? "error"

instance FromJSON BulkError where
  parseJSON = withObject "BulkError" $
    \o -> BulkError <$> o .: "type" <*> o .: "reason"
