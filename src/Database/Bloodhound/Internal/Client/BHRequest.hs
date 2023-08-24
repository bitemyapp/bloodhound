{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module : Database.Bloodhound.Client
-- Copyright : (C) 2014, 2018 Chris Allen
-- License : BSD-style (see the file LICENSE)
-- Maintainer : Gautier DI FOLCO <gautier.difolco@gmail.com>
-- Stability : provisional
-- Portability : GHC
--
-- Client side abstractions to interact with Elasticsearch servers.
module Database.Bloodhound.Internal.Client.BHRequest
  ( -- * Request
    BHRequest (..),
    StatusIndependant,
    StatusDependant,
    mkFullRequest,
    mkSimpleRequest,
    ParsedEsResponse,
    ParseBHResponse (..),
    Server (..),
    Endpoint (..),
    mkEndpoint,
    withQueries,
    getEndpoint,
    withBHResponse,
    withBHResponse_,
    withBHResponseParsedEsResponse,
    keepBHResponse,
    joinBHResponse,

    -- * Response
    BHResponse (..),

    -- * Response interpretation
    decodeResponse,
    eitherDecodeResponse,
    parseEsResponse,
    parseEsResponseWith,
    isVersionConflict,
    isSuccess,
    isCreated,
    statusCodeIs,

    -- * Response handling
    EsProtocolException (..),
    EsResult (..),
    EsResultFound (..),
    EsError (..),

    -- * Common results
    Acknowledged (..),
    Accepted (..),
    IgnoredBody (..),
  )
where

import qualified Blaze.ByteString.Builder as BB
import Control.Applicative as A
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Ix
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable
import Database.Bloodhound.Internal.Client.Doc
import GHC.Exts
import Network.HTTP.Client
import qualified Network.HTTP.Types.Method as NHTM
import qualified Network.HTTP.Types.Status as NHTS
import qualified Network.HTTP.Types.URI as NHTU
import Prelude hiding (filter, head)

-- | 'Server' is used with the client functions to point at the ES instance
newtype Server = Server Text
  deriving stock (Eq, Show)
  deriving newtype (FromJSON)

-- | 'Endpoint' represents an url before being built
data Endpoint = Endpoint
  { getRawEndpoint :: [Text],
    getRawEndpointQueries :: [(Text, Maybe Text)]
  }
  deriving stock (Eq, Show)

instance IsList Endpoint where
  type Item Endpoint = Text
  toList = getRawEndpoint
  fromList = mkEndpoint

-- | Create an 'Endpoint' from a list of url parts
mkEndpoint :: [Text] -> Endpoint
mkEndpoint urlParts = Endpoint urlParts mempty

-- | Generate the raw URL
getEndpoint :: Server -> Endpoint -> Text
getEndpoint (Server serverRoot) endpoint =
  T.intercalate "/" (serverRoot : getRawEndpoint endpoint) <> queries
  where
    queries = T.decodeUtf8 $ BB.toByteString $ NHTU.renderQueryText prependQuestionMark $ getRawEndpointQueries endpoint
    prependQuestionMark = True

-- | Severely dumbed down query renderer. Assumes your data doesn't
-- need any encoding
withQueries :: Endpoint -> [(Text, Maybe Text)] -> Endpoint
withQueries endpoint queries = endpoint {getRawEndpointQueries = getRawEndpointQueries endpoint <> queries}

-- | 'Request' upon Elasticsearch's server.
--
-- @parsingContext@ is a phantom type for the expected status-dependancy
-- @responseBody@ is a phantom type for the expected result
data BHRequest parsingContext responseBody = BHRequest
  { bhRequestMethod :: NHTM.Method,
    bhRequestEndpoint :: Endpoint,
    bhRequestBody :: Maybe BL.ByteString,
    bhRequestParser :: BHResponse parsingContext responseBody -> Either EsProtocolException (ParsedEsResponse responseBody)
  }

instance Functor (BHRequest parsingContext) where
  fmap f req =
    req
      { bhRequestParser =
          \BHResponse {..} -> fmap (fmap f) $ bhRequestParser req $ BHResponse {..}
      }

-- | 'BHResponse' body-parsing does not depend on 'statusCode'
data StatusIndependant

-- | 'BHResponse' body-parsing may depend on 'statusCode'
data StatusDependant

-- | 'BHRequest' with a body
mkFullRequest :: (ParseBHResponse parsingContext, FromJSON responseBody) => NHTM.Method -> Endpoint -> BL.ByteString -> BHRequest parsingContext responseBody
mkFullRequest method' endpoint body =
  BHRequest
    { bhRequestMethod = method',
      bhRequestEndpoint = endpoint,
      bhRequestBody = Just body,
      bhRequestParser = parseBHResponse
    }

-- | 'BHRequest' without a body
mkSimpleRequest :: (ParseBHResponse parsingContext, FromJSON responseBody) => NHTM.Method -> Endpoint -> BHRequest parsingContext responseBody
mkSimpleRequest method' endpoint =
  BHRequest
    { bhRequestMethod = method',
      bhRequestEndpoint = endpoint,
      bhRequestBody = Nothing,
      bhRequestParser = parseBHResponse
    }

class ParseBHResponse parsingContext where
  parseBHResponse ::
    FromJSON a =>
    BHResponse parsingContext a ->
    Either EsProtocolException (ParsedEsResponse a)

instance ParseBHResponse StatusDependant where
  parseBHResponse = parseEsResponse

instance ParseBHResponse StatusIndependant where
  parseBHResponse r =
    return $
      case eitherDecodeResponse r of
        Right d -> Right d
        Left e ->
          Left $
            EsError
              { errorStatus = NHTS.statusCode (responseStatus $ getResponse r),
                errorMessage = "Unable to parse body: " <> T.pack e
              }

-- | Work with the full 'BHResponse'
withBHResponse ::
  forall a parsingContext b.
  (Either EsProtocolException (ParsedEsResponse a) -> BHResponse StatusDependant a -> b) ->
  BHRequest parsingContext a ->
  BHRequest StatusDependant b
withBHResponse f req =
  req
    { bhRequestParser = \resp ->
        liftResponse $ f (bhRequestParser req $ castResponse @_ @_ @parsingContext @a resp) $ castResponse @_ @_ @StatusDependant @a resp
    }
  where
    liftResponse :: b -> Either EsProtocolException (ParsedEsResponse b)
    liftResponse = return . return

-- | Internal only
castResponse :: BHResponse parsingContext0 responseBody0 -> BHResponse parsingContext1 responseBody1
castResponse BHResponse {..} = BHResponse {..}

-- | Work with the full 'BHResponse'
withBHResponse_ ::
  forall a parsingContext b.
  (BHResponse StatusDependant a -> b) ->
  BHRequest parsingContext a ->
  BHRequest StatusDependant b
withBHResponse_ f = withBHResponse $ const f

-- | Enable working with 'ParsedEsResponse'
withBHResponseParsedEsResponse ::
  forall a parsingContext.
  BHRequest parsingContext a ->
  BHRequest StatusDependant (ParsedEsResponse a)
withBHResponseParsedEsResponse req =
  req
    { bhRequestParser = \BHResponse {..} -> return <$> bhRequestParser req BHResponse {..}
    }

-- | Keep with the full 'BHResponse'
keepBHResponse ::
  forall a parsingContext.
  BHRequest parsingContext a ->
  BHRequest StatusDependant (BHResponse StatusDependant a, a)
keepBHResponse = joinBHResponse . withBHResponse (\parsed resp -> fmap (fmap ((,) resp)) parsed)

joinBHResponse ::
  forall a parsingContext.
  BHRequest parsingContext (Either EsProtocolException (ParsedEsResponse a)) ->
  BHRequest parsingContext a
joinBHResponse req =
  req
    { bhRequestParser = \resp ->
        case bhRequestParser req $ castResponse resp of
          Left e -> Left e
          Right (Right a) -> a
          Right (Left e) -> Right (Left e)
    }

-- | Result of a 'BHRequest'
newtype BHResponse parsingContext body = BHResponse
  { getResponse :: Network.HTTP.Client.Response BL.ByteString
  }
  deriving stock (Show)

-- | Result of a 'parseEsResponse'
type ParsedEsResponse a = Either EsError a

-- | Tries to parse a response body as the expected type @body@ and
-- failing that tries to parse it as an EsError. All well-formed, JSON
-- responses from elasticsearch should fall into these two
-- categories. If they don't, a 'EsProtocolException' will be
-- thrown. If you encounter this, please report the full body it
-- reports along with your Elasticsearch version.
parseEsResponse ::
  FromJSON body =>
  BHResponse parsingContext body ->
  Either EsProtocolException (ParsedEsResponse body)
parseEsResponse response
  | isSuccess response = case eitherDecode body of
      Right a -> return (Right a)
      Left err ->
        tryParseError err
  | otherwise = tryParseError "Non-200 status code"
  where
    body = responseBody $ getResponse response
    tryParseError originalError =
      case eitherDecode body of
        Right e -> return (Left e)
        -- Failed to parse the error message.
        Left err -> explode ("Original error was: " <> originalError <> " Error parse failure was: " <> err)
    explode errorMsg = Left $ EsProtocolException (T.pack errorMsg) body

-- | Parse 'BHResponse' with an arbitrary parser
parseEsResponseWith ::
  ( MonadThrow m,
    FromJSON body
  ) =>
  (body -> Either String parsed) ->
  BHResponse parsingContext body ->
  m parsed
parseEsResponseWith parser response =
  case eitherDecode body of
    Left e -> explode e
    Right parsed ->
      case parser parsed of
        Right a -> return a
        Left e -> explode e
  where
    body = responseBody $ getResponse response
    explode errorMsg = throwM $ EsProtocolException (T.pack errorMsg) body

-- | Helper around 'aeson' 'decode'
decodeResponse ::
  FromJSON a =>
  BHResponse StatusIndependant a ->
  Maybe a
decodeResponse = decode . responseBody . getResponse

-- | Helper around 'aeson' 'eitherDecode'
eitherDecodeResponse ::
  FromJSON a =>
  BHResponse StatusIndependant a ->
  Either String a
eitherDecodeResponse = eitherDecode . responseBody . getResponse

-- | Was there an optimistic concurrency control conflict when
-- indexing a document?
isVersionConflict :: BHResponse parsingContext a -> Bool
isVersionConflict = statusCheck (== 409)

-- | Check '2xx' status codes
isSuccess :: BHResponse parsingContext a -> Bool
isSuccess = statusCodeIs (200, 299)

-- | Check '201' status code
isCreated :: BHResponse parsingContext a -> Bool
isCreated = statusCheck (== 201)

-- | Check status code
statusCheck :: (Int -> Bool) -> BHResponse parsingContext a -> Bool
statusCheck prd = prd . NHTS.statusCode . responseStatus . getResponse

-- | Check status code in range
statusCodeIs :: (Int, Int) -> BHResponse parsingContext body -> Bool
statusCodeIs r resp = inRange r $ NHTS.statusCode (responseStatus $ getResponse resp)

-- | 'EsResult' describes the standard wrapper JSON document that you see in
--    successful Elasticsearch lookups or lookups that couldn't find the document.
data EsResult a = EsResult
  { _index :: Text,
    _type :: Maybe Text,
    _id :: Text,
    foundResult :: Maybe (EsResultFound a)
  }
  deriving (Eq, Show)

-- | 'EsResultFound' contains the document and its metadata inside of an
--    'EsResult' when the document was successfully found.
data EsResultFound a = EsResultFound
  { _version :: DocVersion,
    _source :: a
  }
  deriving (Eq, Show)

instance (FromJSON a) => FromJSON (EsResult a) where
  parseJSON jsonVal@(Object v) = do
    found <- v .:? "found" .!= False
    fr <-
      if found
        then parseJSON jsonVal
        else return Nothing
    EsResult
      <$> v
        .: "_index"
      <*> v
        .:? "_type"
      <*> v
        .: "_id"
      <*> pure fr
  parseJSON _ = empty

instance (FromJSON a) => FromJSON (EsResultFound a) where
  parseJSON (Object v) =
    EsResultFound
      <$> v
        .: "_version"
      <*> v
        .: "_source"
  parseJSON _ = empty

-- | 'EsError' is the generic type that will be returned when there was a
--    problem. If you can't parse the expected response, its a good idea to
--    try parsing this.
data EsError = EsError
  { errorStatus :: Int,
    errorMessage :: Text
  }
  deriving (Eq, Show, Typeable)

instance Exception EsError

instance Semigroup EsError where
  _ <> x = x

instance Monoid EsError where
  mempty = EsError 0 "Monoid value, shouldn't happen"

instance FromJSON EsError where
  parseJSON (Object v) =
    EsError
      <$> v
        .: "status"
      <*> (v .: "error" <|> (v .: "error" >>= (.: "reason")))
  parseJSON _ = empty

-- | 'EsProtocolException' will be thrown if Bloodhound cannot parse a response
-- returned by the Elasticsearch server. If you encounter this error, please
-- verify that your domain data types and FromJSON instances are working properly
-- (for example, the 'a' of '[Hit a]' in 'SearchResult.searchHits.hits'). If you're
-- sure that your mappings are correct, then this error may be an indication of an
-- incompatibility between Bloodhound and Elasticsearch. Please open a bug report
-- and be sure to include the exception body.
data EsProtocolException = EsProtocolException
  { esProtoExMessage :: !Text,
    esProtoExResponse :: !BL.ByteString
  }
  deriving (Eq, Show)

instance Exception EsProtocolException

newtype Acknowledged = Acknowledged {isAcknowledged :: Bool}
  deriving stock (Eq, Show)

instance FromJSON Acknowledged where
  parseJSON =
    withObject "Acknowledged" $
      fmap Acknowledged . (.: "acknowledged")

newtype Accepted = Accepted {isAccepted :: Bool}
  deriving stock (Eq, Show)

instance FromJSON Accepted where
  parseJSON =
    withObject "Accepted" $
      fmap Accepted . (.: "accepted")

data IgnoredBody = IgnoredBody
  deriving stock (Eq, Show)

instance FromJSON IgnoredBody where
  parseJSON _ = return IgnoredBody
