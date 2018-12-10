module Bloodhound.Import
  ( module X
  , LByteString
  , Method
  , omitNulls
  , parseNEJSON
  , parseReadText
  , readMay
  , showText
  , deleteSeveral
  , oPath
  , tshow
  ) where

import           Control.Applicative       as X (Alternative (..), optional)
import           Control.Exception         as X (Exception)
import           Control.Monad             as X (MonadPlus (..), forM, (<=<))
import           Control.Monad.Catch       as X (MonadCatch, MonadMask,
                                                 MonadThrow)
import           Control.Monad.Except      as X (MonadError)
import           Control.Monad.Fix         as X (MonadFix)
import           Control.Monad.IO.Class    as X (MonadIO (..))
import           Control.Monad.Reader      as X (MonadReader (..),
                                                 MonadTrans (..), ReaderT (..))
import           Control.Monad.State       as X (MonadState)
import           Control.Monad.Writer      as X (MonadWriter)
import           Data.Aeson                as X
import           Data.Aeson.Types          as X (Pair, Parser, emptyObject,
                                                 parseEither, parseMaybe,
                                                 typeMismatch)
import           Data.Bifunctor            as X (first)
import           Data.Char                 as X (isNumber)
import           Data.Hashable             as X (Hashable)
import qualified Data.HashMap.Strict       as HM
import           Data.List                 as X (foldl', intercalate, nub)
import           Data.List.NonEmpty        as X (NonEmpty (..), toList)
import           Data.Maybe                as X (catMaybes, fromMaybe,
                                                 isNothing, maybeToList)
import           Data.Scientific           as X (Scientific)
import           Data.Semigroup            as X (Semigroup (..))
import           Data.Text                 as X (Text)
import           Data.Time.Calendar        as X (Day (..), showGregorian)
import           Data.Time.Clock           as X (NominalDiffTime, UTCTime)
import           Data.Time.Clock.POSIX     as X

import qualified Data.ByteString.Lazy      as BL
import qualified Data.Text                 as T
import qualified Data.Traversable          as DT
import qualified Data.Vector               as V
import qualified Network.HTTP.Types.Method as NHTM

type LByteString = BL.ByteString

type Method = NHTM.Method

readMay :: Read a => String -> Maybe a
readMay s = case reads s of
              (a, ""):_ -> Just a
              _         -> Nothing

parseReadText :: Read a => Text -> Parser a
parseReadText = maybe mzero return . readMay . T.unpack

showText :: Show a => a -> Text
showText = T.pack . show

omitNulls :: [(Text, Value)] -> Value
omitNulls = object . filter notNull where
  notNull (_, Null)    = False
  notNull (_, Array a) = (not . V.null) a
  notNull _            = True

parseNEJSON :: (FromJSON a) => [Value] -> Parser (NonEmpty a)
parseNEJSON []     = fail "Expected non-empty list"
parseNEJSON (x:xs) = DT.mapM parseJSON (x :| xs)

deleteSeveral :: (Eq k, Hashable k) => [k] -> HM.HashMap k v -> HM.HashMap k v
deleteSeveral ks hm = foldr HM.delete hm ks

oPath :: ToJSON a => NonEmpty Text -> a -> Value
oPath (k :| []) v   = object [k .= v]
oPath (k:| (h:t)) v = object [k .= oPath (h :| t) v]

tshow :: Show a => a -> Text
tshow = T.pack . show
