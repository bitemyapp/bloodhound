{-# LANGUAGE OverloadedStrings #-}

module Test.Import
  ( module X
  , module Test.Import
  ) where


import Control.Applicative as X
import Control.Exception as X (evaluate)
import Control.Monad as X
import Control.Monad.Catch as X
import Control.Monad.Reader as X
import Data.Aeson as X
import Data.Aeson.TH as X
import Data.Aeson.Types as X (parseEither)
import Data.Maybe as X
import Data.List.NonEmpty as X (NonEmpty(..))
import Data.Monoid as X
import Data.Ord as X (comparing)
import Data.Proxy as X
import Data.Text as X (Text)
import Data.Time.Calendar as X (Day(..), fromGregorian)
import Data.Time.Clock as X
import Data.Typeable as X
import Database.V5.Bloodhound as X hiding (key)
import Lens.Micro as X
import Lens.Micro.Aeson as X
import Network.HTTP.Client as X hiding (Proxy, fileSize)
import System.IO.Temp as X
import System.PosixCompat.Files as X
import Test.Hspec as X
import Test.Hspec.QuickCheck as X (prop)
import Test.QuickCheck as X hiding (Result, Success)
import Test.QuickCheck.Property.Monoid as X (T (..), eq, prop_Monoid)
import Text.Pretty.Simple as X (pPrint)

import qualified Data.List as L

noDuplicates :: Eq a => [a] -> Bool
noDuplicates xs = L.nub xs == xs

getSource :: EsResult a -> Maybe a
getSource = fmap _source . foundResult

grabFirst :: Either EsError (SearchResult a) -> Either EsError a
grabFirst r =
  case fmap (hitSource . head . hits . searchHits) r of
    (Left e)         -> Left e
    (Right Nothing)  -> Left (EsError 500 "Source was missing")
    (Right (Just x)) -> Right x

when' :: Monad m => m Bool -> m () -> m ()
when' b f = b >>= \x -> when x f

headMay :: [a] -> Maybe a
headMay (x : _) = Just x
headMay _ = Nothing
