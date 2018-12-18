{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.ApproxEq where

import Database.V5.Bloodhound

import Test.Import

import qualified Data.List.NonEmpty as NE

-- | Typeclass for "equal where it matters". Use this to specify
-- less-strict equivalence for things such as lists that can wind up
-- in an unpredictable order
class ApproxEq a where
  (=~) :: a -> a -> Bool

  showApproxEq :: a -> String
  default showApproxEq :: (Show a) => a -> String
  showApproxEq = show

(==~) :: (ApproxEq a) => a -> a -> Property
a ==~ b = counterexample (showApproxEq a ++ " !=~ " ++ showApproxEq b) (a =~ b)

instance ApproxEq NominalDiffTime where (=~) = (==)
instance ApproxEq Bool where (=~) = (==)
instance ApproxEq Int where (=~) = (==)
instance (Eq a, Show a) => ApproxEq (Maybe a) where (=~) = (==)
instance ApproxEq Char where
  (=~) = (==)

instance ApproxEq NodeAttrFilter where (=~) = (==)
instance ApproxEq NodeAttrName where (=~) = (==)
instance (Eq a, Show a) => ApproxEq (NonEmpty a) where (=~) = (==)
instance (ApproxEq l, Show l, ApproxEq r, Show r) => ApproxEq (Either l r) where
  Left a =~ Left b = a =~ b
  Right a =~ Right b = a =~ b
  _ =~ _ = False
  showApproxEq (Left x)  = "Left " <> showApproxEq x
  showApproxEq (Right x) = "Right " <> showApproxEq x
instance (ApproxEq a, Show a) => ApproxEq [a] where
  as =~ bs = and (zipWith (=~) as bs)
instance ApproxEq ReplicaCount where (=~) = (==)
instance ApproxEq ReplicaBounds where (=~) = (==)
instance ApproxEq Bytes where (=~) = (==)
instance ApproxEq AllocationPolicy where (=~) = (==)
instance ApproxEq InitialShardCount where (=~) = (==)
instance ApproxEq FSType where (=~) = (==)

-- | Due to the way nodeattrfilters get serialized here, they may come
-- out in a different order, but they are morally equivalent
instance ApproxEq UpdatableIndexSetting where
  RoutingAllocationInclude a =~ RoutingAllocationInclude b =
    NE.sort a =~ NE.sort b
  RoutingAllocationExclude a =~ RoutingAllocationExclude b =
    NE.sort a =~ NE.sort b
  RoutingAllocationRequire a =~ RoutingAllocationRequire b =
    NE.sort a =~ NE.sort b
  a =~ b = a == b
  showApproxEq (RoutingAllocationInclude xs) = show (RoutingAllocationInclude (NE.sort xs))
  showApproxEq (RoutingAllocationExclude xs) = show (RoutingAllocationExclude (NE.sort xs))
  showApproxEq (RoutingAllocationRequire xs) = show (RoutingAllocationRequire (NE.sort xs))
  showApproxEq x = show x
