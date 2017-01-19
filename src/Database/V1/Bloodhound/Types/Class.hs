{-# LANGUAGE CPP #-}
module Database.V1.Bloodhound.Types.Class
       ( Seminearring(..) )
       where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

class Monoid a => Seminearring a where
  -- 0, +, *
  (<||>) :: a -> a -> a
  (<&&>) :: a -> a -> a
  (<&&>) = mappend

infixr 5 <||>
infixr 5 <&&>
