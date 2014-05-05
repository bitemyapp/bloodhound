module Database.Bloodhound.Types.Class
       ( Seminearring(..) )
       where

import Data.Monoid

class Monoid a => Seminearring a where
  -- 0, +, *
  (<||>) :: a -> a -> a
  (<&&>) :: a -> a -> a
  (<&&>) = mappend

infixr 5 <||>
infixr 5 <&&>
