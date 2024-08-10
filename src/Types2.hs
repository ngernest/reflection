{-# LANGUAGE GADTs #-}

module Types2 (Type (..), Rep (..), Iso (..)) where

import Control.Monad
import Data.List (foldl')
import Data.Typeable

-- A simple representation of type structure

-- This version requires that isomorphic types be instances of
-- the Typeable class.

data Type a where
  TBase :: Typeable a => Type a
  TProd :: Type a -> Type b -> Type (a, b)
  TEither :: Type a -> Type b -> Type (Either a b)
  TIso :: Typeable a => Iso a b -> Type b -> Type a

-- two types are isomorphic when there are functions to
-- take the elements from one to the other. Furthermore
-- we should have both to . from == id  and from . to == id

data Iso a b = Iso
  { to :: a -> b,
    from :: b -> a
  }

-- The class of representable types, must also be Typeable.

class Typeable a => Rep a where
  rep :: Type a

instance Rep Int where rep = TBase

instance Rep Char where rep = TBase

instance Rep () where rep = TBase

instance
  (Rep a, Rep b) =>
  Rep (Either a b)
  where
  rep = TEither rep rep

instance
  (Rep a, Rep b) =>
  Rep (a, b)
  where
  rep = TProd rep rep
