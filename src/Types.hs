{-# LANGUAGE GADTs #-}

module Types (Type (..), Rep (..), Iso (..)) where

import Control.Monad
import Data.List (foldl')
import Data.Typeable
import qualified Language.Haskell.TH as TH

-- A simple representation of type structure, plus TH code to
-- generate this representation automatically.

data Type a where
  TInt :: Type Int
  TChar :: Type Char
  TUnit :: Type ()
  TProd :: Type a -> Type b -> Type (a, b)
  TEither :: Type a -> Type b -> Type (Either a b)
  TIso :: Iso a b -> Type b -> Type a

-- two types are isomorphic when there are functions to
-- take the elements from one to the other. Furthermore
-- we should have both to . from == id  and from . to == id

data Iso a b = Iso
  { to :: a -> b,
    from :: b -> a
  }

-- The class of representable types

class Rep a where
  rep :: Type a

instance Rep Int where rep = TInt

instance Rep Char where rep = TChar

instance Rep () where rep = TUnit

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
