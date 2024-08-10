{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Gen where

import Data.Set (Set)
import qualified Data.Set as S
import Test.QuickCheck

data Ty t where
  TyInt :: Ty Int
  TyBool :: Ty Bool
  TySet :: Ty (Set Int) -- monomorphic sets

data Expr t where
  Is_Empty :: Expr (Set Int) -> Expr Bool
  Lit :: Show a => a -> Expr a
  Empty :: Expr (Set Int)
  Insert :: Expr Int -> Expr (Set Int) -> Expr (Set Int)
  Member :: Expr Int -> Expr (Set Int) -> Expr Bool

interp :: Expr t -> t
interp e = case e of
  (Is_Empty s) -> S.null (interp s)
  (Lit c) -> c
  Empty -> S.empty
  Insert e s -> S.insert (interp e) (interp s)
  Member e s -> S.member (interp e) (interp s)

-- GADTs require standalone deriving
deriving instance Show (Ty t)

deriving instance Show (Expr t)

genTy :: Gen (Some Ty)
genTy = elements [Exists TyInt, Exists TyBool, Exists TySet]

genExpr :: Ty t -> Gen (Expr t)
genExpr t = case t of
  TyInt -> Lit <$> arbitrary
  TyBool ->
    oneof
      [ Is_Empty <$> genExpr TySet,
        Member <$> genExpr TyInt <*> genExpr TySet
      ]
  TySet -> oneof [return Empty, Insert <$> genExpr TyInt <*> genExpr TySet]

-- an existential type
data Some f where
  Exists :: Show (f t) => f t -> Some f

deriving instance Show (Some f)

instance Arbitrary (Some Expr) where
  arbitrary :: Gen (Some Expr)
  arbitrary = do
    st <- genTy
    case st of
      Exists t -> do
        e <- genExpr t
        return (Exists e)