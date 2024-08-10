{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module TDP2 where

import Control.Monad
import Data.Char
import Data.Maybe
import Data.Typeable
import Test.QuickCheck
import Types2

class Rep a => Children a where
  children :: a -> [a]

instance Rep a => Children a where
  children x = case rep of
    (TIso iso tb) -> aux tb (to iso x)
    _ -> []
    where
      aux :: Type b -> b -> [a]
      aux (TEither t1 t2) (Left x) = undefined
      aux (TEither t1 t2) (Right x) = undefined
      aux (TProd t1 t2) (x1, x2) = undefined
      aux (TIso iso tb) x = undefined
      aux _ x = [] -- base types

data Dynamic = forall a. (Show a) => ToDyn a

fromDyn :: Typeable b => Dynamic -> Maybe b
fromDyn = undefined

instance Show Dynamic where
  show (ToDyn d) = "(ToDyn " ++ show d ++ ")"

-- a heterogenous list of values
dynlist = [ToDyn (0 :: Int), ToDyn True, ToDyn "ABC", ToDyn [1 :: Int, 2, 3], ToDyn (4 :: Int)]

-- increment the value if it happens to be a number
incr :: Dynamic -> Dynamic
incr d = undefined

deriving instance Typeable1 Tree

represent ''Bool
represent ''Maybe
represent ''[]

data RoseTree a = Node a [RoseTree a]
  deriving (Eq, Show, Typeable)

represent ''RoseTree

roseTree :: RoseTree Int
roseTree = (Node 1 [Node 2 [Node 5 [Node 6 [], Node 7 []]], Node 3 [], Node 4 []])

class Rep a => Universe a where
  universe :: a -> [a]

instance Rep a => Universe a where
  universe x = undefined
    where
      aux :: Type b -> b -> [a]
      aux (TEither t1 t2) (Left x) = aux t1 x
      aux (TEither t1 t2) (Right x) = aux t2 x
      aux (TProd t1 t2) (x1, x2) = aux t1 x1 ++ aux t2 x2
      aux (TIso iso tb) x = undefined
      aux _ x = []

example_universe = universe (N (N E 'a' E) 'b' (N E 'c' E))

example_u_rost =
  universe
    ( Node
        'a'
        [ Node 'b' [],
          Node 'c' [],
          Node 'd' [Node 'e' [], Node 'f' []]
        ]
    )

data Bop = Plus | Times | Minus | Div
  deriving (Eq, Show, Typeable)

represent ''Bop

data Expr
  = Var String
  | Val Int
  | Op Bop Expr Expr
  deriving (Eq, Show, Typeable)

represent ''Expr

vars :: Expr -> [String]
vars x = [s | Var s <- universe x]

example_vars =
  vars (Op Minus (Op Plus (Var "X") (Var "Y")) (Var "Z"))

lowerCaseVars :: Expr -> Bool
lowerCaseVars x = any (isLower . head) [v | (Var v) <- universe x]

countDivZero :: Expr -> Int
countDivZero x =
  length
    [ ()
      | Op Div _ (Val 0) <- universe x
    ]

class Rep a => Transform a where
  transform :: (a -> a) -> a -> a

instance Rep a => Transform a where
  transform f x =
    f
      ( case rep of
          (TIso iso tb) -> from iso (aux tb (to iso x))
          _ -> x
      )
    where
      aux :: Type b -> b -> b
      aux (TEither t1 t2) (Left x) = Left (aux t1 x)
      aux (TEither t1 t2) (Right x) = Right (aux t2 x)
      aux (TProd t1 t2) (x1, x2) = (aux t1 x1, aux t2 x2)
      aux (TIso iso tb) x = apply f (from iso (aux tb (to iso x)))
      aux _ x = x

apply :: (a -> a) -> b -> b
apply f x = undefined

constVal :: Expr -> Expr
constVal (Op Plus (Val i) (Val j)) = Val (i + j)
constVal (Op Times (Val i) (Val j)) = Val (i * j)
constVal (Op Minus (Val i) (Val j)) = Val (i - j)
constVal (Op Div (Val i) (Val j)) | j /= 0 = Val (i `div` j)
constVal e = e

t1 = transform constVal (Op Plus (Val 1) (Op Times (Val 2) (Val 3)))

t2 =
  transform
    constVal
    ( Op
        Plus
        (Var "x")
        (Op Times (Val 2) (Val 3))
    )

plusZero (Op Plus e (Val 0)) = e
plusZero (Op Plus (Val 0) e) = e
plusZero e = e

timesZero (Op Times e (Val 0)) = Val 0
timesZero (Op Times (Val 0) e) = Val 0
timesZero e = e

timesOne (Op Times e (Val 1)) = e
timesOne (Op Times (Val 1) e) = e
timesOne e = e

transformExpr =
  transform plusZero
    . transform timesZero
    . transform timesOne
    . transform constVal

t3 = transformExpr (Op Plus (Var "x") (Op Times (Var "y") (Val 0)))

data Statement
  = Assign String Expr
  | While Expr Statement
  | If Expr Statement Statement
  | Sequence Statement Statement
  | Skip
  deriving (Eq, Show, Typeable)

represent ''Statement

class Rep a => GTransform a where
  gtransform :: Typeable b => (b -> b) -> a -> a

instance Rep a => GTransform a where
  gtransform f x = apply f (aux rep x)
    where
      aux :: Type b -> b -> b
      aux (TEither t1 t2) (Left x1) = Left (aux t1 x1)
      aux (TEither t1 t2) (Right x2) = Right (aux t2 x2)
      aux (TProd t1 t2) (x1, x2) = (aux t1 x1, aux t2 x2)
      aux (TIso iso tb) x =
        apply f (from iso (aux tb (to iso x)))
      aux _ x = x

transformStmt :: Statement -> Statement
transformStmt = gtransform (plusZero . timesZero)

t4 =
  transformStmt
    ( Assign
        "x"
        (Op Plus (Var "x") (Op Times (Var "y") (Val 0)))
    )
