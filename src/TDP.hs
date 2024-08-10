{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}

module TDP where

import Control.Monad (liftM)
import Data.Bits
import Data.Typeable
import State
import Test.QuickCheck
import Types (Iso (..), Rep (..), Type (..))

-- | our own version of structural equality
class Teq a where
  eq :: a -> a -> Bool

x :: Type (Int, Char)
x = TProd TInt TChar

-- | `from` is the left inverse of `to`
prop_Iso1 :: Eq a => Iso a b -> a -> Bool
prop_Iso1 iso x = from iso (to iso x) == x

-- | `to` is the left inverse of `from`
prop_Iso2 :: Eq b => Iso a b -> b -> Bool
prop_Iso2 iso x = to iso (from iso x) == x

-- | An isomorphism between `Bool` and `Either () ()`
boolIso :: Iso Bool (Either () ())
boolIso =
  Iso
    { to = \b -> if b then Right () else Left (),
      from = from_either
    }
  where
    from_either :: Either () () -> Bool
    from_either (Left ()) = False
    from_either (Right ()) = True

-- | The isomorphism above allows us to represent the structure of `Bool`!
tBool :: Type Bool
tBool = TIso boolIso (TEither TUnit TUnit)

-- | A type-directed equality function
teq :: Type a -> a -> a -> Bool
teq TInt i1 i2 = i1 == i2
teq TChar c1 c2 = c1 == c2
teq TUnit () () = True
teq (TProd tya tyb) (a1, b1) (a2, b2) = teq tya a1 a2 && teq tyb b1 b2
teq (TEither tya tyb) (Left a1) (Left a2) = teq tya a1 a2
teq (TEither tya tyb) (Right b1) (Right b2) = teq tyb b1 b2
teq (TEither _ _) _ _ = False
teq (TIso iso tyb) a1 a2 = teq tyb (to iso a1) (to iso a2)

example :: Bool
example =
  teq tBool True False
    && teq
      (TProd TInt (TProd TChar tBool))
      (3, ('a', True))
      (4, ('a', True))

-- >>> example
-- False

instance Rep Bool where rep = tBool

-- | We turn all representable types into equality types
-- by supplying the representation to the type-directed equality function
instance (Rep a) => Teq a where
  eq :: a -> a -> Bool
  eq a1 a2 = teq rep a1 a2

example_fixed :: Bool
example_fixed =
  eq True False
    && eq (3 :: Int, ('a', True)) (4, ('a', True))

-- | `Maybe a` is isomorphic to `Either () a`
maybeIso :: Iso (Maybe a) (Either () a)
maybeIso = Iso t f
  where
    t :: Maybe a -> Either () a
    t Nothing = Left ()
    t (Just a) = Right a

    f :: Either () a -> Maybe a
    f (Left ()) = Nothing
    f (Right a) = Just a

-- | A representation of `Maybe a`
tMaybe :: Rep a => Type (Maybe a)
tMaybe = TIso maybeIso rep

instance Rep a => Rep (Maybe a) where
  rep :: Type (Maybe a)
  rep = tMaybe

example_maybe :: Bool
example_maybe =
  eq (Just True) (Just True)

data A = B | C Char | D Int Bool Char

-- | The type `A` is isomorphic to `(() + Char) + (Int, (Bool, Char))`
aIso :: Iso A (Either (Either () Char) (Int, (Bool, Char)))
aIso = Iso t f
  where
    t B = Left (Left ())
    t (C c) = Left (Right c)
    t (D i b c) = Right (i, (b, c))

    f (Left (Left ())) = B
    f (Left (Right c)) = C c
    f (Right (i, (b, c))) = D i b c

aType :: Type A
aType = TIso aIso rep

instance Rep a => Rep [a] where
  rep :: Rep a => Type [a]
  rep = tList

tList :: Rep a => Type [a]
tList = TIso listIso rep

-- | An isomorphicm between `[a]` & `Either () (a, [a])`
listIso :: Rep a => Iso [a] (Either () (a, [a]))
listIso = Iso t f
  where
    t :: [a] -> Either () (a, [a])
    t [] = Left ()
    t (x : xs) = Right (x, xs)

    f :: Either () (a, [a]) -> [a]
    f (Left ()) = []
    f (Right (x, xs)) = x : xs

data Bit = O | I deriving (Eq, Show)

class Rep a => Marshall a where
  marshall :: a -> [Bit]
  unmarshall :: [Bit] -> a

prop_m1 :: AnyGType -> Property
prop_m1 (AnyG (t1 :: Type a)) = forAll arbitrary $ \x ->
  tunmarshall t1 (tmarshall t1 x) == x

intSize :: Int
intSize = finiteBitSize (0 :: Int)

serializeInt :: Int -> Int -> [Bit]
serializeInt 0 x = []
serializeInt n x =
  (if even x then O else I) :
  serializeInt (n - 1) (x `div` 2)

marshallInt :: Int -> Int -> [Bit] -> [Bit]
marshallInt n x bs = serializeInt n x ++ bs

unmarshallInt :: Int -> State [Bit] Int
unmarshallInt 0 = return 0
unmarshallInt n = do
  bs' <- get
  case bs' of
    (O : bs) ->
      put bs >> fmap (* 2) (unmarshallInt (n - 1))
    (I : bs) ->
      put bs >> unmarshallInt (n - 1) >>= \x -> return (x * 2 + 1)
    [] -> error "read error"

prop_Int :: Int -> Bool
prop_Int i = j == i
  where
    (j, _) =
      runState
        (unmarshallInt intSize)
        (marshallInt intSize i [])

instance Rep a => Marshall a where
  marshall :: a -> [Bit]
  marshall = tmarshall rep

  unmarshall :: [Bit] -> a
  unmarshall = tunmarshall rep

tmarshall :: Type a -> a -> [Bit]
tmarshall t x = aux t x []
  where
    aux :: Type a -> a -> [Bit] -> [Bit]
    aux = undefined

tunmarshall :: Type a -> [Bit] -> a
tunmarshall t bs = evalState (aux t) bs
  where
    aux :: Type a -> State [Bit] a
    aux = undefined

data AnyType = forall a. Any (Type a)

data AnyGType
  = forall a.
    (Show a, Eq a, Arbitrary a) =>
    AnyG (Type a)

instance Show AnyGType where
  show (AnyG t) = show (Any t)

instance Show AnyType where
  show (Any TInt) = "TInt"
  show (Any TChar) = "TChar"
  show (Any TUnit) = "TUnit"
  show (Any (TEither t1 t2)) =
    "(TEither " ++ show (Any t1) ++ " "
      ++ show (Any t2)
      ++ ")"
  show (Any (TProd t1 t2)) =
    "(TProd " ++ show (Any t1) ++ " "
      ++ show (Any t2)
      ++ ")"
  show _ = undefined

instance Arbitrary Bit where
  arbitrary = elements [O, I]

instance Arbitrary AnyGType where
  arbitrary = sized anyn
    where
      base = elements [AnyG TInt, AnyG TChar, AnyG TUnit]
      anyn 0 = base
      anyn n =
        oneof
          [ base,
            do
              a1 <- anyn (n `div` 2)
              a2 <- anyn (n `div` 2)
              case (a1, a2) of
                (AnyG t1, AnyG t2) ->
                  return $ AnyG (TProd t1 t2),
            do
              a1 <- anyn (n `div` 2)
              a2 <- anyn (n `div` 2)
              case (a1, a2) of
                (AnyG t1, AnyG t2) ->
                  return $ AnyG (TEither t1 t2)
          ]
