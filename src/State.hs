{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module State (State, get, put, state, runState, evalState, execState) where

import Control.Monad (ap, liftM)

newtype State s a = S {runState :: s -> (a, s)}

state :: (s -> (a, s)) -> State s a
state = S

instance Monad (State s) where
  return :: a -> State s a
  return x = S (\s -> (x, s))

  (>>=) :: State s a -> (a -> State s b) -> State s b
  st >>= f =
    S
      ( \s ->
          let (x, s') = runState st s
           in runState (f x) s'
      )

instance Applicative (State s) where
  pure :: a -> State s a
  pure = return

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) = ap

instance Functor (State s) where
  fmap = liftM

evalState :: State s a -> s -> a
evalState s = fst . runState s

execState :: State s a -> s -> s
execState s = snd . runState s

get :: State s s
get = S (\s -> (s, s))

put :: s -> State s ()
put s' = S (const ((), s'))
