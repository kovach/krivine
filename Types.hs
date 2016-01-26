module Types where

data N = O | S N
  deriving (Eq, Show)
data E = R N | App E E | Lam E | Atom Int
  deriving (Eq, Show)
data Pair = Pair E [Pair]
  deriving (Eq, Show)

-- (Expression, Argument stack, Context)
type S = (E, [Pair], [Pair])

-- Util

size :: E -> Int
size (R _) = 1
size (App a b) = 1 + size a + size b
size (Lam e) = 1 + size e
size (Atom _) = 1
