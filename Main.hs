module Main where

import Prelude hiding (id)
import Data.List (find)
import Control.Arrow (first, second)

import Types

kr :: S -> S
-- OPTIMIZE --
--kr (Lam b, (Pair (R       O)           (v : _)) : xs, c) = (b, xs, v : c)
--kr (Lam b, (Pair (R    (S O))      (_ : v : _)) : xs, c) = (b, xs, v : c)
kr (Lam b, (Pair (R (S (S O))) (_ : _ : v : _)) : xs, c) = (b, xs, v : c)
-- standard machine --
kr (Lam b, (v : xs), c) = (b, xs, v : c)
kr (App f x, xs, c) = (f, Pair x c : xs, c)
kr (R O, s, (Pair v c : _)) = (v, s, c)
kr (R (S n), s, _ : c) = (R n, s, c)
kr s@(Lam b, _, _) = s
-- Symbols -- 
kr s@(Atom _, _, _) = s

kr (x, _, _) = error $ show x

reduce :: Int -> S -> (Int, S)
reduce n s =
  let s' = kr s
  in if s' == s then (n, s) else reduce (n+1) s'

term :: E -> S
term e = (e, [], [])

match x (y, _, _) | x == y = True
match _ _ = False

x = R O
y = R (S O)

zero = Lam $ Lam $ y
suc = Lam $ Lam $ Lam $ (App (R O) (R (S (S O))))

n 0 = zero
n k = App suc (n (k-1))

compose f g = Lam (App f (App g x))

apps [] = id
apps (n : ns) = Lam (App (App x n) (apps ns))

numberify :: S -> Int
numberify (t, s, c) =
  case reduce 0 (App (App t (Atom 0)) id, s, c) of
    (_, (Atom i, _, _)) -> i
    (_, s') -> 1 + numberify s'

testn n = apps $ map Atom [0..n]

dotest i = App (testn i) (n i)

id = Lam x
sap = Lam (App x x)
mu = App sap sap

t1 = term $ App id id
t2 = term $ mu

--chk = (\(a, _, _) -> a) . reduce . term
chk t =
  let (n, (a, _, _)) = reduce 0 $ term t
  in (n, a)

asdf = map (\i -> chk $ App ((iterate (compose id) id) !! i) id) [0..9]
