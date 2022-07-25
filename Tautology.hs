module Tautology (module Tautology) where

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equiv Prop Prop
        deriving Show

p1, p2, p3, p4 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 = Imply (And (Var 'A') (Var 'B')) (Var 'B')
p4 = Imply (And (Var 'A') (Imply  (Var 'A') (Var 'B'))) (Var 'B')

type Assoc a b = [(a, b)]
type Subst = Assoc Char Bool

remdups :: Eq a => [a] -> [a]
remdups (x:xs) = x : remdups (filter (/=x) xs)
remdups [] = []

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var v) = find v s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Imply p q) = eval s p >= eval s q
eval s (Equiv p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var c) = [c]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (True : ) b ++ map (False : ) b
  where
    b = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip chars) (bools (length chars))
  where
    chars = remdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
