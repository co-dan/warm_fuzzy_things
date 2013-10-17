> module Origami where

Solutions and code for the great article/chapter
"Origami programming" by Jeremy Gibbons

> import Data.Maybe

----------------------------------------

* Folds for lists

> foldL :: (a -> b -> b) -> b -> [a] -> b
> foldL _ e []      = e
> foldL f e (x:xs) = f x (foldL f e xs)
 
Universal property of foldL:

  h = foldL f e
<=>
  h xs = case xs of
      []     -> e
      (y:ys) -> f y (h ys)

----------------------------------------

Excercise 3.1. Fusion law. Prove for strict h:

  h . foldL f e = foldL f' e'
<=
  (h (f a b) = f' a (h b)) and (h e = e')

Why does this law not hold for non-strict h?
--

Let's take a look at

g = h . foldL f e

g []       = (h . foldL f e) []
= h (foldL f e [])
= h e = e' = foldL f' e'

g (x::xs)  = (h . foldL f e) (x::xs)
= h (foldL f e (x::xs))
= h (f x (foldL f e xs))
= f' x (h (foldL f e xs))
 By induction
= f' x (foldL f' e' xs)
= foldL f' e' (x::xs)

Therefore g satisfies the condition for being foldL f e

----------------------------------------

Excercise 3.2. Define as instances of foldL equivalents of the
standard prelude functions map, ++ and concat.

> mapL :: (a -> b) -> [a] -> [b]
> mapL f = foldL ((:) . f) []

> appendL :: [a] -> [a] -> [a]
> appendL xs ys = foldL (:) ys xs
 
> concatL :: [[a]] -> [a]
> concatL = foldL appendL []

----------------------------------------

Excercise 3.3. As a corollary of the general fusion law, and using
your answer to Exercise 3.2, prove the map fusion law

  foldL f e . map g = foldL (f . g) e
--

foldL f e . map g = foldL f e . foldL ((:) . g) []

To prove the property using the fusion law we must prove
1) foldL f e [] = e
2) foldL f e (((:) . g) a b) = (f . g) a (foldL f e b)

1) Trivial
2) foldL f e ((:) . g) x xs)
= foldL f e ((g x):xs)
 By definition of the foldlL
= f (g x) (foldL f e xs)

QED

----------------------------------------

> isort :: Ord a => [a] -> [a]
> isort = foldL insert []
>   where
>     insert :: Ord a => a -> [a] -> [a]
>     insert x []     = [x]
>     insert x (y:ys) = case x > y of
>         True  -> y:(insert x ys)
>         False -> x:y:ys

----------------------------------------

Exercise 3.4 The fact that insert y (Cons x xs) sometimes requires xs
as well as insert y xs means that insert y is difficult to write as an
instance of foldL. However, the tupled function insert1 satisfying

    insert1 y xs = (xs, insert y xs)

can be written straightforwardly as a fold; show how.
--

> insert1 :: Ord a => a -> [a] -> ([a], [a])
> insert1 y = foldL f ([], [y])
>   where
>     f x (xs,acc) = (x:xs, inserted x xs acc)
>     inserted x xs acc = case y < x of
>         True  -> y:x:xs
>         False -> x:acc

----------------------------------------

We can define a left fold for lists in terms of `foldL`

> leftFold :: (b -> a -> b) -> b -> [a] -> b
> leftFold f z ls = foldL (\b g -> \x -> g (f x b)) id ls z

----------------------------------------

Paramorphisms

> paraL :: (a -> ([a], b) -> b) -> b -> [a] -> b
> paraL _ e []     = e
> paraL f e (x:xs) = f x (xs, paraL f e xs)

----------------------------------------

Excercise 3.5
Define insert as an instance of paraL

> insert2 :: Ord a => a -> [a] -> [a]
> insert2 y = paraL f [y]
>   where
>     f x (xs, inserted) = case y < x of
>         True  -> y:x:xs
>         False -> x:inserted

----------------------------------------

* Unfolds for lists

> unfoldL' :: (b -> Maybe (a,b)) -> b -> [a]
> unfoldL' f u = case f u of
>     Nothing    -> []
>     Just (a,b) -> a:(unfoldL' f b)


> unfoldL :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
> unfoldL p f g b = case p b of
>     False -> (f b):(unfoldL p f g (g b))
>     True  -> []

----------------------------------------

Excercise 3.6. Express unfoldL in terms of unfoldL', and vice versa.

> unfooldL :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
> unfooldL p f g b = unfoldL' (\b' ->
>     case p b of
>         True  -> Nothing
>         False -> Just (f b', g b')) b
                                   
> unfooldL' :: (b -> Maybe (a,b)) -> b -> [a]
> unfooldL' h b = unfooldL p f g b
>   where
>     p = isNothing . h
>     f = fst . fromJust . h
>     g = snd . fromJust . h
     
----------------------------------------
     
The universal property of unfoldL

  h = unfoldL p f g
<=>
  h b = if p b then [] else (f b):(h (g b))

----------------------------------------

Excercise 3.7. Using the universal property, prove the fusion law:

  unfoldL p f g . h = unfoldL p' f' g'
<=
  (p . h = p') and (f . h = f') and (g . h = h . g')
--

(unfoldL p f g . h) b = unfoldL p f g (h b)

p (h b) = p' b

Case 1: p' b = True. Then

unfoldL p f g (h b)
= []

Case 2: p' b = False. Then

unfoldL p f g (h b)
= (f (h b)):(unfoldL p f g (g (h b)))
= (f' b):(unfoldL p f g (h (g' b))

Therfore, universal property holds.

----------------------------------------


> foldL' :: (Maybe (a, b) -> b) -> [a] -> b
> foldL' f []     = f Nothing
> foldL' f (x:xs) = f (Just (x, foldL' f xs))

----------------------------------------

Excersie 3.8. Define foldL' in terms of foldL and vice versa.

TODO

Exercise 3.9. The adaptation of the single-argument fold and unfold to
the multi-argument interface is simplified by functions of the
following types:

> foldLargs :: (a -> b -> b) -> b -> (Maybe (a,b) -> b)
> unfoldLargs :: (b -> Bool) -> (b -> a) -> (b -> b) -> (b -> Maybe (a,b))

Implement those two functions.

> foldLargs _ b Nothing      = b
> foldLargs f _ (Just (x,y)) = f x y
 
> unfoldLargs p f g b
>     | p b       = Just (f b, g b)
>     | otherwise = Nothing

----------------------------------------


> delmin :: Ord a => [a] -> Maybe (a, [a])
> delmin [] = Nothing
> delmin xs = Just (y, deleteL y xs)
>   where y = minimumL xs
> 
> minimumL :: Ord b => [b] -> b
> minimumL (x:xs) = foldL min x xs
>
> ssort :: Ord a => [a] -> [a]
> ssort = unfoldL' delmin

Exercise 3.10. The case deleteL y (Cons x xs) requires both the tail
xs and the result deleteL y xs on that tail, so this function is
another paramorphism. Redefine deleteL using paraL.

> deleteL :: Eq b => b -> [b] -> [b]
> deleteL y = paraL f []
>   where
>     f x (xs, b) = if x == y
>                   then xs
>                   else x:b
>     

Exercise 3.11. In fact, delmin itself is a paramorphism. Redefine
delmin using paraL as the only form of recursion, taking care to
retain the order of the remainder of the list.

TODO

----------------------------------------

> bubble :: Ord a => [a] -> Maybe (a, [a])
> bubble = foldL step Nothing
>   where
>     step x Nothing        = Just (x, [])
>     step x (Just (y, ys))
>         | x < y     = Just (x, y:ys)
>         | otherwise = Just (y, x:ys)

----------------------------------------

Exercise 3.12 Of course, the type Maybe (a, [a]) is equivalent to
simply [a]. Use this fact to define bubble to return a list
instead, with the minimum element bubbled to the top; one would
deconstruct this list as a final step. This definition might seem more
natural to the imperative programmer handicapped by an impoverished
type system. 􏱊

> bubble' :: Ord a => [a] -> [a]
> bubble' = maybe [] (uncurry (:)) . bubble

----------------------------------------

> bsort :: Ord a => [a] -> [a]
> bsort = unfoldL' bubble

----------------------------------------

Exercise 3.13 In fact, the function insert above can be defined as an
unfold. The idea is that the ‘state’ of the unfold consists of a pair:
the list into which to insert, and Maybe an element to be inserted.
Initially there is an element to insert; once it has been inserted,
the remainder of the list is merely copied. Complete the definition,
using unfoldL′.


     insert :: Ord a => a -> [a] -> [a]
     insert x []     = [x]
     insert x (y:ys) = case x > y of
         True  -> y:(insert x ys)
         False -> x:y:ys

> insert'' :: Ord a => a -> [a] -> [a]
> insert'' y xs = unfoldL' step (xs, Just y)
>   where
>     step ([],   Nothing) = Nothing
>     step (x:xs, Nothing) = Just (x, (xs, Nothing))
>     step ([],   Just y)  = Just (y, ([], Nothing))
>     step (x:xs, Just y)
>         | y > x     = Just (x, (xs,   Just y))
>         | otherwise = Just (y, (x:xs, Nothing))

----------------------------------------

Exercise 3.14. The characterisation of insert as an unfold is a bit
unsatisfactory, because once the correct position is found at which
to insert the element, the remainder of the list must still be copied
item by item. The directly recursive definition did not have this
problem: one branch shares the remainder of the original list without
making a recursive call. This general pattern can be captured as
another recursion operator, known as an apomorphism

> apoL' :: (b -> Maybe (a, Either b [a])) -> b -> [a]
> apoL' f u = case f u of
>     Nothing            -> []
>     Just (x, Left v)   -> x:(apoL' f v)
>     Just (x, Right xs) -> x:xs

Define insert as an instance of apoL'
--

> insert :: Ord a => a -> [a] -> [a]
> insert y xs = apoL' step (xs, Just y)
>   where
>     step ([],   Nothing) = Nothing
>     step (x:xs, Nothing) = Just (x, Right xs)
>     step ([],   Just y)  = Just (y, Right [])
>     step (x:xs, Just y)
>         | y > x     = Just (x, Left (xs,   Just y))
>         | otherwise = Just (y, Left (x:xs, Nothing))

----------------------------------------

* Hylomorphisms

> hyloL f e p g h = foldL f e . unfoldL p g h

> fact = hyloL (*) 1 (==0) id pred


----------------------------------------

* Folds for naturals

> data Nat = Zero | Succ Nat
>          deriving (Show)

> one = Succ Zero
> two = Succ one
> three = Succ two

> foldN :: a -> (a -> a) -> Nat -> a
> foldN z s Zero     = z
> foldN z s (Succ n) = s (foldN z s n)

iter n f x = foldN x f n
----------------------------------------

Exercise 3.16 What is the single-argument version foldN' of foldN?
Express each in terms of the other.

TODO

Exercise 3.17 What is the universal property of foldN , and what is
the fusion law?

  h = foldN z s
<=>
  h n = case n of
      Zero     -> z
      (Succ m) -> s (h m)

----------------------------------------

Exercise 3.18 Express addN , mulN and powN — binary operators for
addition, multiplication and exponentiation on Nat — using foldN

> addN :: Nat -> Nat -> Nat
> addN n = foldN n Succ
 
> multN :: Nat -> Nat -> Nat
> multN n = foldN Zero (addN n)

> powN :: Nat -> Nat -> Nat
> powN n = foldN one (multN n)
>   where one = Succ Zero

----------------------------------------

Exercise 3.19 The function predN :: Nat -> Maybe Nat, which returns
the predecessor of a number (or Nothing as the predecessor of Zero) is
easily expressed by pattern matching:

> predN :: Nat -> Maybe Nat
> predN = foldN Nothing step
>   where
>     step Nothing  = Just Zero
>     step (Just n) = Just (Succ n)

----------------------------------------

Exercise 3.20.

> subN :: Nat -> Nat -> Maybe Nat
> subN n = foldN (Just n) ((=<<) predN)

> eqN :: Nat -> Nat -> Bool
> eqN n m =  isJust (subN n m)
>         && isJust (subN m n)
 
----------------------------------------

* Unfolds for naturals

> unfoldN' :: (a -> Maybe a) -> a -> Nat
> unfoldN' f x = case f x of
>     Nothing -> Zero
>     Just  y -> Succ (unfoldN' f y)

> unfoldN :: (a -> Bool) -> (a -> a) -> a -> Nat
> unfoldN p g x = if p x then Zero else Succ (unfoldN p g (g x))
