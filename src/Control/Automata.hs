module Control.Automata (
       Automata(..),
       accepts, NFA, DFA,
       binop, intersection, union,
       complement, empty) where

import Control.Monad
import Control.Monad.Identity
import Data.Foldable          hiding (elem)
import Prelude                hiding (foldl, or)

-- | A generalized automaton datatype
data Automata q s m = A { states  :: [q]
                          -- ^ A (set) of states
                        , trans   :: (q, s) -> m q
                          -- ^ The transition function
                        , final   :: q -> Bool
                          -- ^ A predicate which is true on final states
                        , initial :: q
                          -- ^ The initial state
                        }

-- | Whether an automaton accepts a word
accepts :: (Monad m, Foldable m, Foldable t) =>
     Automata q s m -> t s -> Bool
accepts (A _ trans final initial) =
  or . liftM final . foldlM (curry trans) initial

-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
-- foldlM :: (Monad m, Foldable t) =>
--      (a -> b -> m a) -> a -> t b -> m a
-- or :: Foldable t => t Bool -> Bool

-- | Example types
type DFA q s = Automata q s Identity
type NFA q s = Automata q s []

-- | Binary operation on automata
-- Assuming sets of states are disjoint
binop :: Monad m =>
         (Bool -> Bool -> Bool) -> Automata q s m -> Automata q s m -> Automata (q,q) s m
binop op a1 a2 = A { states = [(q1, q2) | q1 <- states a1, q2 <- states a2]
                   , initial = (initial a1, initial a2)
                   , trans = \((q1, q2), s) -> do
                        q1' <- trans a1 (q1,s)
                        q2' <- trans a2 (q2,s)
                        return (q1', q2')
                   , final = \(q1, q2) ->
                      (final a1 q1) `op` (final a2 q2)
                   }

-- | Intersection of automata
intersection :: Monad m =>
  Automata q s m -> Automata q s m -> Automata (q,q) s m
intersection = binop (&&)

-- | Union of automata
union :: Monad m =>
  Automata q s m -> Automata q s m -> Automata (q,q) s m
union = binop (||)

-- | Complement of an automaton
complement :: Automata q s m -> Automata q s m
complement a = a { final = not . final a }

-- | Whether an automaton is empty
-- TODO: rewrite more elegantly?
-- We are requiring that all states in our automata are connected to the initial
empty :: Automata q s m -> Bool
empty a = not $ foldl (\acc q -> acc || final a q) False (states a)

-- | Some examples
dfa1 :: DFA Int Char
dfa1 = A { states = [0, 1]
         , initial = 0
         , final = flip elem [0]
         , trans = \x -> Identity $
             case x of
               (0,'b') -> 0
               (0,'a') -> 1
               (1,'a') -> 0
               (1,'b') -> 1
         }

dfa2 :: DFA Int Char
dfa2 = A { states = [2,3]
         , initial = 2
         , final = flip elem [2]
         , trans = \x -> Identity $
             case x of
               (2,'a') -> 2
               (2,'b') -> 3
               (3,'a') -> 3
               (3,'b') -> 2
         }

nfa1 :: NFA Int Char
nfa1 = A { states = [0,1]
         , trans = \x ->
            case x of
              (0, 'a') -> [0,1]
              (0, _) -> [0]
              (1, _) -> [1]
         , final = (==1)
         , initial = 0
         }

a2 :: Automata Int Char Maybe
a2 = A { states = [0,1]
       , trans = \x ->
          case x of
            (0, 'a') -> Just 1
            (1, 'a') -> Just 1
            (_, _) -> Nothing
       , final = (==1)
       , initial = 0
       }
