{-# LANGUAGE TypeSynonymInstances #-}
module Data.Stream (Stream(..), h, t) where

import Control.Applicative
import Control.Arrow
import Control.Monad       (liftM)

-- | The Stream datatype
newtype Stream a = Stream { unS :: [a] }
                 deriving (Eq,Show)

-- | The head of the stream
h :: Stream a -> a
h = head . unS

-- | The tail of the stream
t :: Stream a -> Stream a
t = Stream . tail . unS

-- | Unfold laws:
-- unfold (h &&& t) = id
-- h . unfold (f &&& g) = f
-- t . unfold (f &&& g) = unfold (f &&& g) . g
-- Fusion law: f . h = (id *** h) . g ==> t . unfold (f &&& g)
unfold :: (b -> (a,b)) -> b -> Stream a
unfold f b = Stream $ a:unS (unfold f b')
  where (a,b') = f b

instance Functor Stream where
  fmap f = unfold ((f . h) &&& t)

instance Applicative Stream where
  pure = Stream . repeat
  fs <*> ss = Stream $ (f s):unS (fs' <*> ss')
    where f = h fs
          s = h ss
          fs' = t fs
          ss' = t ss

join :: Stream (Stream a) -> Stream a
join = unfold hhtt
  where hhtt = hh &&& tt
        hh   = h . h
        tt   = fmap t . t

instance Monad Stream where
  return = pure
  m >>= f = join (fmap f m)
