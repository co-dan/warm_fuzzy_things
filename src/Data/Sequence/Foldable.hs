{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Sequence.Foldable where

import           Prelude               hiding (drop, take)

import           Data.Functor.Foldable
import           Data.Sequence         (Seq, ViewL (..), (<|))
import qualified Data.Sequence         as S


data instance Prim (Seq a) b = EmptyP | ViewP a b

instance Functor (Prim (Seq a)) where
    fmap _ EmptyP = EmptyP
    fmap f (ViewP a b) = ViewP a (f b)

type instance Base (Seq a) = Prim (Seq a)

viewlToPrim :: ViewL a -> Prim (Seq a) (Seq a)
viewlToPrim EmptyL = EmptyP
viewlToPrim (a :< as) = ViewP a as

instance Foldable (Seq a) where
    project = viewlToPrim . S.viewl

instance Unfoldable (Seq a) where
    embed EmptyP = S.empty
    embed (ViewP a b) = a <| b
