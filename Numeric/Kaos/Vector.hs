{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Kaos.Vector

where

import Control.Applicative
import Data.Foldable
import Data.Functor.Identity
import Data.Traversable
import GHC.Generics
import Linear.Vector

class Canonical f where
  quality :: (Floating a) => f a -> a
  quality = snd . normalize'
  normalize :: (Floating a) => f a -> f a
  normalize = fst . normalize'
  normalize' :: (Floating a) => f a -> (f a, a)
  normalize' x = (normalize x, quality x)
  {-# MINIMAL (quality, normalize) | normalize' #-}

class (Traversable (MonomorphicVector vec)) => Vector (vec :: (* -> *) -> * -> *) where
  data MonomorphicVector vec :: * -> *
  type AtomicDimensions vec :: [*]
  toMono :: vec Identity a -> MonomorphicVector vec a
  fromMono :: MonomorphicVector vec a -> vec Identity a

-- Empty Vectors
data EmptyVector (f :: * -> *) (a :: *) = EmptyVector { } -- this is a record consructor for consistency with signal vectors that do have fields, to help TH if we end up needing to reify
  deriving (Eq, Functor, Generic)

instance Canonical (EmptyVector a) where
  quality = const 1
  normalize = id

instance Vector EmptyVector where
  data MonomorphicVector EmptyVector a = EmptyVector' {}
    deriving (Eq, Functor, Foldable, Traversable, Generic)
  type AtomicDimensions EmptyVector = '[]
  toMono = const EmptyVector'
  fromMono = const EmptyVector

instance Applicative (EmptyVector f) where
  pure = const EmptyVector
  _ <*> _ = EmptyVector

instance Additive (EmptyVector f) where
  zero = EmptyVector

instance Applicative (MonomorphicVector EmptyVector) where
  pure = const EmptyVector'
  _ <*> _ = EmptyVector'

instance Additive (MonomorphicVector EmptyVector) where
  zero = EmptyVector' 
