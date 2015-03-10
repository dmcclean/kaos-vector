{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Kaos.Vector

where

import Control.Applicative
import Data.Functor.Identity
import GHC.Generics
import Linear.Vector
import Text.LaTeX

class Canonical a where
	quality :: a -> Double
	quality = snd . normalize'
	normalize :: a -> a
	normalize = fst . normalize'
	normalize' :: a -> (a, Double)
	normalize' x = (normalize x, quality x)
	{-# MINIMAL (quality, normalize) | normalize' #-}

class (Functor (MonomorphicVector vec)) => Vector (vec :: (* -> *) -> * -> *) where
	data MonomorphicVector vec :: * -> *
	toMono :: vec Identity a -> MonomorphicVector vec a
	fromMono :: MonomorphicVector vec a -> vec Identity a

-- Metavariable names
data Metavariable = Metavariable { basic :: String, pretty :: LaTeX }

-- Empty Vectors
data EmptyVector (f :: * -> *) (a :: *) = EmptyVector { } -- this is a record consructor for consistency with signal vectors that do have fields, to help TH if we end up needing to reify
	deriving (Eq, Functor, Generic)

instance Vector EmptyVector where
	data MonomorphicVector EmptyVector a = EmptyVector'
		deriving (Eq, Functor, Generic)
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