{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Numeric.Kaos.UnitVectors
where

import Control.Applicative
import Linear.Vector
import Linear.Metric
import Linear.Quaternion
import Numeric.Kaos.Vector

newtype Unit f a = Unit (f a)
  deriving (Eq, Ord, Show, Functor, Additive, Metric, Applicative, Monad)

instance (Metric f) => Canonical (Unit f) where
	quality (Unit v) = recip . norm $ v
	normalize (Unit v) = Unit . fmap (/ n) $ v
	  where
	  	n = norm v

type UnitQuaternion = Unit Quaternion