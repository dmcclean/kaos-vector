{-# LANGUAGE RankNTypes #-}

module Numeric.Kaos.SimpleDiff
where

data SimpleDerivative = Constant Double
                      | ConstantDerivative Double
                      | Variable

knownDerivative :: SimpleDerivative -> Maybe Double
knownDerivative (Constant _) = Just 0
knownDerivative (ConstantDerivative x) = Just x
knownDerivative Variable = Nothing

approximateJacobian :: (forall a.Floating a=>[a]->[a]) -> Integer -> [[Maybe Double]]
approximateJacobian f n = fmap (fmap knownDerivative . f) ident
  where
  	kronecker i j = if i == j then (ConstantDerivative 1) else (Constant 0)
  	ident = [[kronecker i j | i <- [1..n]] | j <- [1..n]]

instance Num SimpleDerivative where
  (Constant a) + (Constant b) = Constant (a + b)
  (Constant _) + b = b
  a + (Constant _) = a
  _ + _ = Variable
  a - b = a + (negate b)
  (Constant a) * (Constant b) = Constant (a * b)
  (Constant 0) * b = Constant 0
  a * (Constant 0) = Constant 0
  (Constant a) * (ConstantDerivative b) = ConstantDerivative (a * b)
  (ConstantDerivative a) * (Constant b) = ConstantDerivative (a * b)
  _ * _ = Variable
  negate (Constant a) = Constant (negate a)
  negate (ConstantDerivative a) = ConstantDerivative (negate a)
  negate _ = Variable
  fromInteger = Constant . fromInteger
  abs = abstractFun abs
  signum = abstractFun signum

abstractFun :: (Double -> Double) -> SimpleDerivative -> SimpleDerivative
abstractFun f (Constant x) = Constant (f x)
abstractFun f (ConstantDerivative 0) = ConstantDerivative 0
abstractFun _ _ = Variable

instance Fractional SimpleDerivative where
  recip (Constant a) = Constant (recip a)
  recip (ConstantDerivative a) = ConstantDerivative (recip a)
  recip _ = Variable
  fromRational = Constant . fromRational

instance Floating SimpleDerivative where
  pi = Constant pi
  exp = abstractFun exp
  sqrt = abstractFun sqrt
  log = abstractFun log
  (Constant a) ** (Constant b) = Constant (a ** b)
  (ConstantDerivative 0) ** (ConstantDerivative 0) = ConstantDerivative 0
  _ ** (Constant 0) = Constant 1
  (ConstantDerivative a) ** (Constant 1) = ConstantDerivative a
  (ConstantDerivative a) ** (Constant (-1)) = ConstantDerivative (negate a)
  _ ** _ = Variable
  logBase (Constant a) (Constant b) = Constant (logBase a b)
  logBase (ConstantDerivative 0) (ConstantDerivative 0) = ConstantDerivative 0
  logBase _ _ = Variable
  sin = abstractFun sin
  tan = abstractFun tan
  cos = abstractFun cos
  asin = abstractFun asin
  atan = abstractFun atan
  acos = abstractFun acos
  sinh = abstractFun sinh
  tanh = abstractFun tanh
  cosh = abstractFun cosh
  asinh = abstractFun asinh
  atanh = abstractFun atanh
  acosh = abstractFun acosh
