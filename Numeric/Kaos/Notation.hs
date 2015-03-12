{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Numeric.Kaos.Notation

where

import Control.Monad.Trans.Reader
import Data.Matrix
import Text.LaTeX
import Text.LaTeX.Base.Class (LaTeXC, fromLaTeX, comm0)
import Text.LaTeX.Packages.AMSMath

-- Metavariable names
data Metavariable = Metavariable { basic :: String, pretty :: LaTeX, independent :: Bool }
                  | Estimate Metavariable
                  | Indexed Metavariable Metavariable
                  | Derivative Int Metavariable Metavariable
                  | Measurement Metavariable

metavariable :: String -> Metavariable
metavariable n = Metavariable { basic = n, pretty = fromString n, independent = False}

metavariable' :: String -> LaTeX -> Metavariable
metavariable' n l = Metavariable { basic = n, pretty = l, independent = False }

independentMetavariable :: String -> Metavariable
independentMetavariable n = Metavariable { basic = n, pretty = fromString n, independent = True}

instance Texy Metavariable where
  texy = fromLaTeX . formatMetavariable wikipediaNotation

formatMetavariable :: NotationalConvention -> Metavariable -> LaTeX
formatMetavariable _ (Metavariable {pretty}) = pretty
formatMetavariable c (Estimate x) = estimateStyle c x'
  where
    x' = formatMetavariable c x
formatMetavariable c (Indexed sub x) = indexedStyle c sub' x'
  where
    sub' = formatMetavariable c sub
    x' = formatMetavariable c x
formatMetavariable c (Derivative n x y) = derivativeStyle c ind n x' y'
  where
    ind = independent y
    x' = formatMetavariable c x
    y' = formatMetavariable c y
formatMetavariable c (Measurement x) = measurementStyle c x'
  where
    x' = formatMetavariable c x

dottedDerivative :: Int -> LaTeX -> LaTeX -> LaTeX
dottedDerivative 1 x _ = dot x
dottedDerivative 2 x _ = ddot x
dottedDerivative 3 x _ = dddot x
dottedDerivative n x y = defaultDerivative n x y

defaultDerivative :: Int -> LaTeX -> LaTeX -> LaTeX
defaultDerivative 0 x _ = x
defaultDerivative 1 x y = fromLaTeX $ ("d" <> x) / ("d" <> y)
defaultDerivative n x y = fromLaTeX $ ("d" ** n' <> x) / (("d" <> y) ** n')
  where
    n' = texy n

type MathExpr = Reader NotationalConvention LaTeX

formatMathExpr :: NotationalConvention -> MathExpr -> LaTeX
formatMathExpr = flip runReader

var :: Metavariable -> MathExpr
var x = do
          c <- ask
          return $ formatMetavariable c x

estimate :: MathExpr -> MathExpr
estimate = lift1 estimateStyle

indexed :: MathExpr -> MathExpr -> MathExpr
indexed = lift2 indexedStyle

derivative :: Bool -> Int -> MathExpr -> MathExpr -> MathExpr
derivative ind n = lift2 (\c -> derivativeStyle c ind n)

measurement :: MathExpr -> MathExpr
measurement = lift1 measurementStyle

transpose :: MathExpr -> MathExpr
transpose = lift1 transposeStyle

matrixInverse :: MathExpr -> MathExpr
matrixInverse = lift1 matrixInverseStyle

functionInverse :: MathExpr -> MathExpr
functionInverse = lift1 functionInverseStyle

expectation :: MathExpr -> MathExpr
expectation = lift1 expectationStyle

hasDistribution :: MathExpr -> MathExpr -> MathExpr
hasDistribution = lift2 hasDistributionStyle

normalDistribution :: MathExpr -> MathExpr -> MathExpr
normalDistribution = lift2 normalDistributionStyle

equals :: MathExpr -> MathExpr -> MathExpr
equals = lift2 (\_ x y -> x <> "=" <> y)

--product :: [MathExpr] -> MathExpr
--product xs = do
               --xs' <- sequence xs
               --return $ mconcat xs'

instance Num MathExpr where
  (+) = lift2 $ const (+)
  (-) = lift2 $ const (-)
  (*) = lift2 $ const (*)
  negate = lift1 $ const negate
  fromInteger = return . fromInteger
  abs = lift1 $const abs
  signum = lift1 $ const signum

instance Fractional MathExpr where
  (/) = lift2 $ const (/)
  fromRational = return . fromRational

instance Floating MathExpr where
  pi = return pi_
  exp = lift1 $ const exp
  sqrt = lift1 $ const sqrt
  log = lift1 $ const log
  (**) = lift2 $ const (**)
  logBase = lift2 $ const logBase
  sin = lift1 $ const sin
  tan = lift1 $ const tan
  cos = lift1 $ const cos
  asin = lift1 $ const asin
  atan = lift1 $ const atan
  acos = lift1 $ const acos
  sinh = lift1 $ const sinh
  tanh = lift1 $ const tanh
  cosh = lift1 $ const cosh
  asinh = lift1 $ const asinh
  atanh = lift1 $ const atanh
  acosh = lift1 $ const acosh

lift1 :: (NotationalConvention -> LaTeX -> LaTeX) -> MathExpr -> MathExpr
lift1 style x = do
                  c <- ask
                  x' <- x
                  return $ style c x'

lift2 :: (NotationalConvention -> LaTeX -> LaTeX -> LaTeX) -> MathExpr -> MathExpr -> MathExpr
lift2 style x y = do
                    c <- ask
                    x' <- x
                    y' <- y
                    return $ style c x' y'

data NotationalConvention = NotationalConvention
                          {
                            estimateStyle :: LaTeX -> LaTeX,
                            indexedStyle :: LaTeX -> LaTeX -> LaTeX,
                            derivativeStyle :: Bool -> Int -> LaTeX -> LaTeX -> LaTeX,
                            measurementStyle :: LaTeX -> LaTeX,
                            transposeStyle :: LaTeX -> LaTeX,
                            matrixInverseStyle :: LaTeX -> LaTeX,
                            functionInverseStyle :: LaTeX -> LaTeX,
                            expectationStyle :: LaTeX -> LaTeX,
                            hasDistributionStyle :: LaTeX -> LaTeX -> LaTeX,
                            normalDistributionStyle :: LaTeX -> LaTeX -> LaTeX,
                            matrixStyle :: forall a l.(Texy a, LaTeXC l) => Maybe HPos -> Matrix a -> l
                          }

wikipediaNotation :: NotationalConvention
wikipediaNotation = NotationalConvention
                  {
                    estimateStyle = hat,
                    indexedStyle = flip (!:),
                    derivativeStyle = \case 
                                        True -> dottedDerivative
                                        False -> defaultDerivative,
                    measurementStyle = (^: "*"),
                    transposeStyle = (^: mathrm "T"),
                    matrixInverseStyle = (^: (-1)),
                    functionInverseStyle = (^: (-1)),
                    expectationStyle = \x -> operatorname (mathrm "E") <> "[" <> x <> "]",
                    hasDistributionStyle = \v dist -> v <> (comm0 "sim") <> dist,
                    normalDistributionStyle = \mean v -> mathcal "N" <> "(" <> mean <> "," <> v <> ")",
                    matrixStyle = bmatrix
                  }

{-- metavariables for systems, belongs elsewhere
                    independentVariable = independentMetavariable "t",
                    stateVector = metavariable' "x" (mathbf "x"),
                    outputVector = metavariable' "y" (mathbf "y"),
                    inputVector = metavariable' "u" (mathbf "u"),
                    stateTransitionMatrix = metavariable "A",
                    inputMatrix = metavariable "B",
                    outputMatrix = metavariable "C",
                    feedthroughMatrix = metavariable "D",
                    stateTransitionFunction = metavariable "f",
                    outputFunction = metavariable "h",
                    discreteStateIndex = metavariable "k"
                  }
--}