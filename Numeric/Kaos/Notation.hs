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
import Data.Traversable
import Text.LaTeX
import Text.LaTeX.Base.Class (LaTeXC, fromLaTeX, comm0, commS)
import Text.LaTeX.Packages.AMSMath

variable :: LaTeX -> MathExpr
variable = return

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

matrix :: Matrix MathExpr -> MathExpr
matrix xs = do
              c <- ask
              xs' <- sequenceA xs
              return $ matrixStyle c Nothing xs'

jacobianAt :: MathExpr -> MathExpr -> MathExpr -> MathExpr
jacobianAt = lift3 jacobianAtStyle

equals :: MathExpr -> MathExpr -> MathExpr
equals = lift2' (\x y -> x <> "=" <> y)

instance Num MathExpr where
  (+) = lift2' (+)
  (-) = lift2' (-)
  (*) = lift2' (*)
  negate = lift1' negate
  fromInteger = return . fromInteger
  abs = lift1' abs
  signum = lift1' signum

instance Fractional MathExpr where
  (/) = lift2' (/)
  fromRational = return . fromRational

instance Floating MathExpr where
  pi = return pi_
  exp = lift1' exp
  sqrt = lift1' sqrt
  log = lift1' log
  (**) = lift2' (**)
  logBase = lift2' logBase
  sin = lift1' sin
  tan = lift1' tan
  cos = lift1' cos
  asin = lift1' asin
  atan = lift1' atan
  acos = lift1' acos
  sinh = lift1' sinh
  tanh = lift1' tanh
  cosh = lift1' cosh
  asinh = lift1' asinh
  atanh = lift1' atanh
  acosh = lift1' acosh

lift1 :: (NotationalConvention -> LaTeX -> LaTeX) -> MathExpr -> MathExpr
lift1 style x = do
                  c <- ask
                  x' <- x
                  return $ style c x'

lift1' :: (LaTeX -> LaTeX) -> MathExpr -> MathExpr
lift1' = lift1 . const

lift2 :: (NotationalConvention -> LaTeX -> LaTeX -> LaTeX) -> MathExpr -> MathExpr -> MathExpr
lift2 style x y = do
                    c <- ask
                    x' <- x
                    y' <- y
                    return $ style c x' y'

lift2' :: (LaTeX -> LaTeX -> LaTeX) -> MathExpr -> MathExpr -> MathExpr
lift2' = lift2 . const

lift3 :: (NotationalConvention -> LaTeX -> LaTeX -> LaTeX -> LaTeX) -> MathExpr -> MathExpr -> MathExpr -> MathExpr
lift3 style x y z = do
                      c <- ask
                      x' <- x
                      y' <- y
                      z' <- z
                      return $ style c x' y' z'

type MatrixFormatFunction = forall a l.(Texy a, LaTeXC l) => Maybe HPos -> Matrix a -> l

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
                            matrixStyle :: MatrixFormatFunction,
                            jacobianAtStyle :: LaTeX -> LaTeX -> LaTeX -> LaTeX
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
                    matrixStyle = bmatrix,
                    jacobianAtStyle = defaultJacobianAtStyle
                  }

defaultJacobianAtStyle :: LaTeX -> LaTeX -> LaTeX -> LaTeX
defaultJacobianAtStyle f x x' = left <> "." <> ((partial <> f) / (partial <> x)) <> right <> vert !: x'
  where
    left = commS "left"
    partial = comm0 "partial"
    right = commS "right"
    vert = commS "vert"

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