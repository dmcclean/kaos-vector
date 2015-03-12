{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Numeric.Kaos.Notation

where

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
dottedDerivative n x y = derivative n x y

derivative :: Int -> LaTeX -> LaTeX -> LaTeX
derivative 0 x _ = x
derivative 1 x y = fromLaTeX $ ("d" <> x) / ("d" <> y)
derivative n x y = fromLaTeX $ ("d" ** n' <> x) / (("d" <> y) ** n')
  where
    n' = texy n

samples :: LaTeX -> LaTeX -> LaTeX
samples var dist = var <> (comm0 "sim") <> dist

normalDistribution :: LaTeX -> LaTeX -> LaTeX
normalDistribution mean var = mathcal "N" <> "(" <> mean <> "," <> var <> ")"

expectation :: LaTeX -> LaTeX
expectation x = operatorname (mathrm "E") <> "[" <> x <> "]"

data NotationalConvention = NotationalConvention
                          {
                            estimateStyle :: LaTeX -> LaTeX,
                            indexedStyle :: LaTeX -> LaTeX -> LaTeX,
                            derivativeStyle :: Bool -> Int -> LaTeX -> LaTeX -> LaTeX,
                            measurementStyle :: LaTeX -> LaTeX,
                            transposeStyle :: LaTeX -> LaTeX,
                            matrixInverseStyle :: LaTeX -> LaTeX,
                            functionInverseStyle :: LaTeX -> LaTeX,
                            matrixStyle :: forall a l.(Texy a, LaTeXC l) => Maybe HPos -> Matrix a -> l,
                            independentVariable :: Metavariable,
                            stateVector :: Metavariable,
                            outputVector :: Metavariable,
                            inputVector :: Metavariable,
                            stateTransitionMatrix :: Metavariable,
                            inputMatrix :: Metavariable,
                            outputMatrix :: Metavariable,
                            feedthroughMatrix :: Metavariable,
                            stateTransitionFunction :: Metavariable,
                            outputFunction :: Metavariable,
                            discreteStateIndex :: Metavariable
                          }

wikipediaNotation :: NotationalConvention
wikipediaNotation = NotationalConvention
                  {
                    estimateStyle = hat,
                    indexedStyle = flip (!:),
                    derivativeStyle = \case 
                                        True -> dottedDerivative
                                        False -> derivative,
                    measurementStyle = (^: "*"),
                    transposeStyle = (^: mathrm "T"),
                    matrixInverseStyle = (^: (-1)),
                    functionInverseStyle = (^: (-1)),
                    matrixStyle = bmatrix,
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