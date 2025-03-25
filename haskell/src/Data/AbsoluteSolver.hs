module Data.AbsoluteSolver 
    ( solvedFor
    , solvedForValue
    ) where

import Control.Arrow (left)
import Data.AbsoluteSolver.Evaluate (evaluate, Context)
import Data.AbsoluteSolver.Isolate (isolate, Steps)
import Data.AbsoluteSolver.Parser (parseEquation)
import Data.AbsoluteSolver.Structures (Equation(..))
import Data.Map (fromList)

solvedFor :: String -> String -> Either String (Equation, Steps)
solvedFor eqn sym = do
    result <- (`isolate` sym) <$> parseEquation eqn
    stringizeError result

solvedForValue :: String -> String -> Context -> Either String (Double, Equation, Steps)
solvedForValue eqn sym ctx = do
    (eqn', steps) <- eqn `solvedFor` sym
    let Equation (_, value) = eqn'
    value' <- stringizeError $ evaluate value ctx
    return (value', eqn', steps)

solvedForValue' :: String -> String -> [(String, Double)] -> Either String (Double, Equation, Steps)
solvedForValue' eqn sym ctx = solvedForValue eqn sym $ fromList ctx

stringizeError :: Show a => Either a b -> Either String b
stringizeError = left show