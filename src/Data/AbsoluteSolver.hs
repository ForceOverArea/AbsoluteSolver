module Data.AbsoluteSolver 
    ( solvedFor
    ) where
import Data.AbsoluteSolver.Parser (parseEquation)
import Data.AbsoluteSolver.Isolate (isolate, Steps)
import Data.AbsoluteSolver.Structures (Equation)

solvedFor :: String -> String -> Either String (Equation, Steps)
solvedFor eqn sym = do
    result <- (`isolate` sym) <$> parseEquation eqn
    case result of
        Left err -> Left $ show err
        Right soln -> Right soln