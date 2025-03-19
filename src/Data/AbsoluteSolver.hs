module Data.AbsoluteSolver 
    ( solvedFor
    ) where
import Data.AbsoluteSolver.Parser (parseEquation)
import Data.AbsoluteSolver.Structures (isolate)

solvedFor :: String -> String -> Either String String
solvedFor eqn sym = case show . (`isolate` sym) <$> parseEquation eqn of
    Left err -> Left $ show err
    Right soln -> Right soln