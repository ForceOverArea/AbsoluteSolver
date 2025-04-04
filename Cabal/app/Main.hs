module Main (main) where

import Data.AbsoluteSolver (solvedFor, solvedForValue')
import Data.AbsoluteSolver.Internal (parseContextString)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [x, y] ->  case x `solvedFor` y of
            Left err -> print err
            Right (soln, steps) -> do
                putStrLn $ "Solution: " ++ show soln
                putStrLn $ "\nSteps: "
                mapM_ putStrLn steps
        [x, y, z] -> case solvedForValue' x y (parseContextString z) of
            Left err -> print err
            Right (val, soln, steps) -> do
                putStrLn $ "Solution: " ++ show soln
                putStrLn $ ('\n':y) ++ " = " ++ show val
                putStrLn $ "\nSteps: "
                mapM_ putStrLn steps
        _ -> do
            putStrLn "Incorrect number of arguments"
            putStrLn "USAGE: <equation> <target> [context]"