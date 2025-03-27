module Main (main) where

import Data.AbsoluteSolver (solvedFor)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2 then
        error "USAGE: <equation> <target-symbol>"
    else 
        case head args `solvedFor` last args of
            Left err -> print err
            Right (soln, steps) -> do
                putStrLn $ "Solution: " ++ show soln
                putStrLn $ "\nSteps: "
                mapM_ putStrLn steps