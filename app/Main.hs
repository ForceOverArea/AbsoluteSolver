module Main (main) where

import Data.AbsoluteSolver (solvedFor)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case head args `solvedFor` last args of
        Left err -> print err
        Right (soln, steps) -> do
            print soln
            mapM_ print steps