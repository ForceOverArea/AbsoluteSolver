module Main (main) where

import Data.AbsoluteSolver (solvedFor)

main :: IO ()
main = do
    -- symbolically solve the equation for 'ligma'
    putStrLn $ case "4 + 3 * ligma = 3" `solvedFor` "ligma" of
        Left x -> x
        Right x -> show x
