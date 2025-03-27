{-# LANGUAGE ForeignFunctionInterface #-}

module Glue where

import Control.Arrow ((&&&), (|||), second, right)
import Data.AbsoluteSolver (solvedFor, solvedForValue')
import Data.AbsoluteSolver.Evaluate (Context)
import Data.AbsoluteSolver.Structures (Equation)
import Data.List (findIndex)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CDouble(..))
import Foreign.C (newCString)

solvedForHs :: CString -> CString -> IO CString
solvedForHs eqn target = do
    eqn' <- peekCString eqn
    target' <- peekCString target
    let result = eqn' `solvedFor` target'
    newCString $ coerceResult result
    where 
        coerceResult :: Either String (Equation, b) -> String
        coerceResult = id ||| show . fst

solvedForValueHs :: CString -> CString -> CString -> IO CDouble
solvedForValueHs eqn target ctx = do
    ctx' <- parseContextString <$> peekCString ctx
    eqn' <- peekCString eqn
    target' <- peekCString target
    return $ 
        case solvedForValue' eqn' target' ctx' of
            Left _ -> CDouble nan
            Right (soln, _, _) -> CDouble soln
    where 
        nan :: Double
        nan = 0.0 / 0.0

parseContextString :: String -> [(String, Double)]
parseContextString ctx = map parseItem items
    where
        items :: [String]
        items = splitStrAt ',' ctx
        
        parseItem :: String -> (String, Double)
        parseItem = second read . get2 . splitStrAt '='

get2 :: [a] -> (a, a)
get2 = head &&& head . tail

splitStrAt :: Char -> String -> [String]
splitStrAt p s = case dropWhile (== p) s of
    "" -> []
    s' -> w : splitStrAt p s''
        where (w, s'') = break (== p) s'

foreign export ccall solvedForHs :: CString -> CString -> IO CString

foreign export ccall solvedForValueHs :: CString -> CString -> CString -> IO CDouble