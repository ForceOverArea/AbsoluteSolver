{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Data.AbsoluteSolver.Internal where

import safe Control.Arrow ((&&&), (|||), second)
import safe Data.AbsoluteSolver (solvedFor, solvedForValue')
import safe Data.AbsoluteSolver.Structures (Equation)
import safe Foreign.C.String (peekCString, CString)
import safe Foreign.C.Types (CDouble(..))
import safe Foreign.C (newCString)

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