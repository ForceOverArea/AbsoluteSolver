{-# LANGUAGE ForeignFunctionInterface #-}

module Data.AbsoluteSolver.C () where

import Control.Arrow ((|||))
import Data.AbsoluteSolver (solvedFor, solvedForValue')
import Data.AbsoluteSolver.Internal (parseContextString)
import Data.AbsoluteSolver.Structures (Equation)
import Foreign (free, nullPtr, Ptr)
import Foreign.C (newCString, peekCString, CString, CDouble(..))

solvedForHs :: CString -> CString -> IO CString
solvedForHs eqn target = do
    eqn' <- peekCString eqn
    target' <- peekCString target
    case eqn' `solvedFor` target' of
        Left _ -> return nullPtr
        Right (soln, _steps) -> newCString $ show soln

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

foreign export ccall free :: Ptr a -> IO ()
foreign export ccall solvedForHs :: CString -> CString -> IO CString
foreign export ccall solvedForValueHs :: CString -> CString -> CString -> IO CDouble