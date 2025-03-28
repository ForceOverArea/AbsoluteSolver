{-# LANGUAGE ForeignFunctionInterface #-}

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
