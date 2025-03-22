{-# LANGUAGE Safe #-}
module Data.AbsoluteSolver.Isolate
    ( (~?)
    , isolate
    , Steps
    ) where

import safe Prelude hiding (exp, log)
import safe Control.Monad.Trans (lift)
import safe Control.Monad.RWS (ask, get, put, tell, RWST(..))
import safe Control.Monad.Except (throwError, runExcept, Except)
import safe Data.AbsoluteSolver.Structures (AlgebraicStruct(..), Equation (Equation), Symbol)
import safe Data.List (partition)

type Steps = [String]

type Solver = RWST Symbol Steps Equation (Except SolverError)

data SolverError
    = NeedsPolysolve
    | SymbolNotFound
    | IsolationErrorOccurred
    deriving (Eq, Ord)

instance Show SolverError where
    show NeedsPolysolve = "the solution requires polysolving capability that is not yet supported"
    show SymbolNotFound = "the given symbol was not found in the given equation"
    show IsolationErrorOccurred = "an unknown error occurred while trying to isolate the given symbol in the equation"

-- | A binary operation that reveals whether @sym@ is present as 
--   a raw symbol in the given @AlgebraicStruct@.
(~?) :: AlgebraicStruct -> Symbol -> Bool
Sum terms ~? sym = any (~? sym) terms

Difference subtrahends ~? sym = any (~? sym) subtrahends

Product factors ~? sym = any (~? sym) factors

Quotient s d ~? sym = any (~? sym) [s, d]

Exponent b e ~? sym = any (~? sym) [b, e]

Logarithm b l ~? sym = any (~? sym) [b, l]

Group g ~? sym = g ~? sym

Value _ ~? _ = False

Symbol x ~? sym = x == sym

isolate :: Equation -> Symbol -> Either SolverError (Equation, Steps)
isolate eqn sym = do
    (_, result, steps) <- runExcept $ runRWST isolateMain sym eqn
    return (result, steps)

isolateMain :: Solver ()
isolateMain = do
    lhs <- getLhs
    sym <- ask
    logStep
    case lhs of
        Sum ts -> isolateSum ts
        Difference ss -> isolateDiff ss
        Product fs -> isolateProd fs
        Quotient d s -> isolateQuotient d s
        Exponent b e -> isolateExp b e
        Logarithm b l -> isolateLog b l
        Group g -> setLhs g
        Value _ -> lift $ throwError IsolationErrorOccurred
        Symbol s -> 
            if s == sym then 
                return () -- terminate recursive loop
            else
                lift $ throwError IsolationErrorOccurred

isolateSum :: [AlgebraicStruct] -> Solver ()
isolateSum terms = do
    wrapperTerms <- isolatePolynomialTerm terms
    modifyRhs $ \rhs -> Difference (Group rhs:wrapperTerms)
    isolateMain

isolateDiff :: [AlgebraicStruct] -> Solver ()
isolateDiff subtrahends = do
    wrapperTerms <- isolatePolynomialTerm subtrahends
    modifyRhs $ Sum . (:wrapperTerms)
    isolateMain

isolateProd :: [AlgebraicStruct] -> Solver ()
isolateProd factors = do
    wrapperTerms <- isolatePolynomialTerm factors
    modifyRhs $ \rhs -> Quotient (Group rhs) (Product wrapperTerms)
    isolateMain

isolateQuotient :: AlgebraicStruct -> AlgebraicStruct -> Solver ()
isolateQuotient d s = do
    sym <- ask
    case map (~? sym) [d, s] of
        [False, False] -> lift $ throwError SymbolNotFound
        [True, True] -> lift $ throwError NeedsPolysolve
        [True, False] -> do
            setLhs d
            modifyRhs $ Product . (:[s])
        [False, True] -> do
            setLhs s
            modifyRhs $ \rhs -> Quotient s rhs
        _ -> error "an unknown error occurred in isolateQuotient"
    isolateMain

isolateExp :: AlgebraicStruct -> AlgebraicStruct -> Solver ()
isolateExp b e = do
    sym <- ask
    case map (~? sym) [b, e] of
        [False, False] -> lift $ throwError SymbolNotFound
        [True, True] -> lift $ throwError NeedsPolysolve
        [True, False] -> do
            setLhs b
            let rhsExp = Group $ Quotient (Value 1.0) e
            modifyRhs $ \rhs -> Exponent rhs rhsExp
        [False, True] -> do
            setLhs e
            modifyRhs $ \rhs -> Logarithm b rhs
        _ -> error "an unknown error occurred in isolateExponent"
    isolateMain

isolateLog :: AlgebraicStruct -> AlgebraicStruct -> Solver ()
isolateLog b l = do
    sym <- ask
    case map (~? sym) [b, l] of
        [False, False] -> lift $ throwError SymbolNotFound
        [True, True] -> lift $ throwError NeedsPolysolve
        [True, False] -> do
            setLhs b
            let rhsExp = Group $ Quotient (Value 1.0) l
            modifyRhs $ flip Exponent rhsExp
        [False, True] -> do
            setLhs l
            modifyRhs $ Exponent b . Group
        _ -> error "an unknown error has occurred in isolateLogarithm"
    isolateMain

modifyRhs :: (AlgebraicStruct -> AlgebraicStruct) -> Solver ()
modifyRhs f = do
    Equation (lhs, rhs) <- get
    put $ Equation (lhs, f rhs)

logStep :: Solver ()
logStep = do
    step <- show <$> get
    tell [step]

getLhs :: Solver AlgebraicStruct
getLhs = do
    Equation (lhs, _) <- get
    return lhs

setLhs :: AlgebraicStruct -> Solver ()
setLhs lhs = do
    Equation (_, rhs) <- get
    put $ Equation (lhs, rhs)

isolatePolynomialTerm :: [AlgebraicStruct] -> Solver [AlgebraicStruct]
isolatePolynomialTerm terms = do
    sym <- ask
    let (wrapped, wrapper) = partition (~? sym) terms
    let n = length wrapped
    if n /= 1 then do
        case n of
            0 -> lift $ throwError SymbolNotFound
            _ -> lift $ throwError NeedsPolysolve
    else do
        setLhs $ head wrapped
        return wrapper