module Data.AbsoluteSolver.Evaluate 
    ( evaluate
    ) where

import Control.Monad.Except (Except, MonadError (throwError))
import Control.Monad.Reader (asks, ReaderT)
import Control.Monad.Trans (lift)
import Data.AbsoluteSolver.Structures (AlgebraicStruct(..))
import qualified Data.Map as M (lookup, Map)

type Context = M.Map String Double

data EvalError = SymbolNotDefined

type Evaluate = ReaderT Context (Except EvalError)

evaluate :: AlgebraicStruct -> Evaluate Double
evaluate (Sum terms) = sum <$> mapM evaluate terms

evaluate (Difference subtrahends) = foldl (-) 0 <$> mapM evaluate subtrahends

evaluate (Product factors) = product <$> mapM evaluate factors

evaluate (Quotient d s) = do
    d' <- evaluate d
    s' <- evaluate s
    return $ d' / s'

evaluate (Exponent b e) = do
    b' <- evaluate b
    e' <- evaluate e
    return $ b' ** e'

evaluate (Logarithm b l) = do
    b' <- evaluate b
    l' <- evaluate l
    return $ logBase b' l'

evaluate (Group g) = evaluate g

evaluate (Value v) = return v

evaluate (Symbol s) = do
    value <- asks (M.lookup s)
    case value of
        Just x -> return x
        Nothing -> lift $ throwError SymbolNotDefined
