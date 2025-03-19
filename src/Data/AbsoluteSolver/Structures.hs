module Data.AbsoluteSolver.Structures 
    ( (~?)
    , isolate
    , AlgebraicStruct(..)
    , Equation(..)
    ) where

import Prelude hiding (exp)
import Data.List (intercalate, partition)

-- | An equation represented by two algebraic structures whose values are deemed to be equivalent
newtype Equation = Equation (AlgebraicStruct, AlgebraicStruct)

instance Show Equation where
    show (Equation (lhs, rhs)) = show lhs ++ " = " ++ show rhs

-- | A type alias for 
type Symbol = String

-- | Represents a (possibly nested) 'schoolyard algebra' structure
data AlgebraicStruct
    -- | Represents the sum of an unknown number of terms
    = Sum [AlgebraicStruct]

    -- | Represents the difference of an unknown number of subtrahends
    | Difference [AlgebraicStruct]

    -- | Represents the product of an unknown number of factors 
    | Product [AlgebraicStruct]

    -- | Represents the quotient of a @dividend@ and a @divisor@
    | Quotient 
        { dividend :: AlgebraicStruct
        , divisor  :: AlgebraicStruct
        }

    -- | Represents a @base@ value raised to the power of @exponent@
    | Exponent
        { base :: AlgebraicStruct
        , exp  :: AlgebraicStruct
        }

    -- | Represents the exponent required to achieve @log@ given @base@
    | Logarithm
        { base :: AlgebraicStruct
        , log  :: AlgebraicStruct
        }

    -- | Represents a grouped set of algebraic structures contained within parenthesis
    | Group AlgebraicStruct

    -- | Represents a raw numeric value that cannot be reversed and contains no other algebraic structures
    | Value Double

    -- | Represents a raw value that cannot be reversed and contains no other algebraic structures
    | Symbol String
    -- deriving Show

instance Show AlgebraicStruct where
    show (Sum terms) = intercalate " + " $ map show terms

    show (Difference subtrahends) = intercalate " - " $ map show subtrahends

    show (Product factors) = intercalate " * " $ map show factors

    show (Quotient sor dend) = "(" ++ show sor ++ ") / " ++ show dend

    show (Exponent b e) = show b ++ "^(" ++ show e ++ ")"

    show (Logarithm b l) = "log" ++ show b ++ "(" ++ show l ++ ")"

    show (Group g) = "(" ++ show g ++ ")"

    show (Value val) = show val

    show (Symbol sym) = sym

-- | A binary operation that reveals whether @sym@ is present as 
--   a raw symbol in the given @AlgebraicStruct@.
(~?) :: AlgebraicStruct -> Symbol -> Bool
Sum terms ~? sym = any (~? sym) terms

Difference subtrahends ~? sym = any (~? sym) subtrahends

Product factors ~? sym = any (~? sym) factors

Quotient sor dend ~? sym = any (~? sym) [sor, dend]

Exponent b e ~? sym = any (~? sym) [b, e]

Logarithm b l ~? sym = any (~? sym) [b, l]

Group g ~? sym = g ~? sym

Value _ ~? _ = False

Symbol x ~? sym = x == sym

isolate :: Equation -> Symbol -> Equation
isolate (Equation (Sum lhsTerms, rhs)) sym =
    case partition (~? sym) lhsTerms of
        ([wrapped], wrapper) -> Equation (wrapped, Difference [rhs, Sum wrapper]) `isolate` sym
        _ -> error "solution requires polysolve"

isolate (Equation (Difference lhsTerms, rhs)) sym =
    case partition (~? sym) lhsTerms of
        ([wrapped], wrapper) -> Equation (wrapped, Sum [rhs, Sum wrapper]) `isolate` sym
        _ -> error "solution requires polysolve"

isolate (Equation (Product lhsFactors, rhs)) sym = 
    case partition (~? sym) lhsFactors of
        ([wrapped], wrapper) -> Equation (wrapped, Quotient rhs $ Product wrapper)
        _ -> error "solution requires polysolve"

-- isolate (Equation (Quotient q, rhs)) sym = 
--     case partition (~? sym) [dividend q, divisor q] of
--         ()

isolate (Equation (Group lhsGrp, rhs)) _ = 
    Equation (lhsGrp, rhs)

isolate _ _ = error "ligma balls"