{-# LANGUAGE Safe #-}
module Data.AbsoluteSolver.Parser 
    ( parseEquation
    , parseExpression
    ) where

import safe Prelude hiding (exp, exponent, log, logBase, product, sum)

import safe Data.AbsoluteSolver.Structures(Equation(..), AlgebraicStruct(..))
import safe Data.Text (pack, split, strip, unpack)
import safe Text.Parsec
import safe Text.Parsec.Language (haskell)
import safe Text.Parsec.Token (GenTokenParser(..)) 

-- | Parser type for reading algebraic structures from plain text
type AbSolve = Parsec String () AlgebraicStruct

parseExpression :: String -> Either ParseError AlgebraicStruct
parseExpression expr = runParser expression () expr expr

parseEquation :: String -> Either String Equation
parseEquation eqn 
    = case mapM parseExpression (preprocess eqn) of
        Right [lhs, rhs] -> Right $ Equation (lhs, rhs)
        Right (_:_:_) -> Left $ "more than 1 '=' found in equation: " ++ show eqn
        Right _ -> Left $ "given equation is an expression because it has no '=': " ++ show eqn
        Left x -> (Left . show) x
    where 
        preprocess :: String -> [String]
        preprocess = map (unpack . strip) . split (== '=') . pack 

expression :: AbSolve
expression = sum

polyTerm :: Char -> ([AlgebraicStruct] -> AlgebraicStruct) -> AbSolve -> String -> AbSolve
polyTerm c cnstrctr p name = do
    elements <- p `sepBy1` char c <?> name
    if length elements == 1 then
        return $ head elements
    else
        return $ cnstrctr elements

sum :: AbSolve
sum = polyTerm '+' Sum difference "sum"

difference :: AbSolve
difference = polyTerm '-' Difference product "difference"

product :: AbSolve
product = polyTerm '*' Product p3Term "product"

quotient :: AbSolve
quotient = do
    dend <- p2Term
    _ <- char '/'
    sor <- p2Term <?> "quotient"
    return Quotient 
        { dividend = dend
        , divisor  = sor
        }

exponent :: AbSolve
exponent = do
    expBase <- p1Term 
    _ <- char '^'
    expExp <- p1Term <?> "exponent"
    return Exponent
        { base = expBase
        , exp  = expExp
        }

logarithm :: AbSolve
logarithm = do
    whiteSpace haskell
    _ <- string "log"
    logBase <- expression
    logLog <- parens haskell expression <?> "logarithm"
    return Logarithm 
        { base = logBase
        , log  = logLog
        }

parenthetical :: AbSolve
parenthetical = Group <$> (whiteSpace haskell >> parens haskell expression) <?> "grouped expression"

-- | Parses an identifier (haskell definition) and returns an @AlgebraicStruct.Symbol@
value :: AbSolve
value = do
    val <- try (float haskell) <|> fromInteger <$> integer haskell <?> "value"
    return $ Value val

-- | Parses a number literal and returns an @AlgebraicStruct.Value@
symb :: AbSolve
symb = do
    sym <- whiteSpace haskell >> identifier haskell <?> "symbol"
    return $ Symbol sym

p3Term :: AbSolve
p3Term = try quotient <|> p2Term

p2Term :: AbSolve
p2Term = try exponent <|> p1Term

p1Term :: AbSolve
p1Term = try logarithm <|> p0Term

p0Term :: AbSolve
p0Term = try parenthetical <|> try symb <|> value