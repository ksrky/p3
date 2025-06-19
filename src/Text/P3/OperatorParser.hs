{-|
Parser for mixfix operators.
-}
module Text.P3.OperatorParser
    ( Oper (..)
    , MixfixOp (..)
    , insertMixfixParser
    , mkParserTable
    , infixOp
    , infixlOp
    , infixrOp
    , prefixOp
    , postfixOp
    ) where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader.Class
import Language.Haskell.TH.Syntax (Lift)
import Text.P3.Logic
import Text.P3.Monad
import Text.P3.Types

-- | Operator or Operand
data Oper t
    = Operator t
    | Operand BindingPower
    deriving (Lift)

instance (Show t) => Show (Oper t) where
    show (Operator t) = show t
    show (Operand bp) = ":" ++ show bp

data MixfixOp t = MixfixOp
    { name  :: Name
    , opers :: [Oper t]
    }
    deriving (Show, Lift)

-- | Build a parser from Operator/Operand list.
parseOpers :: Token t => [Oper t] -> ParserM t ()
parseOpers opers = forM_ opers $ \case
    Operator t -> matchToken (t ==)
    Operand bp -> local (bindingPower .~ bp) parseLeading

-- | Insert a parser for a mixfix operator into a parser table.
insertMixfixParser :: Token t => MixfixOp t -> ParserTable t -> ParserTable t
insertMixfixParser MixfixOp{name, opers = Operator t0 : opers} = do
    let arity = length [() | Operand{} <- opers]
        parser = execParserM $ do
            parseOpers opers
            mkNode name arity
    insertLeadingParser t0 parser
insertMixfixParser MixfixOp{name, opers = Operand bp0 : Operator t1 : opers} = do
    let arity = 1 + length [() | Operand{} <- opers]
        parser = execParserM $ do
            bp <- view bindingPower
            when (bp0 < bp) $ throwError LowerBindingPower
            nextToken_
            parseOpers opers
            mkNode name arity
            parseTrailing
    insertTrailingParser t1 parser
insertMixfixParser _ = error "invalid mixfix op: an operator does not appear at the first or second position."

-- | Create a parser table from a list of mixfix operators.
mkParserTable :: Token t => [MixfixOp t] -> ParserTable t
mkParserTable = foldr insertMixfixParser initParserTable

-- | Operator/Operand list of infix parsers.
infixOp :: t -> BindingPower -> [Oper t]
infixOp t bp = [Operand bp, Operator t, Operand bp]

-- | Operator/Operand list of infixl parsers.
infixlOp :: t -> BindingPower -> [Oper t]
infixlOp t bp = [Operand (succ bp), Operator t, Operand bp]

-- | Operator/Operand list of infixr parsers.
infixrOp :: t -> BindingPower -> [Oper t]
infixrOp t bp = [Operand bp, Operator t, Operand (succ bp)]

-- | Operator/Operand list of prefix parsers.
prefixOp :: t -> BindingPower -> [Oper t]
prefixOp t bp = [Operator t, Operand bp]

-- | Operator/Operand list of postfix parsers.
postfixOp :: t -> BindingPower -> [Oper t]
postfixOp t bp = [Operand bp, Operator t]
