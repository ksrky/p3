{-|
Constructors useful for building `ParserEntry`
-}
module Text.P3.OperatorParser
    ( Oper (..)
    , MixfixOp (..)
    , insertMixfixParser
    , mkParserTable
    , insertInfixParser
    , insertInfixlParser
    , insertInfixrParser
    , insertPrefixParser
    , insertPostfixParser
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

parseOpers :: Token t => [Oper t] -> ParserM t ()
parseOpers opers = forM_ opers $ \case
    Operator t -> matchToken (t ==)
    Operand bp -> local (bindingPower .~ bp) parseLeading

insertMixfixParser :: Token t => MixfixOp t -> ParserTable t -> ParserTable t
insertMixfixParser MixfixOp{name, opers = []} = error $ "insertMixfixParser: " ++ show name ++ " has no operators."
insertMixfixParser MixfixOp{name, opers = [Operand _]} = error $ "insertMixfixParser: " ++ show name ++ " has no operators."
insertMixfixParser MixfixOp{name, opers = Operator t0 : opers} = do
    let ators = [t | Operator t <- opers]
        arity = length [() | Operand{} <- opers]
        parser = execParserM $ do
            local (reservedTokens <>~ ators) $ parseOpers opers
            mkNode name arity
    insertLeadingParser t0 parser
insertMixfixParser MixfixOp{name, opers = Operand bp0 : Operator t1 : opers} = do
    let ators = [t | Operator t <- opers]
        arity = 1 + length [() | Operand{} <- opers]
        parser = execParserM $ do
            bp <- view bindingPower
            when (bp0 < bp) $ throwError LowerBindingPower
            nextToken_
            local (reservedTokens <>~ ators) $ parseOpers opers
            mkNode name arity
            parseTrailing
    insertTrailingParser t1 parser
insertMixfixParser MixfixOp{name, opers = Operand bp0 : Operand bp1 : opers} = do
    let ators = [t | Operator t <- opers]
        arity = 0 + length [() | Operand{} <- opers]
        parser = execParserM $ do
            bp <- view bindingPower
            when (bp0 < bp) $ throwError LowerBindingPower
            local (reservedTokens <>~ ators) $ parseOpers (Operand bp1 : opers)
            mkNode name arity
    insertUnindexedParser parser

mkParserTable :: Token t => [MixfixOp t] -> ParserTable t
mkParserTable = foldr insertMixfixParser initParserTable

insertInfixParser :: Token t => Name -> t -> BindingPower -> ParserTable t -> ParserTable t
insertInfixParser name t bp = insertMixfixParser MixfixOp
    { name = name
    , opers = [Operand bp, Operator t, Operand bp]
    }

insertInfixlParser :: Token t => Name -> t -> BindingPower -> ParserTable t -> ParserTable t
insertInfixlParser name t bp = insertMixfixParser MixfixOp
    { name = name
    , opers = [Operand (succ bp), Operator t, Operand bp]
    }

insertInfixrParser :: Token t => Name -> t -> BindingPower -> ParserTable t -> ParserTable t
insertInfixrParser name t bp = insertMixfixParser MixfixOp
    { name = name
    , opers = [Operand bp, Operator t, Operand (succ bp)]
    }

insertPrefixParser :: Token t => Name -> t -> BindingPower -> ParserTable t -> ParserTable t
insertPrefixParser name t bp = insertMixfixParser MixfixOp
    { name = name
    , opers = [Operator t, Operand bp]
    }

insertPostfixParser :: Token t => Name -> t -> BindingPower -> ParserTable t -> ParserTable t
insertPostfixParser name t bp = insertMixfixParser MixfixOp
    { name = name
    , opers = [Operand bp, Operator t]
    }
