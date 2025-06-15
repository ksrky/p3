{-|
Constructors useful for building `ParserEntry`
-}
module P3.Utils
    ( Oper (..)
    , MixfixOp (..)
    , insertParser
    ) where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader.Class
import Language.Haskell.TH.Syntax (Lift)
import P3.Combinators
import P3.Init
import P3.Monad
import P3.Types
import P3.Logic

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
    Operand bp ->
        local (bindingPower .~ bp)
        parseLeading

instance ParserSpec MixfixOp where
    insertParser MixfixOp{name, opers = Operator t0 : opers} = do
        let ators = [t | Operator t <- opers]
            arity = length [() | Operand{} <- opers]
            parser = execParserM $ do
                local (reservedTokens <>~ ators) $ parseOpers opers
                mkNode name arity
        insertLeadingParser t0 parser
    insertParser MixfixOp{name, opers = Operand bp0 : Operator t1 : opers} = do
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
    insertParser _ =  error "invalid mixfix op: an operator does not appear at the first or second position."

