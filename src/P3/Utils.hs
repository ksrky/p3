{-|
Constructors useful for building `ParserEntry`
-}
module P3.Utils
    ( Oper (..)
    , MixfixOp (..)
    ) where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Except
import Control.Monad.Reader.Class
import Language.Haskell.TH.Syntax (Lift)
import P3.Init
import P3.Monad
import P3.Types

-- * MixfixOp

-- | Operator or Operand
data Oper t
    = Operator t
    | Operand ParserCategory BindingPower
    deriving (Lift)

instance (Show t) => Show (Oper t) where
    show (Operator t)     = show t
    show (Operand cat bp) = show cat ++ ":" ++ show bp

-- | Mixfix operator represents almost any kind of operators.
-- For example, you can also represent Haskell's unit value `()` as a mixfix operator.
data MixfixOp t = MixfixOp
    { name  :: Name
    , opers :: [Oper t]
    }
    deriving (Show, Lift)

parseOpers :: (Token t, MonadReader e m, HasParserCatTable e t m) => [Oper t] -> ParserM t m ()
parseOpers opers = forM_ opers $ \case
    Operator t -> matchToken (t ==)
    Operand cat bp -> local (parserCat .~ cat) $ local (bindPow .~ bp) parseLeading

instance MkParserEntry (MixfixOp t) t where
    mkParserEntry MixfixOp{name, opers = Operator t0 : opers} = do
        let ators = [t | Operator t <- opers]
            arity = length [() | Operand{} <- opers]
            parser = execParserM $ do
                local (reservedToks <>~ ators) $ parseOpers opers
                mkNode name arity
        LeadingEntry t0 parser
    mkParserEntry MixfixOp{name, opers = Operand _ bp0 : Operator t1 : opers} = do
        let ators = [t | Operator t <- opers]
            arity = 1 + length [() | Operand{} <- opers]
            parser = execParserM $ do
                bp <- view bindPow
                when (bp0 < bp) $ throwError LowerBindingPower
                nextToken_
                local (reservedToks <>~ ators) $ parseOpers opers
                mkNode name arity
                parseTrailing
        TrailingEntry t1 parser
    mkParserEntry _ = error "invalid mixfix op: an operator does not appear at the first or second position.\n\
                            \You may use a unindexed parser instead."
