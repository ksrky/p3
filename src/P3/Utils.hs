module P3.Utils
    ( Oper (..)
    , MixfixOp (..)
    , parseOpers
    ) where

import Control.Lens.Combinators
import Control.Monad.Except
import Language.Haskell.TH.Syntax (Lift)
import P3.Combinators
import P3.Init
import P3.Monad
import P3.Types
import Control.Monad.Reader.Class

-- * MixfixOp

-- | Operator or Operand
data Oper t
    = Operator t
    | Operand BindingPower
    deriving (Lift)

instance (Show t) => Show (Oper t) where
    show (Operator t) = show t
    show (Operand bp) = ':' : show bp

data MixfixOp t = MixfixOp
    { name  :: Name
    , opers :: [Oper t]
    }
    deriving (Show, Lift)

parseOpers :: (Token t, MonadReader e m, HasParserTable e t m) => [Oper t] -> ParserM t m ()
parseOpers opers = forM_ opers $ \case
    Operator t -> matchToken (t ==)
    Operand bp -> withBindPow bp parseLeading

instance MkParserEntry (MixfixOp t) t where
    mkParserEntry MixfixOp{name, opers = Operator t0 : opers} = do
        let arity = length $ filter (\case Operand _ -> True; _ -> False) opers
            parser = execParserM $ do
                parseOpers opers
                mkNode name arity
        Left (t0, parser)
    mkParserEntry MixfixOp{name, opers = Operand bp0 : Operator t1 : opers} = do
        let arity = 1 + length (filter (\case Operand _ -> True; _ -> False) opers)
            parser = execParserM $ do
                bp <- view bindPow
                when (bp0 < bp) $ throwError LowerBindingPower
                nextToken_
                parseOpers opers
                mkNode name arity
                parseTrailing
        Right (t1, parser)
    mkParserEntry _ = error "invalid mixfix op"
