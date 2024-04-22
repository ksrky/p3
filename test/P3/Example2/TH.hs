module P3.Example2.TH
    ( syntax
    , syntaxs
    ) where

import Control.Applicative          hiding (optional)
import Data.Char
import Data.Map.Strict              qualified as M
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax   (Lift (lift))
import P3.Example2.Lexer            qualified as Lexer
import P3.Types
import P3.Utils
import Text.ParserCombinators.ReadP

parserCatIds :: M.Map String ParserCategory
parserCatIds = M.fromList
    [ ("exp", 0)
    , ("typ", 1)
    , ("var", 2)
    ]

pName :: ReadP Name
pName = Name <$> ((:) <$> satisfy isUpper <*> munch isAlphaNum)

pInt :: ReadP Int
pInt = read <$> munch1 isDigit

pOperator :: ReadP (Oper Lexer.Token)
pOperator = do
    _ <- char '"'
    op <- munch1 (/= '"')
    _ <- char '"'
    return $ Operator $ Lexer.mkToken op

pParserCat :: ReadP ParserCategory
pParserCat = do
    catid <- (:) <$> satisfy isLower <*> munch isAlphaNum
    case M.lookup catid parserCatIds of
        Just c  -> return c
        Nothing -> fail "unknown parser category"

pOperand :: ReadP (Oper Lexer.Token)
pOperand = (Operand <$> option 0 pParserCat <* char ':' <*> pInt)
    <++ (Operand <$> pParserCat <*> pure 0)

pOper :: ReadP (Oper Lexer.Token)
pOper = pOperator +++ pOperand

pOpers :: ReadP [Oper Lexer.Token]
pOpers = some (skipSpaces >> pOper)

pMixfixOp :: ReadP (MixfixOp Lexer.Token)
pMixfixOp = MixfixOp <$> (skipSpaces *> pName) <*> pOpers <* skipSpaces

pMixfixOps :: ReadP [MixfixOp Lexer.Token]
pMixfixOps = some (skipSpaces >> pMixfixOp) <* skipSpaces

mkQuasiQuoter :: Lift a => ReadP a -> QuasiQuoter
mkQuasiQuoter p =
    QuasiQuoter
        { quoteExp = \s -> case readP_to_S p s of
            [(res, "")] -> lift res
            _           -> fail "parse error."
        , quotePat = error "Usage as a parttern is not supported"
        , quoteType = error "Usage as a type is not supported"
        , quoteDec = error "Usage as a declaration is not supported"
        }

{- | syntax declaration

@
[syntax| Add :65 "+" :66 |]
@
-}
syntax :: QuasiQuoter
syntax = mkQuasiQuoter (pMixfixOp <* eof)

{- | syntax declarations

@
[syntaxs|
Add  :65 "+" :66
Mul  :70 "*" :71
|]
@
-}
syntaxs :: QuasiQuoter
syntaxs = mkQuasiQuoter (pMixfixOps <* eof)
