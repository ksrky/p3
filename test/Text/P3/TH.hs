module Text.P3.TH
    ( syntax
    , syntaxs
    ) where

import Control.Applicative          hiding (optional)
import Data.Char
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax   (Lift (lift))
import Text.ParserCombinators.ReadP
import Text.P3.OperatorParser
import Text.P3.Types

pName :: ReadP Name
pName = Name <$> ((:) <$> satisfy isAlpha <*> munch1 isAlphaNum)

pInt :: ReadP Int
pInt = read <$> munch1 isDigit

pBindingPower :: ReadP BindingPower
pBindingPower = do
    bp <- pInt
    if bp < minBound || bp > maxBound
        then pfail
        else return $ BindingPower bp

pOperator :: ReadP (Oper String)
pOperator = do
    _ <- char '"'
    op <- munch1 (/= '"')
    _ <- char '"'
    return $ Operator op

pOperand :: ReadP (Oper String)
pOperand = do
    _ <- char ':'
    Operand <$> pBindingPower

pOper :: ReadP (Oper String)
pOper = pOperator +++ pOperand

pOpers :: ReadP [Oper String]
pOpers = some (skipSpaces >> pOper)

pMixfixOp :: ReadP (MixfixOp String)
pMixfixOp = do
    name <- skipSpaces *> pName
    opers <- pOpers <* skipSpaces
    return $ MixfixOp name opers

pMixfixOps :: ReadP [MixfixOp String]
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