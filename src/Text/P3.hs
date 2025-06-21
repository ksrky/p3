module Text.P3
    ( -- * Parser top function
      parse
      -- * Parser monad
    , ParserContext (..)
    , ParserState (..)
    , Parser
    , (>>>)
    , ParserTable (..)
    , insertLeadingParser
    , insertTrailingParser
    , leadingParsersOf
    , trailingParsersOf
    , runParser
    , mkAtom
    , mkNode
      -- * Parser types
    , Name (..)
    , BindingPower (..)
    , Token (..)
    , Syntax (..)
      -- * Parser core logic
    , parseLeading
    , parseTrailing
      -- * Operator parser
    , Oper (..)
    , MixfixOp (..)
    , insertMixfixParser
    , mkParserTable
    , infixOp
    , infixlOp
    , infixrOp
    , prefixOp
    , postfixOp
      -- * Combinators
    , eof
    , token
    , between
    , (<|<)
    , choice
    ) where

import Text.P3.Combinators
import Text.P3.Logic
import Text.P3.Monad
import Text.P3.OperatorParser
import Text.P3.Types

parse :: Token t => ParserTable t -> [t] -> Either String (Syntax t)
parse tbl = runParser tbl (parseLeading <* eof)
