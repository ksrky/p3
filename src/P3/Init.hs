module P3.Init
    ( runParser
    , insertLeadingParser
    , insertTrailingParser
    , insertUnindexedParser
    ) where

import Control.Lens.At
import Control.Lens.Operators
import Control.Monad.Except
import Data.Map.Strict qualified as M
import P3.Monad
import P3.Types

initParserContext :: ParserContext t
initParserContext = ParserContext
    { _bindingPower   = minBound
    , _reservedTokens = []
    , _parserTable    = ParserTable 
        { _leadingParsers   = M.empty
        , _trailingParsers  = M.empty
        , _unindexedParsers = []
        }
    }

initParserState :: [t] -> ParserState t
initParserState toks = ParserState
    { _stxStack = []
    , _tokens = toks
    , _position = 0
    }

runParser :: Parser t -> [t] -> Either String (Syntax t)
runParser parser toks = case runExcept (parser initParserContext (initParserState toks)) of
    Left e -> Left $ show e
    Right s
        | [stx] <- s ^. stxStack -> Right stx
        | otherwise -> Left "runParser: invalid sytax stack"

consMaybe :: a -> Maybe [a] -> Maybe [a]
consMaybe x Nothing = Just [x]
consMaybe x (Just xs) = Just (x : xs)

insertLeadingParser :: Token t => t -> Parser t -> ParserTable t -> ParserTable t
insertLeadingParser t p = leadingParsers . at t %~ consMaybe p

insertTrailingParser :: Token t => t -> Parser t -> ParserTable t -> ParserTable t
insertTrailingParser t p = trailingParsers . at t %~ consMaybe p

insertUnindexedParser :: Parser t -> ParserTable t -> ParserTable t
insertUnindexedParser p = unindexedParsers %~ (p :)
