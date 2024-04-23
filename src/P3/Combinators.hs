module P3.Combinators
    ( longestMatch
    , mkAtom
    , mkNode
    , parseLeading
    , parseTrailing
    , parserTop
    ) where

import Control.Applicative
import Control.Lens.Combinators
import Control.Monad.Except
import Control.Monad.Logic
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.List                  qualified as L
import P3.Monad
import P3.Types

longestMatch :: Monad m => [Parser t m] -> ParserM t m ()
longestMatch parsers = do
    ctx <- ask
    st <- get
    sts <- liftParserM $ tryParsers parsers ctx st
    case L.sortOn (negate . view position) sts of
        []        -> throwError NoMatchParsers
        st' : _ -> put st'

tryParsers :: Monad m => [Parser t m] -> ParserContext t -> ParserState t -> ParserInnerM t m [ParserState t]
tryParsers parsers c s = observeAllT $ foldr (\p -> (lift (p c s) `catchError` const empty <|>)) empty parsers

parseLeading :: (Token t, MonadReader e m, HasParserCatTable e t m) => ParserM t m ()
parseLeading = do
    tok <- nextToken
    (longestMatch =<< getLeadingParsers tok)
        `catchError` (\_ -> longestMatch =<< getTerminalParsers tok)
    parseTrailing

parseTrailing :: (Token t, MonadReader e m, HasParserCatTable e t m) => ParserM t m ()
parseTrailing = do
    tok <- peekToken
    (longestMatch =<< getTrailingParsers tok)
        `catchError` (\_ -> longestMatch =<< getUnindexedParsers)
    `catchError` (\_ -> return ())

parserTop :: (Token t, MonadReader e m, HasParserCatTable e t m) => Parser t m
parserTop = execParserM $ parseLeading <* eof
