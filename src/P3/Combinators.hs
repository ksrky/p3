module P3.Combinators
    ( longestMatch
    , mkAtom
    , mkNode
    , parseLeading
    , parseTrailing
    , parse
    ) where

import Control.Applicative
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Except
import Control.Monad.Logic
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.List                  qualified as L
import P3.Monad
import P3.Types

longestMatch :: MonadParserErr m => [Parser t m] -> ParserM t m ()
longestMatch parsers = do
    ctx <- ask
    st <- get
    sts <- liftParserM $ observeAllT $ tryParsers parsers ctx st
    when (null sts) $ throwError NoMatchParsers
    let st' = head $ L.sortOn (^. tokens . to length) sts
    put st'

tryParsers :: MonadParserErr m => [Parser t m] -> ParserContext t -> ParserState t -> LogicT m (ParserState t)
tryParsers parsers c s = foldr (\p -> (lift (p c s) `catchError` const empty <|>)) empty parsers

parseLeading :: (Token t, MonadReader e m, HasParserTable e t m, MonadParserErr m) => ParserM t m ()
parseLeading = do
    tok <- nextToken
    parsers <- liftParserM $ getLeadingParsers tok
    longestMatch parsers `catchError` (\_ -> mkAtom tok)
    parseTrailing

parseTrailing :: (Token t, MonadReader e m, HasParserTable e t m, MonadParserErr m) => ParserM t m ()
parseTrailing = do
    tok <- peekToken
    parsers <- liftParserM $ getTrailingParsers tok
    longestMatch parsers
    `catchError` (\_ -> return ())

parse :: (Token t, MonadReader e m, HasParserTable e t m, MonadParserErr m) => Parser t m
parse = execParserM parseLeading
