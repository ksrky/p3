{-|
Core logic of P3 algorithm.
-}
module Text.P3.Logic
    ( parseLeading
    , parseTrailing
    ) where

import Control.Applicative
import Control.Lens.Combinators
import Control.Monad.Except
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.State
import Data.List                qualified as L
import Text.P3.Monad
import Text.P3.Types

longestMatch :: [Parser t] -> ParserM t ()
longestMatch parsers = do
    ctx <- ask
    st <- get
    sts <- liftParserM $ tryParsers parsers ctx st
    case L.sortOn (negate . view position) sts of
        []     -> throwError NoMatchParsers
        st': _ -> put st'

tryParsers :: [Parser t] -> ParserContext t -> ParserState t -> Except Exception [ParserState t]
tryParsers parsers c s = observeAllT $ foldr (\p -> (lift (p c s) `catchError` const empty <|>)) empty parsers

parseLeading :: Token t => ParserM t ()
parseLeading = do
    tok <- nextToken
    (longestMatch =<< leadingParsersOf tok)
        `catchError` \case
            NoMatchParsers | isAtomic tok -> mkAtom tok
            e -> throwError e
    parseTrailing

parseTrailing :: Token t => ParserM t ()
parseTrailing = do
    (longestMatch =<< trailingParsersOf =<< peekToken)
        `catchError` (\case
            NoMatchParsers -> return ()
            e -> throwError e)
    `catchError` (\_ -> return ())
