module P3.Combinators
    ( nextToken
    , nextToken_
    , peekToken
    , matchToken
    , eof
    ) where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Except
import P3.Monad
import Control.Monad

-- | Get the next token and consume it from the token stream.
nextToken :: ParserM t t
nextToken = do
    toks <- use tokens
    case toks of
        x : xs -> tokens .= xs >> position %= (+ 1) >> return x
        []     -> throwError TokenEOF

-- | `nextToken` but discard the token.
nextToken_ :: ParserM t ()
nextToken_ = void nextToken

-- | Get the next token without consuming it.
peekToken :: ParserM t t
peekToken = do
    toks <- use tokens
    case toks of
        x : _ -> return x
        []    -> throwError TokenEOF

-- | Match the next token with a predicate.
matchToken :: (t -> Bool) -> ParserM t ()
matchToken p = do
    tok <- nextToken
    if p tok
        then return ()
        else throwError TokenUnmatched

-- | Check if there's no more token.
eof :: ParserM t ()
eof = do
    toks <- use tokens
    case toks of
        [] -> return ()
        _  -> mzero
