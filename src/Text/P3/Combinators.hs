module Text.P3.Combinators
    ( eof
    , satisfy
    , token
    , choice
    , between
    , optional
    , many
    , some
    , sepBy
    , sepBy1
    , endBy
    , endBy1
    ) where

import Control.Applicative
import Control.Lens.Combinators
import Control.Monad
import Text.P3.Monad

-- | Check if there's no more token.
eof :: ParserM t ()
eof = do
    toks <- use tokens
    case toks of
        [] -> return ()
        _  -> mzero

satisfy :: (t -> Bool) -> ParserM t t
satisfy p = do
    tok <- nextToken
    if p tok
        then return tok
        else empty

token :: Eq t => t -> ParserM t ()
token t = void $ matchToken (t ==)

between :: Eq t => t -> t -> ParserM t a -> ParserM t a
between open close p = do
    token open
    x <- p
    token close
    return x

choice :: [ParserM t a] -> ParserM t a
choice []       = empty
choice [p]      = p
choice (p : ps) = p <|> choice ps

sepBy1 :: Eq t => ParserM t a -> t -> ParserM t [a]
sepBy1 p sep = liftM2 (:) p $ many $ token sep >> p

sepBy :: Eq t => ParserM t a -> t -> ParserM t [a]
sepBy p sep = sepBy1 p sep <|> return []

endBy :: Eq t => ParserM t a -> t -> ParserM t [a]
endBy p sep = many (do x <- p ; _ <- token sep ; return x)

endBy1 :: Eq t => ParserM t a -> t -> ParserM t [a]
endBy1 p sep = some (do x <- p ; _ <- token sep ; return x)
