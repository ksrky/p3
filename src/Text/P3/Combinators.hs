{-|
Utilities for writing parser combinators in P3.
-}
module Text.P3.Combinators
    ( eof
    , token
    , between
    , (<|<)
    , choice
    ) where

import Text.P3.Monad
import Text.P3.Types

-- | Check if there's no more token.
eof :: Parser t
eof _ st | null (tokens st) = st
         | otherwise = st{errorMsg = Just "Unexpected the end of token stream"}

-- | Consume a given token.
token :: Eq t => t -> Parser t
token t = matchToken (Token t ==)

between :: Eq t => t -> t -> Parser t -> Parser t
between open close p = token open >>> p >>> token close

(<|<) :: Parser t -> Parser t -> Parser t
(<|<) p1 p2 ctx st = case p1 ctx st of
    st' | hasError st' -> p2 ctx st'
        | otherwise    -> st'

choice :: [Parser t] -> Parser t
choice = foldr (<|<) (const id)
