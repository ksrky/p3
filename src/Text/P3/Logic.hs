{-|
Core logic of P3 algorithm.
-}
module Text.P3.Logic
    ( parseLeading
    , parseTrailing
    ) where

import Data.List                qualified as L
import Text.P3.Monad
import Text.P3.Types

longestMatch :: [Parser t] -> Parser t
longestMatch parsers ctx st =
    let sts = tryParsers parsers ctx st in
    case L.sortOn (negate . position) sts of
        []     -> st{errorMsg = Just "No match parsers"}
        st': _ -> st'

tryParsers :: [Parser t] -> ParserContext t -> ParserState t -> [ParserState t]
tryParsers parsers ctx st = filter (not . hasError) $ map (\p -> p ctx st) parsers

parseLeading :: Token t => Parser t
parseLeading ctx st
    | hasError st2
    , Token t <- tok
    , isAtomic t
    , let st3 = mkAtom tok ctx st2
    = parseTrailing ctx st3
    | otherwise
    = parseTrailing ctx st2
  where
    tok = peek st
    st1 = shift ctx st
    st2 = longestMatch (leadingParsersOf ctx tok) ctx st1

parseTrailing :: Token t => Parser t
parseTrailing ctx st = recoverError $ longestMatch (trailingParsersOf ctx (peek st)) ctx st
