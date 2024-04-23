module P3.Init
    ( runParser
    , ParserEntry (..)
    , MkParserEntry (..)
    , insertParserEntry
    , initParserTable
    , initParserCatTable
    ) where

import Control.Lens.At
import Control.Lens.Operators
import Control.Monad.Except
import Control.Monad.Reader.Class
import Control.Monad.Writer
import Data.IntMap                qualified as IM
import Data.List                  qualified as L
import Data.Map.Strict            qualified as M
import P3.Monad
import P3.Types

initParserContext :: ParserContext t
initParserContext = ParserContext
    { _parserCat = 0
    , _bindPow = 0
    , _reservedWords = []
    }

initParserState :: [t] -> ParserState t
initParserState toks = ParserState
    { _stxStack = []
    , _tokens = toks
    , _position = 0
    }

runParser :: Monad m => Parser t m -> [t] -> m (Either String Syntax)
runParser parser toks = runExceptT (parser initParserContext (initParserState toks)) <&> \case
    Left e -> Left $ show e
    Right s
        | [stx] <- s ^. stxStack -> Right stx
        | otherwise -> Left "runParser: invalid sytax stack"

data ParserEntry t m
    = LeadingEntry t (Parser t m)
    | TrailingEntry t (Parser t m)
    | TerminalEntry (t -> Parser t m)
    | UnindexedEntry (Parser t m)

partitionEntries :: [ParserEntry t m] -> ([(t, Parser t m)], [(t, Parser t m)], [t -> Parser t m], [Parser t m])
partitionEntries pes = execWriter $ forM pes $ \case
    LeadingEntry t p -> tell ([(t, p)], [], [], [])
    TrailingEntry t p -> tell ([], [(t, p)], [], [])
    TerminalEntry p -> tell ([], [], [p], [])
    UnindexedEntry p -> tell ([], [], [], [p])

class MkParserEntry a t | a -> t where
    mkParserEntry :: (Token t, MonadReader e m, HasParserCatTable e t m) => a -> ParserEntry t m

insertParserEntry :: Token t => ParserEntry t m -> ParserTable t m -> ParserTable t m
insertParserEntry (LeadingEntry tok p) = leadingParsers . at tok %~ \case
    Nothing -> Just [p]
    Just ps -> Just $ p : ps
insertParserEntry (TrailingEntry tok p) = trailingParsers . at tok %~ \case
    Nothing -> Just [p]
    Just ps -> Just $ p : ps
insertParserEntry (TerminalEntry p) = terminalParsers %~ (p :)
insertParserEntry (UnindexedEntry p) = unindexedParsers %~ (p :)

initParserTable :: Token t => [ParserEntry t m] -> ParserTable t m
initParserTable entries = do
    let (ldps, trps, tmps, uips) = partitionEntries entries
    ParserTable
        { _leadingParsers   = M.fromList $ groupParsers ldps
        , _trailingParsers  = M.fromList $ groupParsers trps
        , _terminalParsers  = tmps
        , _unindexedParsers = uips
        }

groupParsers :: forall t m. Token t => [(t, Parser t m)] -> [(t, [Parser t m])]
groupParsers = groupSnd . L.sortOn fst
  where
    groupSnd :: [(t, Parser t m)] -> [(t, [Parser t m])]
    groupSnd [] = []
    groupSnd ((k, v) : xs) = (k, v : map snd ys) : groupSnd zs
        where (ys, zs) = span ((k ==) . fst) xs

initParserCatTable :: [ParserTable t m] -> ParserCatTable t m
initParserCatTable = IM.fromList . zip [0..]
