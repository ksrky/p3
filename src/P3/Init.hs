{-# LANGUAGE FunctionalDependencies #-}

module P3.Init
    ( runParser
    , ParserEntry
    , MkParserEntry (..)
    , insertParserEntry
    , initParserTable
    ) where

import Control.Lens.At
import Control.Lens.Operators
import Control.Monad.Except
import Control.Monad.Reader.Class
import Data.Either
import Data.List                  qualified as L
import Data.Map.Strict            qualified as M
import Data.Vector                qualified as V
import P3.Monad
import P3.Types

initParserContext :: ParserContext t
initParserContext = ParserContext
    { _bindPow = 0
    }

mkParserState :: [t] -> ParserState t
mkParserState toks = ParserState
    { _stxStack = V.empty
    , _tokens = toks
    }

runParser :: Monad m => Parser t m -> [t] -> m (Either Exception Syntax)
runParser parser toks = runExceptT (parser initParserContext (mkParserState toks)) >>= \case
    Left err -> return $ Left err
    Right s -> return $ Right $ V.head $ s ^. stxStack

type ParserEntry t m = Either (t, Parser t m) (t, Parser t m)

class MkParserEntry a t | a -> t where
    mkParserEntry :: (Token t, MonadReader e m, HasParserTable e t m) => a -> ParserEntry t m

insertParserEntry :: Token t => ParserEntry t m -> ParserTable t m -> ParserTable t m
insertParserEntry (Left (tok, p)) = leadingParsers . at tok %~ \case
    Nothing -> Just [p]
    Just ps -> Just $ p : ps
insertParserEntry (Right (tok, p)) = trailingParsers . at tok %~ \case
    Nothing -> Just [p]
    Just ps -> Just $ p : ps

initParserTable :: Token t => [ParserEntry t m] -> ParserTable t m
initParserTable entries = do
    let (lps, tps) = partitionEithers entries
    ParserTable
        { _leadingParsers = M.fromList $ groupParsers lps
        , _trailingParsers = M.fromList $ groupParsers tps
        }

groupParsers :: forall t m. Token t => [(t, Parser t m)] -> [(t, [Parser t m])]
groupParsers = groupSnd . L.sortOn fst
  where
    groupSnd :: [(t, Parser t m)] -> [(t, [Parser t m])]
    groupSnd [] = []
    groupSnd ((k, v) : xs) = (k, v : map snd ys) : groupSnd zs
        where (ys, zs) = span ((k ==) . fst) xs
