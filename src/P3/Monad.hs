{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingVia #-}

module P3.Monad
    ( ParserContext (..)
    , HasParserContext (..)
    , ParserState (..)
    , HasParserState (..)
    , Exception (..)
    , ParserInnerM
    , Parser
    , ParserM
    , execParserM
    , liftParserM
    , nextToken
    , nextToken_
    , peekToken
    , matchToken
    , eof
    , mkAtom
    , mkNode
    , ParserTable (..)
    , HasParserTable (..)
    , ParserCatTable
    , HasParserCatTable (..)
    , getLeadingParsers
    , getTrailingParsers
    , getTerminalParsers
    , getUnindexedParsers
    ) where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.IntMap              qualified as IM
import Data.Map.Strict          qualified as M
import P3.Types
import Data.Semigroup

-- | Reader context for parser.
data ParserContext t = ParserContext
    { _parserCat     :: ParserCategory
    , _bindPow       :: BindingPower
      -- | Scoped reserved keywords.
    , _reservedToks :: [t]
    }

makeClassy ''ParserContext

data ParserState t = ParserState
    { -- | Stored parse results.
      _stxStack :: SyntaxStack
      -- | Input token stream.
    , _tokens   :: [t]
      -- | Position in the token stream.
    , _position :: Int
    }

makeClassy ''ParserState

data Exception
    = NoMatchParsers
    | LowerBindingPower
    | TokenUnmatched
    | TokenEOF
    deriving (Eq, Show)
    deriving Semigroup via Last Exception

instance Monoid Exception where
    mempty = NoMatchParsers

type ParserInnerM t m = ExceptT Exception m

type Parser t m = ParserContext t -> ParserState t -> ParserInnerM t m (ParserState t)

type ParserM t m = ReaderT (ParserContext t) (StateT (ParserState t) (ParserInnerM t m))

execParserM :: Monad m => ParserM t m a -> ParserContext t -> ParserState t -> ParserInnerM t m (ParserState t)
execParserM m = execStateT . runReaderT m

liftParserM :: Monad m => ParserInnerM t m a -> ParserM t m a
liftParserM = lift . lift

-- * Operations

-- ** Tokens

-- | Get the next token and consume it from the token stream.
nextToken :: Monad m => ParserM t m t
nextToken = do
    toks <- use tokens
    case toks of
        x : xs -> tokens .= xs >> position %= (+ 1) >> return x
        []     -> throwError TokenEOF

-- | `nextToken` but discard the token.
nextToken_ :: Monad m => ParserM t m ()
nextToken_ = void nextToken

-- | Get the next token without consuming it.
peekToken :: Monad m => ParserM t m t
peekToken = do
    toks <- use tokens
    case toks of
        x : _ -> return x
        []    -> throwError TokenEOF

-- | Match the next token with a predicate.
matchToken :: Monad m => (t -> Bool) -> ParserM t m ()
matchToken p = do
    tok <- nextToken
    if p tok
        then return ()
        else throwError TokenUnmatched

-- | Check if there's no more token.
eof :: Monad m => ParserM t m ()
eof = do
    toks <- use tokens
    case toks of
        [] -> return ()
        _  -> mzero

-- ** Syntax

-- | Push a syntax node to the syntax stack.
pushSyntax :: Monad m => Syntax -> ParserM t m ()
pushSyntax stx = stxStack %= (stx :)

-- | Pop a syntax node from the syntax stack.
popSyntax :: Monad m => ParserM t m Syntax
popSyntax = do
    stx <- use $ stxStack . to head
    stxStack %= tail
    return stx

-- | Push `Atom` to the syntax stack.
mkAtom :: (Token t, Monad m) => t -> ParserM t m ()
mkAtom = pushSyntax . Atom . tokenString

-- | Push `Node` to the syntax stack.
mkNode :: Monad m => Name -> Int -> ParserM t m ()
mkNode name = mkNode' []
  where
    mkNode' :: (Monad m) => [Syntax] -> Int -> ParserM t m ()
    mkNode' stxs n
        | n <= 0 = pushSyntax $ Node name stxs
        | otherwise = do
            stx <- popSyntax
            mkNode' (stx : stxs) (n - 1)

-- ** Parser Table

data ParserTable t m = ParserTable
    { _leadingParsers   :: M.Map t [Parser t m]
    , _trailingParsers  :: M.Map t [Parser t m]
    , _terminalParsers  :: [t -> Parser t m]
    , _unindexedParsers :: [Parser t m]
    }

makeClassy ''ParserTable

type ParserCatTable t m = IM.IntMap (ParserTable t m)

class HasParserCatTable e t m | e -> t where
    getParserCatTable :: e -> ParserCatTable t m

instance HasParserCatTable (ParserCatTable t m) t m where
    getParserCatTable = id

getParserTable :: (MonadReader e m, HasParserCatTable e t m) => ParserM t m (Maybe (ParserTable t m))
getParserTable = do
    cat <- view parserCat
    liftParserM $ asks (IM.lookup cat . getParserCatTable)

getLeadingParsers :: (MonadReader e m, HasParserCatTable e t m, Token t) => t -> ParserM t m [Parser t m]
getLeadingParsers tok = do
    mb_tbl <- getParserTable
    let mb_ps = view leadingParsers <$> mb_tbl
    return $ concat $ M.lookup tok =<< mb_ps

getTrailingParsers :: (MonadReader e m, HasParserCatTable e t m, Token t) => t -> ParserM t m [Parser t m]
getTrailingParsers tok = do
    mb_tbl <- getParserTable
    let mb_ps = view trailingParsers <$> mb_tbl
    return $ concat $ M.lookup tok =<< mb_ps

getTerminalParsers :: (MonadReader e m, HasParserCatTable e t m) => t -> ParserM t m [Parser t m]
getTerminalParsers tok = concatMap (\tab -> view terminalParsers tab ?? tok) <$> getParserTable

getUnindexedParsers :: (MonadReader e m, HasParserCatTable e t m) => ParserM t m [Parser t m]
getUnindexedParsers = concatMap (view unindexedParsers) <$> getParserTable
