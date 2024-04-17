{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module P3.Monad
    ( ParserContext (..)
    , bindPow
    , ParserState (..)
    , stxStack
    , tokens
    , Parser
    , ParserM
    , execParserM
    , liftParserM
    , ParserTable (..)
    , HasParserTable (..)
    , Exception (..)
    , MonadParserErr
    , MonadInner
    , withBindPow
    , nextToken
    , nextToken_
    , peekToken
    , matchToken
    , pushSyntax
    , popSyntax
    , mkAtom
    , mkNode
    , getLeadingParsers
    , getTrailingParsers,
    ) where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Strict          qualified as M
import Data.Vector              qualified as V
import P3.Types

newtype ParserContext t = ParserContext
    { _bindPow :: BindingPower
    }

makeLenses ''ParserContext

data ParserState t = ParserState
    { _stxStack :: SyntaxStack
    , _tokens   :: [t]
    }

makeLenses ''ParserState

type Parser t m = ParserContext t -> ParserState t -> m (ParserState t)

type ParserM t m = ReaderT (ParserContext t) (StateT (ParserState t) m)

execParserM :: (Monad m) => ParserM t m a -> ParserContext t -> ParserState t -> m (ParserState t)
execParserM m = execStateT . runReaderT m

liftParserM :: (Monad m) => m a -> ParserM t m a
liftParserM = lift . lift

type LeadingParser t m = Parser t m
type TrailingParser t m = Parser t m

data ParserTable t m = ParserTable
    { _leadingParsers  :: M.Map t [LeadingParser t m]
    , _trailingParsers :: M.Map t [TrailingParser t m]
    }

makeClassy ''ParserTable

-- * Exceptions

data Exception
    = NoMatchParsers
    | LowerBindingPower
    | TokenUnmatched
    | TokenEOF
    deriving (Eq, Show)

class (MonadError Exception m) => MonadParserErr m

class (MonadReader e m, HasParserTable e t m, MonadError Exception m) => MonadInner e t m

-- * Operations

-- ** ParserContext

withBindPow :: Monad m => BindingPower -> ParserM t m a -> ParserM t m a
withBindPow bp = local (bindPow .~ bp)

-- ** Tokens

-- | Get the next token and consume it from the token stream.
nextToken :: MonadParserErr m => ParserM t m t
nextToken = do
    toks <- use tokens
    case toks of
        x : xs -> tokens .= xs >> return x
        []     -> throwError TokenEOF

-- | `nextToken` but discard the token.
nextToken_ :: MonadParserErr m => ParserM t m ()
nextToken_ = void nextToken

-- | Get the next token without consuming it.
peekToken :: MonadParserErr m => ParserM t m t
peekToken = do
    toks <- use tokens
    case toks of
        x : _ -> return x
        []    -> throwError TokenEOF

-- | Match the next token with a predicate.
matchToken :: MonadParserErr m => (t -> Bool) -> ParserM t m ()
matchToken p = do
    tok <- nextToken
    if p tok
        then return ()
        else throwError TokenUnmatched

-- ** Syntax

-- | Push a syntax node to the syntax stack.
pushSyntax :: Monad m => Syntax -> ParserM t m ()
pushSyntax stx = stxStack %= V.cons stx

-- | Pop a syntax node from the syntax stack.
popSyntax :: Monad m => ParserM t m Syntax
popSyntax = do
    stx <- use $ stxStack . to V.head
    stxStack %= V.tail
    return stx

-- | Push `Atom` to the syntax stack.
mkAtom :: (Token t, Monad m) => t -> ParserM t m ()
mkAtom = pushSyntax . Atom . tokenString

-- | Push a node to the syntax stack.
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

getLeadingParsers ::
    (MonadReader e m, HasParserTable e t m, Token t) =>
    t -> m [LeadingParser t m]
getLeadingParsers tok = do
    ps <- view leadingParsers
    return $ concat $ M.lookup tok ps

getTrailingParsers ::
    (MonadReader e m, HasParserTable e t m, Token t) =>
    t -> m [TrailingParser t m]
getTrailingParsers tok = do
    ps <- view trailingParsers
    return $ concat $ M.lookup tok ps
