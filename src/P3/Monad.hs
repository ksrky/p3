{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module P3.Monad
    ( ParserContext (..)
    , bindPow
    , ParserState (..)
    , stxStack
    , tokens
    , Exception (..)
    , ParserExceptM
    , Parser
    , ParserM
    , execParserM
    , liftParserM
    , withBindPow
    , nextToken
    , nextToken_
    , peekToken
    , matchToken
    , pushSyntax
    , popSyntax
    , mkAtom
    , mkNode
    , ParserTable (..)
    , HasParserTable (..)
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

data Exception
    = NoMatchParsers
    | LowerBindingPower
    | TokenUnmatched
    | TokenEOF
    deriving (Eq, Show)

type ParserExceptM t m = ExceptT Exception m

type Parser t m = ParserContext t -> ParserState t -> ParserExceptM t m (ParserState t)

type ParserM t m = ReaderT (ParserContext t) (StateT (ParserState t) (ParserExceptM t m))

execParserM :: Monad m => ParserM t m a -> ParserContext t -> ParserState t -> ParserExceptM t m (ParserState t)
execParserM m = execStateT . runReaderT m

liftParserM :: (Monad m) => ParserExceptM t m a -> ParserM t m a
liftParserM = lift . lift

-- * Operations

-- ** ParserContext

withBindPow :: Monad m => BindingPower -> ParserM t m a -> ParserM t m a
withBindPow bp = local (bindPow .~ bp)

-- ** Tokens

-- | Get the next token and consume it from the token stream.
nextToken :: Monad m => ParserM t m t
nextToken = do
    toks <- use tokens
    case toks of
        x : xs -> tokens .= xs >> return x
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

data ParserTable t m = ParserTable
    { _leadingParsers  :: M.Map t [Parser t m]
    , _trailingParsers :: M.Map t [Parser t m]
    }

makeClassy ''ParserTable

getLeadingParsers :: (MonadReader e m, HasParserTable e t m, Token t) => t -> ParserExceptM t m [Parser t m]
getLeadingParsers tok = do
    ps <- view leadingParsers
    return $ concat $ M.lookup tok ps

getTrailingParsers :: (MonadReader e m, HasParserTable e t m, Token t) => t -> ParserExceptM t m [Parser t m]
getTrailingParsers tok = do
    ps <- view trailingParsers
    return $ concat $ M.lookup tok ps
