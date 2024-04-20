{-# LANGUAGE TemplateHaskell #-}

module P3.Monad
    ( ParserContext (..)
    , parserCat
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
    , withParserCat
    , withBindPow
    , nextToken
    , nextToken_
    , peekToken
    , matchToken
    , matchToken_
    , pushSyntax
    , popSyntax
    , mkAtom
    , mkNode
    , ParserTable (..)
    , HasParserTable (..)
    , ParserCatTable
    , HasParserCatTable (..)
    , getLeadingParsers
    , getTrailingParsers,
    ) where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.IntMap              qualified as IM
import Data.Map.Strict          qualified as M
import P3.Types

data ParserContext t = ParserContext
    { _parserCat :: ParserCategory
    , _bindPow   :: BindingPower
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

withParserCat :: Monad m => ParserCategory -> ParserM t m a -> ParserM t m a
withParserCat cat = local (parserCat .~ cat)

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

-- | Match the next token with a predicate. Returns the token if matched.
matchToken :: Monad m => (t -> Bool) -> ParserM t m t
matchToken p = do
    tok <- nextToken
    if p tok
        then return tok
        else throwError TokenUnmatched

-- | `matchToken` but discard the token.
matchToken_ :: Monad m => (t -> Bool) -> ParserM t m ()
matchToken_ p = void $ matchToken p

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

type ParserCatTable t m = IM.IntMap (ParserTable t m)

class HasParserCatTable e t m | e -> t where
    getParserCatTable :: e -> ParserCatTable t m

instance HasParserCatTable (ParserCatTable t m) t m where
    getParserCatTable = id

getLeadingParsers :: (MonadReader e m, HasParserCatTable e t m, Token t) => t -> ParserM t m [Parser t m]
getLeadingParsers tok = do
    cat <- view parserCat
    mb_tbl <- liftParserM $ asks (IM.lookup cat . getParserCatTable)
    let mb_ps = view leadingParsers <$> mb_tbl
    return $ concat $ M.lookup tok =<< mb_ps

getTrailingParsers :: (MonadReader e m, HasParserCatTable e t m, Token t) => t -> ParserM t m [Parser t m]
getTrailingParsers tok = do
    cat <- view parserCat
    mb_tbl <- liftParserM $ asks (IM.lookup cat . getParserCatTable)
    let mb_ps = view trailingParsers <$> mb_tbl
    return $ concat $ M.lookup tok =<< mb_ps
