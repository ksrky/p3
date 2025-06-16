{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.P3.Monad
    ( ParserContext (..)
    , initParserContext
    , ParserState (..)
    , initParserState
    , Exception (..)
    , Parser
    , ParserM
    , execParserM
    , liftParserM
    , ParserTable (..)
    , HasParserContext (..)
    , HasParserState (..)
    , initParserTable
    , runParser
    , insertLeadingParser
    , insertTrailingParser
    , insertUnindexedParser
    , nextToken
    , nextToken_
    , peekToken
    , matchToken
    , mkAtom
    , mkNode
    , leadingParsers
    , trailingParsers
    , unindexedParsers
    , leadingParsersOf
    , trailingParsersOf
    , getUnindexedParsers
    ) where

import Control.Monad
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Strict          qualified as M
import Data.Semigroup
import Text.P3.Types

-- * Monad

-- ** Parser context

-- | Reader context for parser.
data ParserContext t = ParserContext
    { _bindingPower   :: BindingPower
      -- | Scoped reserved keywords.
    , _reservedTokens :: [t]
    , _parserTable    :: ParserTable t
    }

initParserContext :: ParserContext t
initParserContext = ParserContext
    { _bindingPower   = minBound
    , _reservedTokens = []
    , _parserTable    = initParserTable
    }

-- ** Parser state

data ParserState t = ParserState
    { -- | Stored parse results.
      _stxStack :: SyntaxStack t
      -- | Input token stream.
    , _tokens   :: [t]
      -- | Position in the token stream.
    , _position :: Int
    }

initParserState :: [t] -> ParserState t
initParserState toks = ParserState
    { _stxStack = []
    , _tokens = toks
    , _position = 0
    }

-- ** Parser exceptions

data Exception
    = NoMatchParsers
    | LowerBindingPower
    | TokenUnmatched
    | TokenEOF
    | UmbigiousSyntax
    deriving (Eq, Show)
    deriving Semigroup via Last Exception

instance Monoid Exception where
    mempty = NoMatchParsers


-- ** Parser monad

type Parser t = ParserContext t -> ParserState t -> Except Exception (ParserState t)

type ParserM t = ReaderT (ParserContext t) (StateT (ParserState t)  (Except Exception))

execParserM :: ParserM t a -> ParserContext t -> ParserState t -> Except Exception (ParserState t)
execParserM m = execStateT . runReaderT m

liftParserM :: Except Exception a -> ParserM t a
liftParserM = lift . lift

-- ** Parser table

data ParserTable t = ParserTable
    { _leadingParsers   :: M.Map t [Parser t]
    , _trailingParsers  :: M.Map t [Parser t]
    , _unindexedParsers :: [Parser t]
    }

initParserTable :: ParserTable t
initParserTable = ParserTable
    { _leadingParsers   = M.empty
    , _trailingParsers  = M.empty
    , _unindexedParsers = []
    }

makeClassy ''ParserContext
makeClassy ''ParserState
makeLenses ''ParserTable

consMaybe :: a -> Maybe [a] -> Maybe [a]
consMaybe x Nothing = Just [x]
consMaybe x (Just xs) = Just (x : xs)

insertLeadingParser :: Token t => t -> Parser t -> ParserTable t -> ParserTable t
insertLeadingParser t p = leadingParsers . at t %~ consMaybe p

insertTrailingParser :: Token t => t -> Parser t -> ParserTable t -> ParserTable t
insertTrailingParser t p = trailingParsers . at t %~ consMaybe p

insertUnindexedParser :: Parser t -> ParserTable t -> ParserTable t
insertUnindexedParser p = unindexedParsers %~ (p :)

runParser :: Parser t -> [t] -> Either String (Syntax t)
runParser parser toks = case runExcept (parser initParserContext (initParserState toks)) of
    Left e -> Left $ show e
    Right s
        | [stx] <- s ^. stxStack -> Right stx
        | otherwise -> Left "runParser: invalid sytax stack"

-- ** Token

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

matchToken :: (t -> Bool) -> ParserM t ()
matchToken p = do
    tok <- nextToken
    if p tok
        then return ()
        else throwError TokenUnmatched

-- ** Syntax

-- | Push a syntax node to the syntax stack.
pushSyntax :: Syntax t -> ParserM t ()
pushSyntax stx = stxStack %= (stx :)

-- | Pop a syntax node from the syntax stack.
popSyntax :: ParserM t (Syntax t)
popSyntax = do
    stxs <- use stxStack
    case stxs of
        [] -> throwError TokenEOF
        stx : stxs' -> do
            stxStack .= stxs'
            return stx

-- | Push `Atom` to the syntax stack.
mkAtom :: t -> ParserM t ()
mkAtom = pushSyntax . Atom

-- | Push `Node` to the syntax stack.
mkNode :: Name -> Int -> ParserM t ()
mkNode name = mkNode' []
  where
    mkNode' :: [Syntax t] -> Int -> ParserM t ()
    mkNode' stxs n
        | n <= 0 = pushSyntax $ Node name stxs
        | otherwise = do
            stx <- popSyntax
            mkNode' (stx : stxs) (n - 1)

-- ** Parser Table
 
leadingParsersOf :: Token t => t -> ParserM t [Parser t]
leadingParsersOf tok = views parserTable $ views leadingParsers (concat . M.lookup tok) 

trailingParsersOf :: Token t => t -> ParserM t [Parser t]
trailingParsersOf tok = views parserTable $ views trailingParsers (concat . M.lookup tok)

getUnindexedParsers :: ParserM t [Parser t]
getUnindexedParsers = views parserTable $ view unindexedParsers
