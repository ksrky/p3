{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}

module P3.Monad
    ( ParserContext (..)
    , HasParserContext (..)
    , ParserState (..)
    , HasParserState (..)
    , Exception (..)
    , Parser
    , ParserM
    , execParserM
    , liftParserM
    , mkAtom
    , mkNode
    , ParserTable (..)
    , leadingParsers
    , trailingParsers
    , ParserSpec (..)
    , leadingParsersOf
    , trailingParsersOf
    ) where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Strict          qualified as M
import Data.Semigroup
import P3.Types

-- | Reader context for parser.
data ParserContext t = ParserContext
    { _bindingPower   :: BindingPower
      -- | Scoped reserved keywords.
    , _reservedTokens :: [t]
    , _parserTable    :: ParserTable t
    }


data ParserState t = ParserState
    { -- | Stored parse results.
      _stxStack :: SyntaxStack t
      -- | Input token stream.
    , _tokens   :: [t]
      -- | Position in the token stream.
    , _position :: Int
    }

data Exception
    = NoMatchParsers
    | LowerBindingPower
    | TokenUnmatched
    | TokenEOF
    | UnknownParserCategory
    deriving (Eq, Show)
    deriving Semigroup via Last Exception

instance Monoid Exception where
    mempty = NoMatchParsers

type Parser t = ParserContext t -> ParserState t -> Except Exception (ParserState t)

type ParserM t = ReaderT (ParserContext t) (StateT (ParserState t)  (Except Exception))

execParserM :: ParserM t a -> ParserContext t -> ParserState t -> Except Exception (ParserState t)
execParserM m = execStateT . runReaderT m

liftParserM :: Except Exception a -> ParserM t a
liftParserM = lift . lift

data ParserTable t = ParserTable
    { _leadingParsers  :: M.Map t [Parser t]
    , _trailingParsers :: M.Map t [Parser t]
    }

makeClassy ''ParserContext
makeClassy ''ParserState
makeLenses ''ParserTable

class ParserSpec f where
    insertParser :: Token t => f t -> ParserTable t -> ParserTable t

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
