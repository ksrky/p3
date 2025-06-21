{-# LANGUAGE DerivingVia     #-}

{-|
Contexts, states and monads for P3 parser.
-}
module Text.P3.Monad
    ( -- * Data types for parser monad
      -- ** Parser context
      ParserContext (..)
    , initParserContext
      -- ** Parser state
    , ParserState (..)
    , initParserState
    , hasError
    , recoverError
    , throwError
      -- ** Parser table
    , ParserTable (..)
    , initParserTable
    , insertLeadingParser
    , insertTrailingParser
    , leadingParsersOf
    , trailingParsersOf
    , Parser
    , (>>>)
    , runParser
    , shift
    , matchToken
    , mkAtom
    , mkNode
    ) where

import Data.Map.Strict qualified as M
import Text.P3.Types

-- | Reader context for parser.
data ParserContext t = ParserContext
    { bindingPower :: BindingPower  -- ^ Current binding power.
    , parserTable  :: ParserTable t -- ^ Parser table indexed by tokens.
    }

initParserContext :: ParserContext t
initParserContext = ParserContext
    { bindingPower = minBound
    , parserTable  = initParserTable
    }

-- | State for parser.
data ParserState t = ParserState
    { stxStack :: SyntaxStack t -- ^ Stored parse results.
    , peek     :: Tok t         -- ^ Next token to be processed. `Nothing` means the end of the token stream.
    , tokens   :: [Tok t]       -- ^ Input token stream.
    , position :: Int           -- ^ Position in the token stream.
    , errorMsg :: Maybe String  -- ^ Error message, if any.
    }

initParserState :: [t] -> ParserState t
initParserState ts = ParserState
    { stxStack = []
    , peek
    , tokens
    , position = 0
    , errorMsg = Nothing
    }
  where
    (peek, tokens) = case ts of
        []     -> (Terminator, [])
        x : xs -> (Token x, map Token xs ++ [Terminator])

hasError :: ParserState t -> Bool
hasError st = errorMsg st /= Nothing

recoverError :: ParserState t -> ParserState t
recoverError st = st{errorMsg = Nothing}

throwError :: String -> ParserState t -> ParserState t
throwError msg st = st{errorMsg = Just msg}

type Parser t = ParserContext t -> ParserState t -> ParserState t

infixl 9 >>>

(>>>) :: Parser t -> Parser t -> Parser t
(>>>) p1 p2 ctx = p2 ctx . p1 ctx

-- | A table containing leading and trailing parsers indexed by tokens.
data ParserTable t = ParserTable
    { leadingParsers  :: M.Map t [Parser t]
    , trailingParsers :: M.Map t [Parser t]
    }

initParserTable :: ParserTable t
initParserTable = ParserTable
    { leadingParsers  = M.empty
    , trailingParsers = M.empty
    }

insertLeadingParser :: Token t => t -> Parser t -> ParserTable t -> ParserTable t
insertLeadingParser t p tbl = tbl{leadingParsers = M.insertWith (++) t [p] (leadingParsers tbl)}

insertTrailingParser :: Token t => t -> Parser t -> ParserTable t -> ParserTable t
insertTrailingParser t p tbl = tbl{trailingParsers = M.insertWith (++) t [p] (trailingParsers tbl)}

leadingParsersOf :: Token t => ParserContext t -> Tok t -> [Parser t]
leadingParsersOf ctx (Token t) = concat $ leadingParsers (parserTable ctx) M.!? t
leadingParsersOf _ Terminator = []

trailingParsersOf :: Token t => ParserContext t -> Tok t -> [Parser t]
trailingParsersOf ctx (Token t) = concat $ trailingParsers (parserTable ctx) M.!? t
trailingParsersOf _ Terminator = []

runParser :: ParserTable t -> Parser t -> [t] -> Either String (Syntax t)
runParser tbl parser ts =
    let st = parser initParserContext{parserTable = tbl} (initParserState ts) in
    case (errorMsg st, stxStack st) of
        (Just msg, _)    -> Left msg
        (Nothing, [stx]) -> Right stx
        (Nothing, _)     -> error "runParser: invalid sytax stack"

-- | Update the current token and position in the parser state.
shift :: Parser t
shift _ st = do 
    case tokens st of
        x : xs -> st{peek = x, tokens = xs, position = position st + 1}
        []     -> st

-- | Match the current token with a predicate and shift the state if it matches.
matchToken :: (Tok t -> Bool) -> Parser t
matchToken p ctx st = do 
    if p (peek st)
        then shift ctx st
        else st{errorMsg = Just "No match parsers"}

-- | Push a syntax node to the syntax stack.
pushSyntax :: Syntax t -> ParserState t -> ParserState t
pushSyntax stx st = st{stxStack = stx : stxStack st}

-- | Push `Atom` to the syntax stack.
mkAtom :: Tok t -> Parser t
mkAtom (Token t) _ = pushSyntax (Atom t)
mkAtom Terminator _ = id

-- | Push `Node` to the syntax stack. Reduce operation.
mkNode :: Name -> Int -> ParserState t -> ParserState t
mkNode _ n _ | n < 0 = error "mkNode: negative arity"
mkNode name n st | n > length (stxStack st) = st{errorMsg = Just $ "Not enough syntax stack for " ++ show name}
mkNode name n st = 
    let (stxs, rest) = splitAt n (stxStack st) in
    st{stxStack = Node name (reverse stxs) : rest}
