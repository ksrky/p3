{-# LANGUAGE DeriveLift #-}

{-|
Common types used in the P3 parser.
-}
module Text.P3.Types
    ( Name (..)
    , BindingPower (..)
    , Token (..)
    , Tok (..)
    , Syntax (..)
    , SyntaxStack
    ) where

import Data.List                  qualified as L
import Data.Text                  qualified as T
import Language.Haskell.TH.Syntax (Lift)

-- | Parser name, which can be a node label of the syntax tree.
newtype Name = Name String
    deriving (Eq, Show, Lift)

-- | Each operand has a binding power.
-- min = 0, max = 100
newtype BindingPower = BindingPower Int
    deriving (Eq, Ord, Show, Enum, Lift)

instance Bounded BindingPower where
    minBound = BindingPower 0
    maxBound = BindingPower 100

-- | Tokens are the smallest units in the P3 parser.
class (Show t, Ord t) => Token t where
    -- | Convert a token to a string.
    tokenString :: t -> String
    tokenString = show
    -- | Atomic tokens can be terminals in the syntax tree.
    isAtomic :: t -> Bool

instance Token String where
    tokenString = show
    isAtomic _ = True

instance Token T.Text where
    tokenString = T.unpack
    isAtomic _ = True

data Tok t
    = -- | Input token.
      Token t
    | -- | A special token that indicates the end of the token stream.
      Terminator
    deriving (Eq, Ord, Show, Lift)

-- | Generalized syntax tree.
data Syntax t
    = -- | @Name@ is a node label of the syntax tree and @[Syntax t]@ is its field.
      Node Name [Syntax t]
    | -- |  A terminal in the syntax tree.
      Atom t
    deriving (Eq)

instance Show t => Show (Syntax t) where
    show (Node (Name name) stxs) = name ++ " [" ++ L.intercalate ", " (map show stxs) ++ "]"
    show (Atom t) = show t

type SyntaxStack t = [Syntax t]
