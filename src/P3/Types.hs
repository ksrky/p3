{-# LANGUAGE DeriveLift #-}

module P3.Types
    ( Name (..)
    , BindingPower
    , ParserCategory
    , Token (..)
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
type BindingPower = Int

-- | Parser category Id.
type ParserCategory = Int

-- * Token

class (Show t, Ord t) => Token t where
    tokenString :: t -> String
    tokenString = show

instance Token String where
    tokenString = show

instance Token T.Text where
    tokenString = T.unpack

-- * Syntax

-- | Generalized AST.
data Syntax
    = -- | @Name@ corresponds to a data constructor of the AST
      -- and @[Syntax]@ is its field.
      Node Name [Syntax]
    | -- | Identifier or literal.
      Atom String
    deriving (Eq)

instance Show Syntax where
    show (Node (Name name) stxs) = name ++ " [" ++ L.intercalate ", " (map show stxs) ++ "]"
    show (Atom str) = str

type SyntaxStack = [Syntax]
