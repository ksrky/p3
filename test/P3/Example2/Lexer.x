{
module P3.Example2.Lexer
    ( Token (..)
    , alexScanTokens
    , mkToken
    ) where

import Language.Haskell.TH.Syntax (Lift)
import P3.Types qualified
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$greek = [α-ωΑ-Ω]
@letter_start = $alpha | $greek | [℀-⅏] | _
@letter_rest = @letter_start | $digit
@letter = @letter_start @letter_rest*
@surrounder_start = [\(\[\{]
@surrounder_end = [\)\]\}]
@surrounder = @surrounder_start+ | @surrounder_end+
@symbol = [\!-\'\*-\/\:-\@\\\^-\`\|\~¡-ÿ←-⇿∀-⋿⟰-⟿⤀-⥿⦀-⧿⨀-⫿]+

tokens :-

$white+                        ;
"--".*                         ;
@letter                        { \s -> Letter s }
@surrounder                    { \s -> Symbol s }
@symbol                        { \s -> Symbol s }
$digit+                        { \s -> Number (read s) }

{
data Token
    = Letter String
    | Symbol String
    | Number Int
    deriving (Eq, Ord, Show, Lift)

instance P3.Types.Token Token

mkToken :: String -> Token
mkToken s = do
    case alexScanTokens s of
        [] -> error "No tokens found"
        [t] -> t
        ts -> error $ "Multiple tokens found: " ++ show ts
}
