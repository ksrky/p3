{-# LANGUAGE QuasiQuotes #-}

module P3.Example2 (specEx2) where

import Control.Applicative
import Control.Monad.Reader
import P3.Combinators
import P3.Example2.Lexer
import P3.Example2.TH
import P3.Init
import P3.Monad
import P3.Types             (Name (..))
import Test.Hspec

newtype ParserTestM a = ParserTestM
    {unParserTestM :: Reader (ParserCatTable Token ParserTestM) a}
    deriving (Functor, Applicative, Monad, MonadReader (ParserCatTable Token ParserTestM))

parserTbl :: ParserCatTable Token ParserTestM
parserTbl = initParserCatTable
    [ initParserTable $ map mkParserEntry [syntaxs|
        Neg         "-" exp:75
        Paren       "(" exp:0 ")"
        Unit        "(" ")"
        Pair        exp:11 "," exp:10
        Or          exp:30 "||" exp:31
        And         exp:35 "&&" exp:36
        Eq          exp:50 "==" exp:50
        Ne          exp:50 "/=" exp:50
        Ne          exp:50 "≠" exp:50
        Lt          exp:50 "<" exp:50
        Le          exp:50 "≦" exp:50
        Gt          exp:50 ">" exp:50
        Ge          exp:50 ">=" exp:50
        Ge          exp:50 "≧" exp:50
        Add         exp:65 "+" exp:66
        Sub         exp:65 "-" exp:66
        Mul         exp:70 "*" exp:71
        Div         exp:70 "/" exp:71
        Subscript   exp:100 "[" exp:0 "]"
        If          "if" exp:30 "then" exp:30 "else" exp:30
        If          "if" exp:30 "then" exp:30
        Lam         "\" var:0 "->" exp:10
        Lam         "λ" var:0 "→" exp:10
        Let         "let" var:0 "=" exp:10 "in" exp:10
        |]
        ++ [pTerminal]
    , initParserTable $ map mkParserEntry [syntaxs|
        TyArrow     typ:21 "->" typ:20
        TyArrow     typ:21 "→" typ:20
        TyProd      typ:31 "×" typ:30
        TyList      "[" typ:0 "]"
        TyForall    "∀" var:0 "." typ:0
        |]
    , initParserTable [pVar]
    ]

pTerminal :: ParserEntry Token ParserTestM
pTerminal = do
    let parser tok = execParserM $ case tok of
            Number _ -> mkAtom tok >> mkNode (Name "Int") 1
            Letter _ -> mkAtom tok >> mkNode (Name "Var") 1
            Symbol _ -> empty
    TerminalEntry parser


pVar :: ParserEntry Token ParserTestM
pVar = do
    let parser tok = execParserM $ case tok of
            Letter _ -> mkAtom tok
            _        -> empty
    TerminalEntry parser

{-
instead of using declarative style to define parser, we can write, for example:
@
pLam :: [ParserEntry Token ParserTestM]
pLam = do
    let parser = execParserM $ do
            mkAtom =<< matchToken (\case Letter{} -> True; _ -> False)
            matchToken_ (`elem` [Symbol "->", Symbol "→"])
            withBindPow 10 parseLeading
            mkNode (Name "Lam") 2
    [LeadingEntry (Symbol "\\") parser, LeadingEntry (Symbol "λ") parser]
@
-}

-- TODO: need reserved keywords
{- pApp :: ParserEntry Token ParserTestM
pApp = do
    let parser = execParserM $ do
            l <- withBindPow 100 $ some parseLeading
            mkNode (Name "App") (length l + 1)
    UnindexedEntry parser -}

parseTokens :: [Token] -> IO String
parseTokens inp = case runReader (unParserTestM (runParser parserTop inp)) parserTbl of
    Left err  -> fail $ show err
    Right stx -> return $ show stx

parseInput :: String -> IO String
parseInput = parseTokens . alexScanTokens

specEx2 :: SpecWith ()
specEx2 = do
    describe "Token parser test with lexer" $ do
        it "-3" $ do
            parseInput "-3" `shouldReturn` "Neg [Int [3]]"
        it "(1 + 2) * 3" $ do
            parseInput "(1 + 2) * 3" `shouldReturn` "Mul [Paren [Add [Int [1], Int [2]]], Int [3]]"
        it "if 1 == 2 then 3 + 4 else 5" $ do
            parseInput "if 1 == 2 then 3 + 4 else 5" `shouldReturn` "If [Eq [Int [1], Int [2]], Add [Int [3], Int [4]], Int [5]]"
        it "1 * (x + y)" $ do
            parseInput "1 * (x + y)" `shouldReturn` "Mul [Int [1], Paren [Add [Var [\"x\"], Var [\"y\"]]]]"
        it "x + a[2]" $ do
            parseInput "x + a[2]" `shouldReturn` "Add [Var [\"x\"], Subscript [Var [\"a\"], Int [2]]]"
        it "if 1 == 2 then 3 else if 4 then 5 else 6" $ do
            parseInput "if 1 == 2 then 3 else if 4 then 5 else 6"
                `shouldReturn` "If [Eq [Int [1], Int [2]], Int [3], If [Int [4], Int [5], Int [6]]]"
        it "x, y, z" $ do
            parseInput "x, y, z" `shouldReturn` "Pair [Var [\"x\"], Pair [Var [\"y\"], Var [\"z\"]]]"
        it "1 - -2" $ do
            parseInput "1 - -2" `shouldReturn` "Sub [Int [1], Neg [Int [2]]]"
        it "()" $ do
            parseInput "()" `shouldReturn` "Unit []"
        it "a[1][2]" $ do
            parseInput "a[1][2]" `shouldReturn` "Subscript [Subscript [Var [\"a\"], Int [1]], Int [2]]"
        it "\\x -> x + 1" $ do
            parseInput "\\x -> x + 1" `shouldReturn` "Lam [\"x\", Add [Var [\"x\"], Int [1]]]"
        it "λ x → x + 1" $ do -- space required between λ and x
            parseInput "λ x → x + 1" `shouldReturn` "Lam [\"x\", Add [Var [\"x\"], Int [1]]]"
        it "let x = 5 * 2 in x * 10" $ do
            parseInput "let x = 5 * 2 in x * 10" `shouldReturn` "Let [\"x\", Mul [Int [5], Int [2]], Mul [Var [\"x\"], Int [10]]]"
