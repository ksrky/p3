{-# LANGUAGE QuasiQuotes #-}

module P3.Example2 (specEx2) where

import Control.Monad.Reader
import P3.Combinators
import P3.Example2.Lexer
import P3.Example2.TH
import P3.Init
import P3.Monad
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
        IfThenElse  "if" exp:30 "then" exp:30 "else" exp:30
        IfThen      "if" exp:30 "then" exp:30
        |]
    , initParserTable $ map mkParserEntry [syntaxs|
        TyArrow      typ:21 "->" typ:20
        TyArrow      typ:21 "→" typ:20
        TyProd       typ:31 "×" typ:30
        TyList       "[" typ:0 "]"
        |]
    ]

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
            parseInput "-3" `shouldReturn` "Neg [Number 3]"
        it "(1 + 2) * 3" $ do
            parseInput "(1 + 2) * 3" `shouldReturn` "Mul [Paren [Add [Number 1, Number 2]], Number 3]"
        it "if 1 == 2 then 3 + 4 else 5" $ do
            parseInput "if 1 == 2 then 3 + 4 else 5" `shouldReturn` "IfThenElse [Eq [Number 1, Number 2], Add [Number 3, Number 4], Number 5]"
        it "1 * (x + y)" $ do
            parseInput "1 * (x + y)" `shouldReturn` "Mul [Number 1, Paren [Add [Atom \"x\", Atom \"y\"]]]"
        it "x + a[2]" $ do
            parseInput "x + a[2]" `shouldReturn` "Add [Atom \"x\", Subscript [Atom \"a\", Number 2]]"
        it "if 1 == 2 then 3 else if 4 then 5 else 6" $ do
            parseInput "if 1 == 2 then 3 else if 4 then 5 else 6"
                `shouldReturn` "IfThenElse [Eq [Number 1, Number 2], Number 3, IfThenElse [Number 4, Number 5, Number 6]]"
        it "x, y, z" $ do
            parseInput "x, y, z" `shouldReturn` "Pair [Atom \"x\", Pair [Atom \"y\", Atom \"z\"]]"
        it "1 - -2" $ do
            parseInput "1 - -2" `shouldReturn` "Sub [Number 1, Neg [Number 2]]"
        it "()" $ do
            parseInput "()" `shouldReturn` "Unit []"
        it "a[1][2]" $ do
            parseInput "a[1][2]" `shouldReturn` "Subscript [Subscript [Atom \"a\", Number 1], Number 2]"
