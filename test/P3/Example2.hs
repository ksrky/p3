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
        Neg         "-" :75
        Paren       "(" :0 ")"
        Unit        "(" ")"
        Pair        :11 "," :10
        Or          :30 "||" :31
        And         :35 "&&" :36
        Eq          :50 "==" :50
        Ne          :50 "/=" :50
        Lt          :50 "<" :50
        Le          :50 "<=" :50
        Gt          :50 ">" :50
        Ge          :50 ">=" :50
        Add         :65 "+" :66
        Sub         :65 "-" :66
        Mul         :70 "*" :71
        Div         :70 "/" :71
        Subscript   :100 "[" :0 "]"
        IfThenElse  "if" :30 "then" :30 "else" :30
        IfThen      "if" :30 "then" :30
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

