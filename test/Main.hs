{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Monad.Except
import Control.Monad.Reader
import P3.Combinators
import P3.Init
import P3.Monad
import P3.Utils.TH
import Test.Hspec

newtype ParserTestM a = ParserTestM
    {unParserTestM :: ReaderT (ParserTable String ParserTestM) (Except Exception) a}
    deriving (Functor, Applicative, Monad, MonadReader (ParserTable String ParserTestM), MonadError Exception, MonadParserErr)

parserTbl :: ParserTable String ParserTestM
parserTbl = initParserTable $ map mkParserEntry [syntaxs|
    Neg         "-" :75
    Paren       "(" :0 ")"
    Unit        "(" ")"
    Pair        :11 "," :10
    Or          :30 "||" :31
    And         :35 "&&" :36
    Eq          :50 "==" :50
    Eq          :50 "/=" :50
    Eq          :50 "<" :50
    Eq          :50 "<=" :50
    Eq          :50 ">" :50
    Eq          :50 ">=" :50
    Add         :65 "+" :66
    Sub         :65 "-" :66
    Mul         :70 "*" :71
    Div         :70 "/" :71
    Subscript   :100 "[" :0 "]"
    IfThenElse  "if" :30 "then" :30 "else" :30
    IfThen      "if" :30 "then" :30
|]

parseStrings :: [String] -> IO String
parseStrings inp = do
    case runExcept $ runReaderT (unParserTestM (runParser parse inp)) parserTbl of
        Left err  -> fail $ show err
        Right stx -> return $ show stx

main :: IO ()
main = hspec $ do
    describe "prefix parser test" $ do
        it "-3" $ do
            parseStrings ["-", "3"] `shouldReturn` "Neg [\"3\"]"
        it "(1 + 2) * 3" $ do
            parseStrings ["(", "1", "+", "2", ")", "*", "3"] `shouldReturn` "Mul [Paren [Add [\"1\", \"2\"]], \"3\"]"
        it "if 1 == 2 then 3 + 4 else 5" $ do
            parseStrings ["if", "1", "==", "2", "then", "3", "+", "4", "else", "5"] `shouldReturn` "IfThenElse [Eq [\"1\", \"2\"], Add [\"3\", \"4\"], \"5\"]"
        it "1 * (x + y)" $ do
            parseStrings ["1", "*", "(", "x", "+", "y", ")"] `shouldReturn` "Mul [\"1\", Paren [Add [\"x\", \"y\"]]]"
        it "x + a[2]" $ do
            parseStrings ["x", "+", "a", "[", "2", "]"] `shouldReturn` "Add [\"x\", Subscript [\"a\", \"2\"]]"
        it "if 1 == 2 then 3 else if 4 then 5 else 6" $ do
            parseStrings ["if", "1", "==", "2", "then", "3", "else", "if", "4", "then", "5", "else", "6"]
                `shouldReturn` "IfThenElse [Eq [\"1\", \"2\"], \"3\", IfThenElse [\"4\", \"5\", \"6\"]]"
        it "x, y, z" $ do
            parseStrings ["x", ",", "y", ",", "z"] `shouldReturn` "Pair [\"x\", Pair [\"y\", \"z\"]]"
        it "1 - -2" $ do
            parseStrings ["1", "-", "-", "2"] `shouldReturn` "Sub [\"1\", Neg [\"2\"]]"
        it "()" $ do
            parseStrings ["(", ")"] `shouldReturn` "Unit []"
        it "a[1][2]" $ do
            parseStrings ["a", "[", "1", "]", "[", "2", "]"] `shouldReturn` "Subscript [Subscript [\"a\", \"1\"], \"2\"]"
