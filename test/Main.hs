{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Test.Hspec
import Text.P3.Combinators
import Text.P3.Logic
import Text.P3.Monad
import Text.P3.OperatorParser
import Text.P3.TH

parserTbl :: ParserTable String
parserTbl = mkParserTable [syntaxs|
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
    IfThenElse  "if" :20 "then" :20 "else" :20
    IfThen      "if" :20 "then" :20
    App         :100 "(" :0 ")"
|]

parseStrings :: [String] -> IO String
parseStrings inp = case runParser parserTbl (parseLeading <* eof) inp of
    Left msg  -> fail $ show msg
    Right stx -> return $ show stx 

spec :: SpecWith ()
spec = do
    describe "Token is a string" $ do
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
        it "if 1 then if 2 then 3 else 4" $ do
            parseStrings ["if", "1", "then", "if", "2", "then", "3", "else", "4"]
                `shouldReturn` "IfThen [\"1\", IfThenElse [\"2\", \"3\", \"4\"]]"
        it "x, y, z" $ do
            parseStrings ["x", ",", "y", ",", "z"] `shouldReturn` "Pair [\"x\", Pair [\"y\", \"z\"]]"
        it "1 - -2" $ do
            parseStrings ["1", "-", "-", "2"] `shouldReturn` "Sub [\"1\", Neg [\"2\"]]"
        it "()" $ do
            parseStrings ["(", ")"] `shouldReturn` "Unit []"
        it "a[1][2]" $ do
            parseStrings ["a", "[", "1", "]", "[", "2", "]"] `shouldReturn` "Subscript [Subscript [\"a\", \"1\"], \"2\"]"
        it "f(x)" $ do
            parseStrings ["f", "(", "x", ")"] `shouldReturn` "App [\"f\", \"x\"]"
        it "f(x, y)" $ do
            parseStrings ["f", "(", "x", ",", "y", ")"] `shouldReturn` "App [\"f\", Pair [\"x\", \"y\"]]"

main :: IO ()
main = hspec spec
