{-# LANGUAGE QuasiQuotes #-}

module P3.Example2 (specEx2) where

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
        IfThenElse  "if" exp:30 "then" exp:30 "else" exp:30
        IfThen      "if" exp:30 "then" exp:30
        |]
        ++ pLam
        ++ [pLet]
    , initParserTable $ map mkParserEntry [syntaxs|
        TyArrow      typ:21 "->" typ:20
        TyArrow      typ:21 "→" typ:20
        TyProd       typ:31 "×" typ:30
        TyList       "[" typ:0 "]"
        |]
    ]

pVar :: ParserM Token ParserTestM ()
pVar = mkAtom =<< matchToken (\case Letter{} -> True; _ -> False)

pLam :: [ParserEntry Token ParserTestM]
pLam = do
    let parser = execParserM $ do
            pVar
            matchToken_ (`elem` [Symbol "->", Symbol "→"])
            withParserCat 0 $ withBindPow 10 parseLeading
            mkNode (Name "Lam") 2
    [Left (Symbol "\\", parser), Left (Symbol "λ", parser)]

pLet :: ParserEntry Token ParserTestM
pLet = do
    let parser = execParserM $ do
            pVar
            matchToken_ (Symbol "=" ==)
            withParserCat 0 $ withBindPow 10 parseLeading
            matchToken_ (Letter "in" ==)
            withParserCat 0 $ withBindPow 10 parseLeading
            mkNode (Name "Let") 3
    Left (Letter "let", parser)

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
            parseInput "1 * (x + y)" `shouldReturn` "Mul [Number 1, Paren [Add [Letter \"x\", Letter \"y\"]]]"
        it "x + a[2]" $ do
            parseInput "x + a[2]" `shouldReturn` "Add [Letter \"x\", Subscript [Letter \"a\", Number 2]]"
        it "if 1 == 2 then 3 else if 4 then 5 else 6" $ do
            parseInput "if 1 == 2 then 3 else if 4 then 5 else 6"
                `shouldReturn` "IfThenElse [Eq [Number 1, Number 2], Number 3, IfThenElse [Number 4, Number 5, Number 6]]"
        it "x, y, z" $ do
            parseInput "x, y, z" `shouldReturn` "Pair [Letter \"x\", Pair [Letter \"y\", Letter \"z\"]]"
        it "1 - -2" $ do
            parseInput "1 - -2" `shouldReturn` "Sub [Number 1, Neg [Number 2]]"
        it "()" $ do
            parseInput "()" `shouldReturn` "Unit []"
        it "a[1][2]" $ do
            parseInput "a[1][2]" `shouldReturn` "Subscript [Subscript [Letter \"a\", Number 1], Number 2]"
        it "\\x -> x + 1" $ do
            parseInput "\\x -> x + 1" `shouldReturn` "Lam [Letter \"x\", Add [Letter \"x\", Number 1]]"
        it "let x = 5 * 2 in x * 10" $ do
            parseInput "let x = 5 * 2 in x * 10" `shouldReturn` "Let [Letter \"x\", Mul [Number 5, Number 2], Mul [Letter \"x\", Number 10]]"
