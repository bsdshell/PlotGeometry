{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE MultiWayIf       #-}

module AronToken where
-- Sat 10 Dec 14:18:31 2022 
-- BUG: There is bug between CPP and Text.RawString.QQ 
-- {-# LANGUAGE CPP #-}
--
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DuplicateRecordFields #-}
-- import Turtle
-- echo "turtle"

-- import Data.Set   -- collide with Data.List 
-- import Control.Monad
-- import Data.Typeable (typeOf) -- runtime type checker, typeOf "k"
-- import qualified Data.List as L
-- import Data.List.Split
-- import Data.Time
-- import Data.Time.Clock.POSIX
import Data.Char
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Posix.Files
import System.Posix.Unistd
import Text.Printf
import Text.Format  
import Control.Monad (unless, when, join)
-- import System.Process
-- import Text.Read
-- import Text.Regex
-- import Text.Regex.Base
-- import Text.Regex.Base.RegexLike
-- import Text.Regex.Posix
-- import Data.IORef 
-- import Control.Concurrent 

-- import qualified Text.Regex.TDFA as TD


import AronModule hiding (CodeBlock(..))
-- import AronHtml
-- import GenePDFHtmlLib
-- import WaiLib 
-- import WaiConstant

-- import qualified Diagrams.Prelude as DIA
-- import Diagrams.Backend.SVG.CmdLine
-- import Diagrams.TrailLike
import Diagrams.Prelude hiding (pre, blue, trim, getSub)
import Diagrams.Backend.SVG.CmdLine
import Text.RawString.QQ
import qualified System.Console.Pretty as SCP


-- KEY: haskell token, shell highlight, command highlight, color highlight, highlight shell

isOp :: Char -> Bool
isOp c = elem c ['+', '-', '*', '/']

isSpaceX :: Char -> Bool
isSpaceX c = c == ' '

isLetterDigit :: Char -> Bool
isLetterDigit c = isLetter c || isDigit c

{-|
    === 

    @
        ab12- => (ab12, -)

        "" "ab12-"

        "a" "b12-"
        "ab" "12-"
        "ab1" "2-"
        "ab12" "-"

    @
-}
alnums :: String -> (String, String)
alnums s = getAlphaNum2 [] s 
                           

alphaStr :: String -> String -> (String, String)
alphaStr a []     = (a, [])
alphaStr a (c:cx) | isLetter c = let (a', cx') = alphaStr a cx
                                 in (c:a', cx')
                  | otherwise = (a, c:cx)


alphaDigitStr :: String -> String -> (String, String)
alphaDigitStr a [] = (a, [])
alphaDigitStr a (c:cx) | isLetter c || isDigit c = let (a', cx') = alphaDigitStr a cx
                                                   in (c:a', cx')
                       | otherwise = (a, c:cx)

getAlphaNum2 :: String -> String -> (String, String)
getAlphaNum2 a []      = (a, [])
getAlphaNum2 a (c:cx)  | isLetter c  = let (a', cx') = alphaDigitStr a cx 
                                       in (c:a', cx')
                       | otherwise     = (a, c:cx)

{-|

    === Extract digits from a string

    @
        getDigit "" "123abc"   => ("123", "abc")
        getDigit "" "+123"     => ("",    "+123")
        getDigit "KK" "123ab"  => ("123KK", "ab")


                [get "KK" "123ab"
                   c = 1
                   [get "KK" "23ab" 
                      c = 2
                      [get "KK" "3ab"
                        c = 3
                        [get "KK" "ab"
                          c = a
                        ]
                        ("KK", "ab")
                      ]
                      ("3KK" "ab")
                   ]
                   ("23KK" "ab")
               ]
               ("123KK" "ab")
    @ 

            accum     inputStr  (accum, restOfStr)
              ↓         ↓              ↓ 
-}
getDigit :: String -> String -> (String, String)
getDigit a [] = (a, [])
getDigit a (c:cx) | isDigit c = let (c',   cx') = getDigit a cx 
                                in  (c:c', cx')
                  | otherwise = (a, c:cx)

getNegativeInt :: String -> String -> (String, String)
getNegativeInt a [] = (a, [])
getNegativeInt a (c:cx) | c == '-' = let (c', cx') = getDigit a cx
                                     in  c' == [] ? (a, c:cx) $ (c:c', cx')
                        | otherwise = (a, c:cx)
                  
-- Generalize the getAlphaNum and getDigit
spanX::(a -> Bool) -> [a] -> [a] -> ([a], [a])
spanX f a []     = (a, [])
spanX f a (c:cx) | f c = let (a', cx') = spanX f a cx
                         in  (c:a', cx')
                 | otherwise = (a, c:cx)

getWSpace :: String -> String -> (String, String)
getWSpace a s = spanX isSpace a s 


                              
isStrDQ :: (String, String) -> Bool
isStrDQ (a, s) = len a >= 2 && head a == '"' && last a == '"'


getStrDQ :: String -> String -> (String, String)
getStrDQ a []     = (a, [])
getStrDQ a (c:cx) | isDQ c = let (q0, cx') = oneDQ [] (c:cx) 
                                 (a', cy') = nonDQ [] cx' 
                                 (q1, cz') = oneDQ [] cy'
                                 s1 = q0 ++ a' ++ q1
                             in  isStrDQ (s1, cz') ?  (s1 ++ a, cz') $ (a, c:cx)
                  | otherwise = (a, c:cx)

yesDQ :: String -> String -> (String, String)
yesDQ a [] = (a, [])
yesDQ a (c:cx) | isDQ c = let (a', cx') = yesDQ a  cx
                          in (c:a', cx')
               | otherwise = (a, c:cx)

oneDQ :: String -> String -> (String, String)
oneDQ a [] = (a, [])
oneDQ a (c:cx) | isDQ c = (c:a, cx)
               | otherwise = (a, c:cx)


{-|
-}
isStrSQ :: (String, String) -> Bool
isStrSQ (a, s) = len a >= 2 && head a == '\'' && last a == '\''

{-|
-}
isSQ :: Char -> Bool
isSQ c = c == '\''

{-|
-}
oneSQ :: String -> String -> (String, String)
oneSQ a [] = (a, [])
oneSQ a (c:cx) | isSQ c = (c:a, cx)
               | otherwise = (a, c:cx)

{-|
-}
nonSQ :: String -> String -> (String, String)
nonSQ a []  = (a, [])
nonSQ a (c:cx) | (not . isSQ) c = let (a', cx') = nonSQ a cx
                                  in (c:a', cx')
               | otherwise = (a, c:cx)


{-|
    String has no Double Quotes
-}
nonDQ :: String -> String -> (String, String)
nonDQ a [] = (a, [])
nonDQ a (c:cx) | (not . isDQ) c = let (a', cx') = nonDQ a cx
                                  in (c:a', cx')
               | otherwise = (a, c:cx)


{-|
   @ 
     getStrSQ "" 'ab'cdef => ("'ab'", "efg")
     getStrSQ "" "'abc"   => ("",   "'abc")
     getStrSQ "" "''abc"  => ("''",   "'abc")

   @
-}
getStrSQ :: String -> String -> (String, String)
getStrSQ a []     = (a, [])
getStrSQ a (c:cx) | isSQ c = let (q0, cx') = oneSQ [] (c:cx) 
                                 (a', cy') = nonSQ [] cx' 
                                 (q1, cz') = oneSQ [] cy'
                                 s1 = q0 ++ a' ++ q1
                             in  isStrSQ (s1, cz') ? (s1 ++ a, cz') $ (a, c:cx)
                  | otherwise = (a, c:cx)

{-|
    Single Quotes String 
-}
--getStrSQ :: String -> String -> (String, String)
--getStrSQ a s = extractStrSQ a s


isDoubleQuote :: Char -> Bool
isDoubleQuote c = c == '"'

isDQ = isDoubleQuote


getEqualRightArrow :: String -> String -> (String, String)
getEqualRightArrow a []      = (a, [])
getEqualRightArrow a (b:c:cx) | b == '=' && c == '>' = (b:c:a, cx)
                              | otherwise = (a, b:c:cx)
getEqualRightArrow a s  = (a, s)

getTwoColon :: String -> String -> (String, String)
getTwoColon a []      = (a, [])
getTwoColon a (b:c:cx) | b == ':' && c == ':' = (b:c:a, cx)
                       | otherwise = (a, b:c:cx)
getTwoColon a s  = (a, s)

{-|
    Single Quotes String 

    @
    ->
    @
-}
getSubRightArrow :: String -> String -> (String, String)
getSubRightArrow a []      = (a, [])
getSubRightArrow a (b:c:cx) | b == '-' && c == '>' = (b:c:a, cx)
                            | otherwise = (a, b:c:cx)
getSubRightArrow a s  = (a, s)

getTwoEqual :: String -> String -> (String, String)
getTwoEqual a []      = (a, [])
getTwoEqual a (b:c:cx) | b == '=' && c == '=' = (b:c:a, cx)
                       | otherwise = (a, b:c:cx)
getTwoEqual a s  = (a, s)

-- =
getEqual :: String -> String -> (String, String)
getEqual a [] = (a, [])
getEqual a (c:cx) | c == '='   = (c:a, cx)
                  | otherwise  = (a, c:cx)

-- '
getSingleQuote :: String -> String -> (String, String)
getSingleQuote a [] = (a, [])
getSingleQuote a (c:cx) | c == '\'' = (c:a, cx)
                        | otherwise = (a, c:cx)


-- "
getDoubleQuote :: String -> String -> (String, String)
getDoubleQuote a [] = (a, [])
getDoubleQuote a (c:cx) | c == '"' = (c:a, cx)
                        | otherwise = (a, c:cx)

-- /
getForwardslash :: String -> String -> (String, String)
getForwardslash a [] = (a, [])
getForwardslash a (c:cx) | c == '/' = (c:a, cx)
                         | otherwise = (a, c:cx)

-- \ 
getBackslash :: String -> String -> (String, String)
getBackslash a [] = (a, [])
getBackslash a (c:cx) | c == '\\' = (c:a, cx)
                      | otherwise = (a, c:cx)

-- Greater than >
getGT :: String -> String -> (String, String)
getGT a [] = (a, [])
getGT a (c:cx) | c == '>' = (c:a, cx)
               | otherwise = (a, c:cx)      

-- Less than <
getLT :: String -> String -> (String, String)
getLT a [] = (a, [])
getLT a (c:cx) | c == '<' = (c:a, cx)
               | otherwise = (a, c:cx)      

{-|
    === 

    @
    Subtraction: -
    @
-}
getSub :: String -> String -> (String, String)
getSub a [] = (a, [])
getSUb a (c:cx) | c == '-'  = (c:a, cx)
                | otherwise = (a, c:cx)      

{-|
    === Get Right Curly Bracket
-}
getRCB :: String -> String -> (String, String)
getRCB a [] = (a, [])
getRCB a (c:cx) | c == '}' = (c:a, cx)
                | otherwise = (a, c:cx)      
{-|
    === Get Left Curly Bracket
-}
getLCB :: String -> String -> (String, String)
getLCB a [] = (a, [])
getLCB a (c:cx) | c == '{' = (c:a, cx)
                | otherwise = (a, c:cx)      


{-|
    === Get Right Parenthese or Right Round Bracket
-}
getRP :: String -> String -> (String, String)
getRP a [] = (a, [])
getRP a (c:cx) | c == ')' = (c:a, cx)
               | otherwise = (a, c:cx)      
{-|
    === Get Left Parenthese or Left Round Bracket
-}
getLP :: String -> String -> (String, String)
getLP a [] = (a, [])
getLP a (c:cx) | c == '(' = (c:a, cx)
               | otherwise = (a, c:cx)      

{-|
    === Get Right Square Bracket
-}
getRSB :: String -> String -> (String, String)
getRSB a [] = (a, [])
getRSB a (c:cx) | c == ']' = (c:a, cx)
                | otherwise = (a, c:cx)      
{-|
    === Get Left Square Bracket 
-}
getLSB :: String -> String -> (String, String)
getLSB a [] = (a, [])
getLSB a (c:cx) | c == '[' = (c:a, cx)
                | otherwise = (a, c:cx)      


data Token = AlphaNum String
            | TokLP String
            | TokRP String
            | TokLCB String
            | TokRCB String
            | TokLSB String   --  [
            | TokRSB String   --  ]
            | TokGT String 
            | TokLT String 
            | TokEqual String 
            | TokSQ String 
            | TokDQ String 
            | TokColon String 
            | TokForwardslash String
            | TokBackslash String
            | OpAdd String
            | OpSub String
            | OpDiv String
            | OpMul String
            | DQString String
            | SQString String
            | WSpace String
            | SymEqualRightArrow String             --  =>
            | SymTwoColon String                    --  :: 
            | SymSubRightArrow String               --  ->
            | NegativeInt String                    --  -31415
            | SymTwoEqual String                    --  ==
            | Unknown String                        --   
            | NumberX String deriving(Show, Eq)

 
{-|

  @
  let s = "( 1 + 2 = 3 )"
  putStr $ (concat . colorToken) $ tokenize s
  @
-}
tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cx) | isLetter c = let (a, cx') = getAlphaNum2 [] (c:cx)
                                in AlphaNum a : tokenize cx'
                 | isDigit c  = let (a, cx') = getDigit [] (c:cx) 
                                in NumberX a : tokenize cx'
                 | isSpace c  = let (a, cx') = getWSpace [] (c:cx)
                                in WSpace a : tokenize cx'
                 | c == '='   = let (a, cx') = getEqualRightArrow [] (c:cx)
                                in case a of 
                                       var | a /= [] -> SymEqualRightArrow a : tokenize cx'
                                           | otherwise -> let (a, cx') = getTwoEqual [] (c:cx) 
                                                          in case a of
                                                               var | a /= [] -> SymTwoEqual a : tokenize cx'
                                                                   | otherwise -> let (a, cx') = getEqual [] (c:cx) 
                                                                                  in TokEqual a : tokenize cx' 
                                                                 

                 | c == '['   = let (a, cx') = getLSB [] (c:cx)
                                in case a of
                                     var | a /= [] -> TokLSB [c] : tokenize cx
                                         | otherwise -> error "ERROR: Unknow error 3"

                 | c == ']'   = let (a, cx') = getRSB [] (c:cx)
                                in case a of
                                     var | a /= [] -> TokRSB [c] : tokenize cx
                                         | otherwise -> error "ERROR: Unknow error 4"

                 | c == '-'   = let (s, cx') = getSubRightArrow [] (c:cx)
                                in case s of
                                      var | s /= [] -> SymSubRightArrow s : tokenize cx'
                                          | otherwise -> let (b, cy') = getNegativeInt [] (c:cx)
                                                         in case b of
                                                               v | b /= [] -> NegativeInt b : tokenize cy'
                                                                 | otherwise -> OpSub [c] : tokenize cx



                 | c == ':'   = let (a, cx') = getTwoColon [] (c:cx)
                                in case a of 
                                      var | a /= [] -> SymTwoEqual a : tokenize cx'
                                          | otherwise -> TokColon [c] : tokenize cx 

                 | c == '/'   = let (a, cx') = getForwardslash [] (c:cx)
                                in case a of
                                      var | a /= [] -> TokForwardslash a : tokenize cx'
                                          | otherwise -> error "ERROR: Unknown Error 5"

                 | c == '\\'   = let (a, cx') = getBackslash [] (c:cx)
                                in case a of
                                      var | a /= [] -> TokBackslash a : tokenize cx'
                                          | otherwise -> error "ERROR: Unknown Error 6"


                 | c == '<'   = let (a, cx') = getLT [] (c:cx)
                                in TokLT a : tokenize cx'
                 | c == '>'   = let (a, cx') = getGT [] (c:cx)
                                in TokGT a : tokenize cx'
                 | c == '('   = let (a, cx') = getLP [] (c:cx)
                                in TokLP a : tokenize cx'
                 | c == ')'   = let (a, cx') = getRP [] (c:cx)
                                in TokRP a : tokenize cx'
                 | c == '{'   = let (a, cx') = getLCB [] (c:cx)
                                in TokLCB a : tokenize cx'
                 | c == '}'   = let (a, cx') = getRCB [] (c:cx)
                                in TokRCB a : tokenize cx'
                 | c == '"'   = let (a, cx') = getStrDQ [] (c:cx)
                                in  case a of 
                                        var | a /= [] -> DQString a : tokenize cx'
                                            | otherwise -> let (a, cx') = getDoubleQuote [] (c:cx)
                                                           in TokDQ a : tokenize cx'

                 | c == '\''  = let (a, cx') = getStrSQ [] (c:cx)
                                in  case a of 
                                        var | a /= [] -> SQString a : tokenize cx'
                                            | otherwise -> let (a, cx') = getSingleQuote [] (c:cx)
                                                           in TokSQ a : tokenize cx' 

                 | otherwise  = Unknown [c] : tokenize cx 

{-|
  === KEY:

  TODO: refactor the code
  @
  let s = "( 1 + 2 = 3 )"
  putStr $ (concat . colorToken) $ tokenize s
  @

-}
colorToken :: [Token] -> [String]
colorToken [] = []
colorToken ls = map(\t -> case t of
                                AlphaNum s -> case s of
                                                 v | let v' = toLowerStr v in hasStr "error" v' -> col SCP.Red s
                                                   | otherwise    -> s
                                NumberX  s -> colorfgStr 150 s 
                                TokLP s -> col SCP.Green s
                                TokRP s -> col SCP.Green s
                                TokLCB s -> col SCP.Red s
                                TokRCB s -> col SCP.Red s
                                TokLSB s -> col SCP.Green s
                                TokRSB s -> col SCP.Green s
                                TokLT s -> col SCP.Yellow s
                                TokGT s -> col SCP.Yellow s
                                TokColon s -> col SCP.Yellow s
                                TokEqual s -> colorfgStr 100 s
                                TokForwardslash s -> colorfgStr 30 s
                                TokBackslash s -> colorfgStr 170 s
                                TokSQ    s -> colorfgStr 120 s
                                TokDQ    s -> colorfgStr 20 s
                                OpSub    s -> colorfgStr 50 s
                                DQString s -> col SCP.Cyan s
                                SQString s -> col SCP.Blue s
                                WSpace s   -> s
                                SymEqualRightArrow s -> col SCP.Red s
                                SymSubRightArrow   s -> colorfgStr 160 s
                                SymTwoEqual s -> col SCP.Magenta s
                                SymTwoColon s -> col SCP.Red s
                                Unknown s -> s
                                _       -> []
                    ) ls
            where
              col = SCP.color
  

leftPadNum :: [Token] -> [String]
leftPadNum [] = []
leftPadNum ls = map(\t -> case t of
                                AlphaNum s -> case s of
                                                 v | let v' = toLowerStr v in hasStr "error" v' -> col SCP.Red s
                                                   | otherwise    -> s
                                NumberX      s -> let a = replicate (5 - len s) " " in concat a ++ s
                                NegativeInt  s -> let a = replicate (5 - len s) " " in concat a ++ s 
                                TokLP s -> col SCP.Green s
                                TokRP s -> col SCP.Green s
                                TokLCB s -> col SCP.Red s
                                TokRCB s -> col SCP.Red s
                                TokLSB s -> col SCP.Green s
                                TokRSB s -> col SCP.Green s
                                TokLT s -> col SCP.Yellow s
                                TokGT s -> col SCP.Yellow s
                                TokColon s -> col SCP.Yellow s
                                TokEqual s -> colorfgStr 100 s
                                TokForwardslash s -> colorfgStr 30 s
                                TokBackslash s -> colorfgStr 170 s
                                TokSQ    s -> colorfgStr 120 s
                                TokDQ    s -> colorfgStr 20 s
                                OpSub    s -> colorfgStr 50 s
                                DQString s -> col SCP.Cyan s
                                SQString s -> col SCP.Blue s
                                WSpace s   -> s
                                SymEqualRightArrow s -> col SCP.Red s
                                SymSubRightArrow   s -> colorfgStr 160 s
                                SymTwoEqual s -> col SCP.Magenta s
                                SymTwoColon s -> col SCP.Red s
                                Unknown s -> s
                                _       -> []
                    ) ls
            where
              col = SCP.color
  
matchStr :: Int -> [Token] -> [String]
matchStr n [] = []
matchStr n ls = map(\t -> case t of
                                AlphaNum s -> s
                                NumberX     s -> leftPad s n
                                NegativeInt s -> leftPad s n
                                TokLP s -> s
                                TokRP s -> s
                                TokLCB s -> s
                                TokRCB s -> s
                                TokLSB s -> s
                                TokRSB s -> s
                                TokLT s -> s
                                TokGT s -> s
                                TokColon s -> s
                                TokEqual s -> s
                                TokForwardslash s -> s
                                TokBackslash s -> s
                                TokSQ    s -> s
                                TokDQ    s -> s
                                OpSub    s -> s
                                DQString s -> s
                                SQString s -> s
                                WSpace s   -> s
                                SymEqualRightArrow s -> s
                                SymSubRightArrow   s -> s
                                SymTwoEqual s -> s
                                SymTwoColon s -> s
                                Unknown s -> s
                                _       -> []
                    ) ls
            where
              leftPad s n = let a = replicate (n - len s) " " in concat a ++ s

px n x = (putStrLn . concat . matchStr n . tokenize) x

{-|
   === KEY: color string

   USE 'AronToken'

   @
     highlight "{abc}"
   @
-}
highlight :: String -> IO()
highlight s = putStr $ concat lt
  where
    ls = tokenize s 
    lt = colorToken ls
  
{-|
   === KEY: color string

   USE 'AronToken'

   @
     putStr $ highlightStr "{abc}"
   @
-}
highlightStr :: String -> String
highlightStr s = concat lt
  where
    ls = tokenize s 
    lt = colorToken ls 
  

testAll :: IO()
testAll = do

        when True $ do
            let s = "ab12+Casper"
            fw "alnums"
            pre $ ("ab12", "+Casper") == alnums s 


        when True $ do
            let s = "12+Casper"
            fw "spanX"
            pre s
            pre $ ("12", "+Casper") == spanX isDigit "" s 

        when True $ do
            let s = "abc123+Casper"
            fw "spanX"
            pre s
            pre $ ("abc123", "+Casper") == spanX isLetterDigit "" s 

        when True $ do
            let s = "abc123+Casper"
            fw "nonDQ"
            pre s
            pre $ ("abc123+Casper", "") == nonDQ "" s 

        when True $ do
            let s = "abc123+\"Casper"
            fw "nonDQ"
            pre s
            pre $ ("abc123+", "\"Casper") == nonDQ "" s 

        when True $ do
            let s = "ab\"kk"
            fw "yesDQ"
            pre s
            pre $ ("", "ab\"kk") == yesDQ "" s 

        when True $ do
            let s = "\"abc"
            fw "yesDQ"
            pre s
            pre $ ("\"", "abc") == yesDQ "" s 

        when True $ do
            let s = "\"\"abc"
            fw "yesDQ"
            pre s
            pre $ ("\"\"", "abc") == yesDQ "" s 

        when True $ do
            let s = "\"\"\"abc"
            fw "yesDQ"
            pre s
            pre $ ("\"\"\"", "abc") == yesDQ "" s 

        when True $ do
            let s = "abc"
            fw "oneDQ"
            pre s
            pre $ ("", "abc") == oneDQ "" s 

        when True $ do
            let s = "\"abc"
            fw "oneDQ"
            pre s
            pre $ ("\"", "abc") == oneDQ "" s 

        when True $ do
            let s = "\"\"\"abc"
            fw "oneDQ"
            pre s
            pre $ ("\"", "\"\"abc") == oneDQ "" s 

        when True $ do

            let s = "\"\"\"abc"
            fw "oneDQ"
            pre s
            pre $ ("\"KK", "\"\"abc") == oneDQ "KK" s 

        when True $ do
            let s = "\"\"\"abc"
            fw "oneDQ"
            pre s
            pre $ ("\"\"", "\"\"abc") == oneDQ "\"" s  

        when True $ do
            let s = "\"Casper"
            fw "oneDQ"
            pre s
            pre $ ("\"ab", "Casper") == oneDQ "ab" s  

        when True $ do
            let s = "\"ab\"Casper"
            fw "getStrDQ"
            pre s
            pre $ ("\"ab\"", "Casper") == getStrDQ "" s  

        when True $ do
            let s = "\"abCasper"
            fw "getStrDQ"
            pre s
            pre $ ("", "\"abCasper") == getStrDQ "" s  

        when True $ do
            let s = "\"\"abCasper"
            fw "getStrDQ"
            pre s
            pre $ ("\"\"", "abCasper") == getStrDQ "" s  

        when True $ do
            let s = "\"\\\"abCasper"
            fw "getStrDQ"
            pre s
            pre $ ("\"\\\"", "abCasper") == getStrDQ "" s  

        when True $ do
            let s = "\"+\"abCasper"
            fw "getStrDQ"
            pre s
            pre $ ("\"+\"", "abCasper") == getStrDQ "" s  

            
        when True $ do
            let s = " "
            fw "getWSpace"
            pre s
            pre $ (" ", "") == getWSpace "" s 

        when True $ do
            let s = "  "
            fw "getWSpace"
            pre s
            pre $ ("  ", "") == getWSpace "" s 


        when True $ do
            let s = "="
            fw "getEqualRightArrow"
            pre s
            pre $ getEqualRightArrow "" s 

        when True $ do
            let s = "=ab"
            fw "getEqualRightArrow"
            pre s
            pre $ getEqualRightArrow "" s 

        when True $ do
            let s = "=>ab"
            fw "getEqualRightArrow"
            pre s
            pre $ getEqualRightArrow "" s 

        when True $ do
            let s = ">=ab"
            fw "getEqualRightArrow"
            pre s
            pre $ getEqualRightArrow "" s 

        when True $ do
            let s = ">=>=ab"
            fw "getEqualRightArrow"
            pre s
            pre $ getEqualRightArrow "" s 

        when True $ do
            let s = "=>>ab"
            fw "getEqualRightArrow"
            pre s
            pre $ getEqualRightArrow "" s 

        when True $ do
            let s = "123 abc123 => >ab"
            fw "getEqualRightArrow"
            pre s
            pre $ getEqualRightArrow "" s 

        when True $ do
            let s = "12+Casper"
            fw "getDigit"
            let t = getDigit "" s 
            pre t
            pre $ ("12", "+Casper") == t 

        when True $ do
            let s = "123abc"
            fw "getDigit"
            let t = getDigit "KK" s 
            pre t
            pre $ ("123KK", "abc") == t 


        when True $ do
            let s = ">abc"
            fw "getGT"
            pre s
            pre $ (">", "abc") == getGT "" s 

        when True $ do
            let s = ">abc"
            fw "getGT"
            pre s
            let t = getGT "12" s 
            pre t
            pre $ (">12", "abc") == t 

        when True $ do
            let s = ">>abc"
            fw "getGT"
            pre s
            let t = getGT "12" s 
            pre t
            pre $ (">12", ">abc") == t 

        when True $ do
            let s = "<abc"
            fw "getLT"
            pre s
            let t = getLT "12" s 
            pre t
            pre $ ("<12", "abc") == t 

        when True $ do
            let s = "<"
            fw "getLT"
            pre s
            let t = getLT "" s 
            pre t
            pre $ ("<", "") == t 

        when True $ do
            let s = "<ab"
            fw "getLT"
            pre s
            let t = getLT "" s 
            pre t
            pre $ ("<", "ab") == t 

        when True $ do
            let s = "\'"
            fw "isSQ"
            pre s
            let t = isSQ '\''  
            pre t
            pre $ True == t 

        when True $ do
            let s = "\'"
            fw "oneSQ"
            pre s
            let t = oneSQ "" s  
            pre t
            pre $ ("\'", "") == t 

        when True $ do
            let s = "\'abc"
            fw "oneSQ"
            pre s
            let t = oneSQ "" s  
            pre t
            pre $ ("\'", "abc") == t 

        when True $ do
            let s = "\'abc"
            fw "oneSQ"
            pre s
            let t = oneSQ "KK" s  
            pre t
            pre $ ("\'KK", "abc") == t 

        when True $ do
            let s = "abc"
            fw "nonSQ"
            pre s
            let t = nonSQ "" s  
            pre t
            pre $ ("abc", "") == t 

        when True $ do
            let s = "\'abc"
            fw "nonSQ"
            pre s
            let t = nonSQ "" s  
            pre t
            pre $ ("", "\'abc") == t 

        when True $ do
            let s = "abc"
            fw "nonSQ"
            pre s
            let t = nonSQ "KK" s  
            pre t
            pre $ ("abcKK", "") == t 

        when True $ do
            let s = "'ab'Casper"
            fw "getStrSQ"
            pre s
            pre $ ("'ab'", "Casper") == getStrSQ "" s  

        when True $ do
            let s = "'abCasper"
            fw "getStrSQ"
            pre s
            pre $ ("", "'abCasper") == getStrSQ "" s  

        when True $ do
            let s = "''abCasper"
            fw "getStrSQ"
            pre s
            pre $ ("''", "abCasper") == getStrSQ "" s  

        when True $ do
            let s = "'\\'abCasper"
            fw "getStrSQ"
            pre s
            pre $ ("'\\'", "abCasper") == getStrSQ "" s  

        when True $ do
            let s = "'+'abCasper"
            fw "getStrSQ"
            pre s
            pre $ ("'+'", "abCasper") == getStrSQ "" s  

        when True $ do
            let s = "'+'abCasper"
            fw "getStrSQ"
            pre s
            let t = getStrSQ "KK" s
            pre t
            pre $ ("'+'KK", "abCasper") == t 

        when True $ do
            let s = "'+''abCasper"
            fw "getStrSQ"
            pre s
            let t = getStrSQ "KK" s
            pre t
            pre $ ("'+'KK", "'abCasper") == t 

        when True $ do
            let s = "abc123"
            fw "tokenize"
            pre s
            pre $ tokenize s 

        when True $ do
            let s = "123"
            fw "tokenize"
            pre s
            pre $ tokenize s 

        when True $ do
            let s = "abc123 456"
            fw "tokenize"
            pre s
            pre $ tokenize s 

        when True $ do
            let col = SCP.color 
            let s = "=>abc123 456=>>(dog){cat}"
            fw "tokenize"
            pre s
            let ls = tokenize s 
            
            let lt = colorToken ls
            pre lt
            putStrLn $ concat lt

        when True $ do
            let col = SCP.color 
            -- let s = "\"abc\"abc (do){cat}<cow> 'SingleQuote' abc=>> '123\"KK"
            let s = "\"abc\"abc (do){cat}<cow> 'SingleQuote' abc=>> 123KK ==> ab=123"
            fw "tokenize"
            let ls = tokenize s 
            pre ls 
            let lt = colorToken ls 
            pre lt
            pre s
            putStrLn $ concat lt

        when True $ do
            let col = SCP.color 
            let s = "=123\"abc\"\"='456'' 3-4-> x--|: fun::Int -> Int 3.14=>2.71[cow]->{dog} s/Croatia/Portugal/  \\abc\\=>"
            pre s
            fw "tokenize"
            let ls = tokenize s 
            pre ls 
            let lt = colorToken ls 
            pre lt
            putStrLn $ concat lt


        when True $ do
            let s = "a12+Casper"
            fw "alphaStr"
            let t = alphaStr "" s
            pre t  
            print $ ("a", "12+Casper") == t

        when True $ do
            let s = "12+Casper"
            fw "alphaStr"
            let t = alphaStr "" s
            pre t  
            print $ ("", "12+Casper") == t

        when True $ do
            let s = "ab12+Casper"
            fw "alphaStr"
            let t = alphaStr "" s
            pre t  
            print $ ("ab", "12+Casper") == t

        when True $ do
            let s = "ab12+Casper"
            fw "alphaDigitStr"
            let t = alphaDigitStr "" s
            pre t  
            print $ ("ab12", "+Casper") == t

        when True $ do
            let s = "12ab+Casper"
            fw "alphaDigitStr"
            let t = alphaDigitStr "" s
            pre t  
            print $ ("12ab", "+Casper") == t

        when True $ do
            let s = "ab12+Casper"
            fw "getAlphaNum2"
            let t = getAlphaNum2 "" s
            pre t  
            print $ ("ab12", "+Casper") == t

        when True $ do
            let s = "12ab+Casper"
            fw "getAlphaNum2"
            let t = getAlphaNum2 "" s
            pre t  
            print $ ("", "12ab+Casper") == t

        when True $ do
            let s = "12+Casper"
            fw "getAlphaNum2"
            let t = getAlphaNum2 "" s
            pre t  
            print $ ("", "12+Casper") == t

        when True $ do
            let s = "ab+Casper"
            fw "getAlphaNum2"
            let t = getAlphaNum2 "" s
            pre t  
            print $ ("ab", "+Casper") == t

        when True $ do
            let s = "+Casper"
            fw "getAlphaNum2"
            let t = getAlphaNum2 "" s
            pre t  
            print $ ("", "+Casper") == t

{--
main = do
        argList <- getArgs
        let ln = len argList
        -- testAll
        if ln == 1 then do
            let s = head argList  
            let ls = tokenize s 
            let lt = colorToken ls 
            putStr $ concat lt
        else do
            putStrLn $ colorfgStr 200 "Need one argument"
            putStrLn $ colorfgStr 100 "cmd \"abc{dog}[cat]<cow> =>\""
--}


{--
data Token = AlphaNum String
            | TokLP String
            | TokRP String
            | TokLCB String
            | TokRCB String
            | TokLSB String
            | TokRSB String
            | TokGT String 
            | TokLT String 
            | OpAdd String
            | OpSub String
            | OpDiv String
            | OpMul String
            | DQString String
            | WSpace String
            | SymEqualRightArrow String               -- =>
            | NumberX String deriving(Show, Eq)
--}
