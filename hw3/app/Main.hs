module Main where

import HW3.Lexer
import HW3.Parser
import HW3.Evaluator
import HW3.Pretty
import HW3.Base
import HW3.Action
import Data.Either
import System.Console.Haskeline
import Data.Ratio
import Prettyprinter (Doc (..), pretty, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle(..))
import qualified Data.ByteString as B (readFile, writeFile, pack, concat)
import System.Directory (createDirectory, getCurrentDirectory, setCurrentDirectory)
import qualified Data.Text as T (pack)
import Data.Time.Clock (getCurrentTime )
import Control.Monad.IO.Class (liftIO )
import qualified Data.Set as Set (empty )
import Data.Time.Clock (UTCTime(..), addUTCTime, diffUTCTime)
import Data.Text.Encoding (encodeUtf8, decodeUtf8 )
import Sound.OSC.Coding.Byte (encode_u8, decode_u8)
import qualified Data.ByteString.Lazy as BL (fromStrict, toStrict)

f :: String -> String
f s = show $ parse $ alexScanTokens s

r = encodeUtf8 (T.pack "aa") 
{-
r1 = decodeUtf8 r
-}

pr (Right r) = prettyValue r
pr (Left err) = pretty (show err)


main :: IO ()
main = runInputT defaultSettings loop
 where
   loop :: InputT IO ()
   loop = do
     minput <- getInputLine "hi> "
     case minput of
       Nothing -> return ()
       Just "quit" -> return ()
       Just input -> do 
         x <- liftIO $ (runHIO (eval $ parse $ alexScanTokens input) Set.empty)
         outputStrLn (show (pr x ) )
         loop




{-
HiExprApply (HiExprValue (HiValueFunction HiFunAnd))
 [HiExprValue (HiValueBool True),HiExprApply (HiExprValue (HiValueFunction HiFunAnd)) 
 [HiExprValue (HiValueBool True),HiExprApply (HiExprValue (HiValueFunction HiFunAnd)) 
 [HiExprValue (HiValueBool True),HiExprValue (HiValueBool True)]]]

-}
e0 = "add (1, 2)"
e1 = "add (1.3, 2)"
e2 = "div (1, 3)"
ee = "div (4, 3)"
e3 = "div (1, 3, 6)"
e4 = "15 (1, 3) "
e5 = "sub(10, add)"
e6 = "div(3, 0)"
e7 = "true"
e8 = "false"
e9 = "equals(add(2, 2), 4)"
e10 = "less-than(mul(999, 99), 10000)"
e11 = "and(less-than(0, 1), less-than(1, 0))"
e12 = "if( greater-than(div ( 2, 5), div ( 3, 7)), 1, 2)"
e13 = "equals(mul, add )"
e14 = "less-than( null, null)"
e15 = "if(true, add, mul)(10, 10)"
e16 = "2 + 2 * 2"
e17 = "(2 + 2) * 3"
e18 = "2 + 2 * 3 == (2 + 2) * 3"
e18a = "true == true && true"
e18b = "true && true == true"
e19 = "10 == 2 * 5 && 143 == 11 * 13"
e19a = "10 == 2 * 5 || 143 == 11 * 13"
e20 = "\"qwerty\""
e21 = "length ( \"HelloWorld\" ) "
e22 = "to-upper(\"HelloWorld\")"
e23 = "to-lower(\"HelloWorld\")"
e24 = "reverse(\"HelloWorld\")"
e25 = "trim(\" HelloWorld \")"
e26 = "\"qwerty\"(2)"
e27 = "\"HelloWorld\"(2, 4)"
e27a = "\"HelloWorld\"(null, 4)"
e27b = "\"HelloWorld\"(2, null)"
e27c = "\"Hello World\"(0, -4)"
e27d = "\"Hello World\"(-4, -1)"
e28 = "\"Hello\" == \"World\""
e29 = "length(\"Hello\" + \"World\")"
e30 = "length(\"hehe\" * 5) / 3"
e31 = "to-upper(\"whatanicelanguage\")(7, 11)"
e32 = "\"hehe\" * 5"
e33 = " range (5.3, 10.3) "
e34 = " [1, 2, 3] "
e35 = " fold( mul, [2]) "
e35a = " fold(div, [11, 22, 33]) "
e35b = " fold(div, [0, 22, 33]) "
e35c = " fold(div, [1, 0, 33]) "
e35d = " [0, true, false, \"hello\", \"world\"](2, 4) "
e36 = " length([1, true, \"Hello\"]) "
e37 = " reverse([1, true, \"Hello\"]) "
e38 = " [1, 2] + [3, 4, 5] "
e39 = " [0, \"x\"] * 3 "
e40 = " [0, \"x\"](1) "
e41 = " [0, \"x\", 1 , 2](0, 1) "
e42 = " [ # aa bb # ] "
b1 = " [ # aa bb # ] "
e43 = "read(\"hitxt\")"
e44 = "write(\"hitxt\", \"Hi\")"
e45 = "mkdir(\"dir\")"
e46 = "cwd"
e47 = "cwd!"
e48 = "cd(\"dir\")!"
e49 = "now!"
e50 = "rand(0 , 10)"
e51 = "echo(\"qwewrw\")"
e52 = "echo(\"qwewrw\")!"
e53 = "rand(0, 10)"
e54 = "rand(0, 10)!"
e55 = "parse-time(\"2021-01-01 00:00:00 UTC\")"
e56 = "parse-time(\"2021-12-15 00:00:00 UTC\") + 1000"
e57 = "parse-time(\"2021-01-01 00:00:00 UTC\") + 365 * 24 * 60 * 60"
e58 = "parse-time(\"2021-12-15 00:37:51.000890793 UTC\") - parse-time(\"2021-12-15 00:37:47.649047038 UTC\")"
e59 = "sub (parse-time(\"2021-12-15 00:37:51.000890793 UTC\"), parse-time(\"2021-12-15 00:37:47.649047038 UTC\"))"
e60 = "1 - 2 - 3 - 4"
e65 = "if(2 == 2, echo(\"OK\")!, echo(\"WTF\")!)"
e66 = "true || echo(\"Don't do this\")!"
e67 = "false && echo(\"Don't do this\")!"
e68 = "zip(encode-utf8(\"Hello, World!\" * 1000))"
e69 = "serialise (\"Hello\")"
e70 = "pack-bytes([ 3, 255, 158, 32 ])"
e71 = "unpack-bytes([# aa bb cc #])"
e72 = "pack-bytes(range(30, 40))"
e73 = "encode-utf8(\"Hello!\")"
e74 = "unzip([# 78 da 63 64 62 06 00 00 0d 00 07 #])" 
e100 = "{ \"width\": 120, \"height\": 80 }"
e101 = "{ \"width\": 120, \"height\": 80 }(\"width\")"
e102 = "{ \"width\": 120, \"height\": 80 }(\"width\")"  -- just same
e103 = "keys({ \"width\": 120, \"height\": 80 })"
e104 = "values({ \"width\": 120, \"height\": 80 })"
e105 = "invert({ \"x\": 1, \"y\" : 2, \"z\": 1 }) "
e106 = "count(\"XXXOX\")"
e107 = "count([true, true, false, true])"
e108 = "count([# aa aa aa bb aa #])"
e109 = "count(\"Hello World\").o"
e110 = "echo.o-3431d-3e"
e111 = "echo.o-3431d-3e!"
{-
count("XXXOX") evaluates to { "O": 1, "X": 4 }
count([# 58 58 58 4f 58 #]) evaluates to { 79: 1, 88: 4 }
count([true, true, false, true]) evaluates to { false: 1, true: 3 }
invert({ "x": 1, "y" : 2, "z": 1 }) 
-}

test = do 
  x <- eval $ parse $ alexScanTokens e0
  putStr ("0: " ++ show (pr x ) ++ "\n")
  x1 <- eval $ parse $ alexScanTokens e1
  putStr ("1: " ++ show (pr x1 ) ++ "\n")
  x2 <- eval $ parse $ alexScanTokens e2
  putStr ("2: " ++ show (pr x2 ) ++ "\n")
  xe <- eval $ parse $ alexScanTokens ee
  putStr ("e: " ++ show (pr xe ) ++ "\n")
  x3 <- eval $ parse $ alexScanTokens e3
  putStr ("3: " ++ show (pr x3 ) ++ "\n")
  x4 <- eval $ parse $ alexScanTokens e4
  putStr ("4: " ++ show (pr x4 ) ++ "\n")
  x5 <- eval $ parse $ alexScanTokens e5
  putStr ("5: " ++ show (pr x5 ) ++ "\n")
  x6 <- eval $ parse $ alexScanTokens e6
  putStr ("6: " ++ show (pr x6 ) ++ "\n")
  x7 <- eval $ parse $ alexScanTokens e7
  putStr ("7: " ++ show (pr x7 ) ++ "\n")
  x8 <- eval $ parse $ alexScanTokens e8
  putStr ("8: " ++ show (pr x8 ) ++ "\n")
  x9 <- eval $ parse $ alexScanTokens e9
  putStr ("9: " ++ show (pr x9 ) ++ "\n")
  x10 <- eval $ parse $ alexScanTokens e10
  putStr ("10: " ++ show (pr x10 ) ++ "\n")
  x11 <- eval $ parse $ alexScanTokens e11
  putStr ("11: " ++ show (pr x11 ) ++ "\n")
  x12 <- eval $ parse $ alexScanTokens e12
  putStr ("12: " ++ show (pr x12 ) ++ "\n")
  x13 <- eval $ parse $ alexScanTokens e13
  putStr ("13: " ++ show (pr x13 ) ++ "\n")
  x14 <- eval $ parse $ alexScanTokens e14
  putStr ("14: " ++ show (pr x14 ) ++ "\n")
  x15 <- eval $ parse $ alexScanTokens e15
  putStr ("15: " ++ show (pr x15 ) ++ "\n")
  x16 <- eval $ parse $ alexScanTokens e16
  putStr ("16: " ++ show (pr x16 ) ++ "\n")
  x17 <- eval $ parse $ alexScanTokens e17
  putStr ("17: " ++ show (pr x17 ) ++ "\n")
  x18 <- eval $ parse $ alexScanTokens e18
  putStr ("18: " ++ show (pr x18 ) ++ "\n")
  x19 <- eval $ parse $ alexScanTokens e19
  putStr ("19: " ++ show (pr x19 ) ++ "\n")
  x19a <- eval $ parse $ alexScanTokens e19a
  putStr ("19a: " ++ show (pr x19a ) ++ "\n")
  x20 <- eval $ parse $ alexScanTokens e20
  putStr ("20: " ++ show (pr x20 ) ++ "\n")
  x21 <- eval $ parse $ alexScanTokens e21
  putStr ("21: " ++ show (pr x21 ) ++ "\n")
  x22 <- eval $ parse $ alexScanTokens e22
  putStr ("22: " ++ show (pr x22 ) ++ "\n")
  x23 <- eval $ parse $ alexScanTokens e23
  putStr ("23: " ++ show (pr x23 ) ++ "\n")
  x24 <- eval $ parse $ alexScanTokens e24
  putStr ("24: " ++ show (pr x24 ) ++ "\n")
  x25 <- eval $ parse $ alexScanTokens e25
  putStr ("25: " ++ show (pr x25 ) ++ "\n")
  x26 <- eval $ parse $ alexScanTokens e26
  putStr ("26: " ++ show (pr x26 ) ++ "\n")
  x27 <- eval $ parse $ alexScanTokens e27
  putStr ("27: " ++ show (pr x27 ) ++ "\n")
  x27a <- eval $ parse $ alexScanTokens e27a
  putStr ("27a: " ++ show (pr x27a ) ++ "\n")
  x27b <- eval $ parse $ alexScanTokens e27b
  putStr ("27b: " ++ show (pr x27b ) ++ "\n")
  x27c <- eval $ parse $ alexScanTokens e27c
  putStr ("27c: " ++ show (pr x27c ) ++ "\n")
  x27d <- eval $ parse $ alexScanTokens e27d
  putStr ("27d: " ++ show (pr x27d ) ++ "\n")
  x32 <- eval $ parse $ alexScanTokens e32
  putStr ("32: " ++ show (pr x32 ) ++ "\n")
  x28 <- eval $ parse $ alexScanTokens e28
  putStr ("28: " ++ show (pr x28 ) ++ "\n")
  x29 <- eval $ parse $ alexScanTokens e29
  putStr ("29: " ++ show (pr x29 ) ++ "\n")
  x30 <- eval $ parse $ alexScanTokens e30
  putStr ("30: " ++ show (pr x30 ) ++ "\n")
  x31 <- eval $ parse $ alexScanTokens e31
  putStr ("31: " ++ show (pr x31 ) ++ "\n")
  x33 <- eval $ parse $ alexScanTokens e33
  putStr ("33: " ++ show (pr x33 ) ++ "\n")
  x34 <- eval $ parse $ alexScanTokens e34
  putStr ("34: " ++ show (pr x34 ) ++ "\n")
  x35 <- eval $ parse $ alexScanTokens e35
  putStr ("35: " ++ show (pr x35 ) ++ "\n")
  x35a <- eval $ parse $ alexScanTokens e35a
  putStr ("35a: " ++ show (pr x35a ) ++ "\n")
  x35b <- eval $ parse $ alexScanTokens e35b
  putStr ("35b: " ++ show (pr x35b ) ++ "\n")
  x35c <- eval $ parse $ alexScanTokens e35c
  putStr ("35c: " ++ show (pr x35c ) ++ "\n")
  x35d <- eval $ parse $ alexScanTokens e35d
  putStr ("35d: " ++ show (pr x35d ) ++ "\n")
  x36 <- eval $ parse $ alexScanTokens e36
  putStr ("36: " ++ show (pr x36 ) ++ "\n")
  x37 <- eval $ parse $ alexScanTokens e37
  putStr ("37: " ++ show (pr x37 ) ++ "\n")
  x38 <- eval $ parse $ alexScanTokens e38
  putStr ("38: " ++ show (pr x38 ) ++ "\n")
  x39 <- eval $ parse $ alexScanTokens e39
  putStr ("39: " ++ show (pr x39 ) ++ "\n")
  x40 <- eval $ parse $ alexScanTokens e40
  putStr ("40: " ++ show (pr x40 ) ++ "\n")
  x41 <- eval $ parse $ alexScanTokens e41
  putStr ("41: " ++ show (pr x41 ) ++ "\n")
  x42 <- eval $ parse $ alexScanTokens e42
  putStr ("42: " ++ show (pr x42 ) ++ "\n")
  x43 <- eval $ parse $ alexScanTokens e43
  putStr ("43: " ++ show (pr x43 ) ++ "\n")
  x44 <- eval $ parse $ alexScanTokens e44
  putStr ("44: " ++ show (pr x44 ) ++ "\n")
  x45 <- eval $ parse $ alexScanTokens e45
  putStr ("45: " ++ show (pr x45 ) ++ "\n")
  x46 <- eval $ parse $ alexScanTokens e46
  putStr ("46: " ++ show (pr x46 ) ++ "\n")
  x47 <- eval $ parse $ alexScanTokens e47
  putStr ("47: " ++ show (pr x47 ) ++ "\n")
  x48 <- eval $ parse $ alexScanTokens e48
  putStr ("48: " ++ show (pr x48 ) ++ "\n")
  x47 <- eval $ parse $ alexScanTokens e47
  putStr ("47: " ++ show (pr x47 ) ++ "\n")
  x49 <- eval $ parse $ alexScanTokens e49
  putStr ("49: " ++ show (pr x49 ) ++ "\n")
  x50 <- eval $ parse $ alexScanTokens e50
  putStr ("50: " ++ show (pr x50 ) ++ "\n")
  x51 <- eval $ parse $ alexScanTokens e51
  putStr ("51: " ++ show (pr x51 ) ++ "\n")
  x52 <- eval $ parse $ alexScanTokens e52
  putStr ("52: " ++ show (pr x52 ) ++ "\n")
  x53 <- eval $ parse $ alexScanTokens e53
  putStr ("53: " ++ show (pr x53 ) ++ "\n")
  x54 <- eval $ parse $ alexScanTokens e54
  putStr ("54: " ++ show (pr x54 ) ++ "\n")
  x54 <- eval $ parse $ alexScanTokens e54
  putStr ("54: " ++ show (pr x54 ) ++ "\n")
  x55 <- eval $ parse $ alexScanTokens e55
  putStr ("55: " ++ show (pr x55 ) ++ "\n")
  x56 <- eval $ parse $ alexScanTokens e56
  putStr ("56: " ++ show (pr x56 ) ++ "\n")
  x57 <- eval $ parse $ alexScanTokens e57
  putStr ("57: " ++ show (pr x57 ) ++ "\n")
  x58 <- eval $ parse $ alexScanTokens e58
  putStr ("58: " ++ show (pr x58 ) ++ "\n")
  x59 <- eval $ parse $ alexScanTokens e59
  putStr ("59: " ++ show (pr x59 ) ++ "\n")
  x60 <- eval $ parse $ alexScanTokens e60
  putStr ("60: " ++ show (pr x60 ) ++ "\n")
  x65 <- eval $ parse $ alexScanTokens e65
  putStr ("65: " ++ show (pr x65 ) ++ "\n")
  x66 <- eval $ parse $ alexScanTokens e66
  putStr ("66: " ++ show (pr x66 ) ++ "\n")
  x67 <- eval $ parse $ alexScanTokens e67
  putStr ("67: " ++ show (pr x67 ) ++ "\n")
  x68 <- eval $ parse $ alexScanTokens e68
  putStr ("68: " ++ show (pr x68 ) ++ "\n")
  x69 <- eval $ parse $ alexScanTokens e69
  putStr ("69: " ++ show (pr x69 ) ++ "\n")
  x70 <- eval $ parse $ alexScanTokens e70
  putStr ("70: " ++ show (pr x70 ) ++ "\n")
  x71 <- eval $ parse $ alexScanTokens e71
  putStr ("71: " ++ show (pr x71 ) ++ "\n")
  x72 <- eval $ parse $ alexScanTokens e72
  putStr ("72: " ++ show (pr x72 ) ++ "\n")
  x73 <- eval $ parse $ alexScanTokens e73
  putStr ("73: " ++ show (pr x73 ) ++ "\n")
  x74 <- eval $ parse $ alexScanTokens e74
  putStr ("74: " ++ show (pr x74 ) ++ "\n")
  x100 <- eval $ parse $ alexScanTokens e100
  putStr ("100: " ++ show (pr x100 ) ++ "\n")
  x101 <- eval $ parse $ alexScanTokens e101
  putStr ("101: " ++ show (pr x101 ) ++ "\n")
  x102 <- eval $ parse $ alexScanTokens e102
  putStr ("102: " ++ show (pr x102 ) ++ "\n")
  x103 <- eval $ parse $ alexScanTokens e103
  putStr ("103: " ++ show (pr x103 ) ++ "\n")
  x104 <- eval $ parse $ alexScanTokens e104
  putStr ("104: " ++ show (pr x104 ) ++ "\n")
  x105 <- eval $ parse $ alexScanTokens e105
  putStr ("105: " ++ show (pr x105 ) ++ "\n")
  x106 <- eval $ parse $ alexScanTokens e106
  putStr ("106: " ++ show (pr x106 ) ++ "\n")
  x107 <- eval $ parse $ alexScanTokens e107
  putStr ("107: " ++ show (pr x107 ) ++ "\n")
  x108 <- eval $ parse $ alexScanTokens e108
  putStr ("108: " ++ show (pr x108 ) ++ "\n")
  x109 <- eval $ parse $ alexScanTokens e109
  putStr ("109: " ++ show (pr x109 ) ++ "\n")
  x110 <- eval $ parse $ alexScanTokens e110
  putStr ("110: " ++ show (pr x110 ) ++ "\n")
  x111 <- eval $ parse $ alexScanTokens e111
  putStr ("111: " ++ show (pr x111 ) ++ "\n")
  

 
{- 	 
  this test works but to check it, you need to run main and type "test" 
-}

t2 = parse $ alexScanTokens e110
t3 = parse $ alexScanTokens e18b
t4 = parse $ alexScanTokens e60
--t4 = B.singleton "11
{-

 HiExprApply (HiExprValue (HiValueFunction HiFunSub)) [HiExprValue (HiValueNumber (1 % 1)),HiExprApply (HiExprValue (HiValueFunction HiFunSub)) [HiExprValue (HiValueNumber (2 % 1)),HiExprApply (HiExprValue (HiValueFunction HiFunSub)) [HiExprValue (HiValueNumber (3 % 1)),HiExprValue (HiValueNumber (4 % 1))]]]  
 HiExprApply (HiExprValue (HiValueFunction HiFunSub)) [HiExprValue (HiValueNumber (1 % 1)),HiExprApply (HiExprValue (HiValueFunction HiFunSub)) [HiExprValue (HiValueNumber (2 % 1)),HiExprApply (HiExprValue (HiValueFunction HiFunSub)) [HiExprValue (HiValueNumber (3 % 1)),HiExprValue (HiValueNumber (4 % 1))]]]
 -}

{-


HiExprRun (HiExprApply (HiExprApply (HiExprValue (HiValueFunction HiFunAnd)) [HiExprValue (HiValueBool False),
HiExprValue (HiValueFunction HiFunEcho)]) [HiExprValue (HiValueString "Don't do this")])
-}  