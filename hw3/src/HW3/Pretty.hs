module HW3.Pretty (
    prettyValue
  ) where
  
import HW3.Base
import Prettyprinter (Doc (..), pretty, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle(..))
import Data.Ratio 
import qualified Data.Scientific as Scientific (fromRationalRepetendUnlimited )
import Data.Maybe (isJust)
import Data.Text (Text(..), pack)
import Data.Sequence (Seq(..), singleton, fromList, cycleTaking, drop, take   )
import Data.Semigroup (stimes)
import qualified Data.ByteString as B (ByteString(..), concat, head, take, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Map as Map (toList)
import Numeric (showHex)
import Data.List 

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueFunction HiFunAdd) = pretty "add"
prettyValue (HiValueFunction HiFunMul) = pretty "mul"
prettyValue (HiValueFunction HiFunDiv) = pretty "div"
prettyValue (HiValueFunction HiFunSub) = pretty "sub"
prettyValue (HiValueFunction HiFunNot) = pretty "not"
prettyValue (HiValueFunction HiFunAnd) = pretty "and"
prettyValue (HiValueFunction HiFunOr) = pretty "or"
prettyValue (HiValueFunction HiFunLessThan) = pretty "less-then"
prettyValue (HiValueFunction HiFunGreaterThan) = pretty "less-then"
prettyValue (HiValueFunction HiFunEquals) = pretty "equals"
prettyValue (HiValueFunction HiFunNotLessThan) = pretty "not-less-than"
prettyValue (HiValueFunction HiFunNotGreaterThan) = pretty "not-greater-than"
prettyValue (HiValueFunction HiFunNotEquals) = pretty "not-equals"
prettyValue (HiValueFunction HiFunIf) = pretty "if"
prettyValue (HiValueFunction HiFunLength) = pretty "length"
prettyValue (HiValueFunction HiFunToUpper) = pretty "to-upper"
prettyValue (HiValueFunction HiFunToLower) = pretty "to-lower"
prettyValue (HiValueFunction HiFunReverse) = pretty "reverse"
prettyValue (HiValueFunction HiFunTrim) = pretty "trim"
prettyValue (HiValueFunction HiFunList) = pretty "list"
prettyValue (HiValueFunction HiFunRange) = pretty "range"
prettyValue (HiValueFunction HiFunFold) = pretty "fold"
prettyValue (HiValueFunction HiFunPackBytes) = pretty "pack-bytes'"
prettyValue (HiValueFunction HiFunUnpackBytes) = pretty "unpack-bytes"
prettyValue (HiValueFunction HiFunEncodeUtf8) = pretty "zip"
prettyValue (HiValueFunction HiFunDecodeUtf8) = pretty "unzip"
prettyValue (HiValueFunction HiFunZip) = pretty "encode-utf8"
prettyValue (HiValueFunction HiFunUnzip) = pretty "decode-utf8"
prettyValue (HiValueFunction HiFunSerialise) = pretty "serialise"
prettyValue (HiValueFunction HiFunDeserialise) = pretty "deserialise"
prettyValue (HiValueFunction HiFunRead) = pretty "read"
prettyValue (HiValueFunction HiFunWrite) = pretty "write"
prettyValue (HiValueFunction HiFunMkDir) = pretty "mkdir"
prettyValue (HiValueFunction HiFunChDir) = pretty "cd"
prettyValue (HiValueFunction HiFunParseTime) = pretty "parse-time"
prettyValue (HiValueFunction HiFunRand) = pretty "rand"
prettyValue (HiValueFunction HiFunEcho) = pretty "echo"
prettyValue (HiValueFunction HiFunCount) = pretty "count"
prettyValue (HiValueFunction HiFunKeys) = pretty "keys"
prettyValue (HiValueFunction HiFunValues) = pretty "values"
prettyValue (HiValueFunction HiFunInvert) = pretty "invert"
prettyValue (HiValueNumber num) = let 
  (si, mi) = Scientific.fromRationalRepetendUnlimited num
  nu       = (numerator num)
  den      = (denominator num) 
  (c, d)   = quotRem nu den
  in
    if (isJust mi)
      then 
        if (c == 0)
          then 
            pretty nu <> pretty "/" <> pretty den  
          else if (0 > d)
            then 
              pretty c <+> pretty "-" <+> pretty (d*(-1)) <> pretty "/" <> pretty den  
            else 
              pretty c <+> pretty "+" <+> pretty d <> pretty "/" <> pretty den  
      else  
        if (d == 0) 
          then -- int
            pretty c
          else -- int.int
            pretty (show si) -- (pretty a)	
prettyValue (HiValueBool True) = pretty  "true"
prettyValue (HiValueBool False) = pretty  "false"
prettyValue (HiValueNull) = pretty  "null"
prettyValue (HiValueString t) = pretty  (show t)
prettyValue (HiValueList l) = pretty  "[" <+> (seqViv l) <+> pretty "]"
prettyValue (HiValueBytes b) = pretty  "[ #" <+> pretty (foldl1 helperbyte $ fmap (`showHex` "") (B.unpack b)) <+> pretty "# ]" 
prettyValue (HiValueTime t) = pretty "parse-time(\"" <> pretty  (show t) <> pretty "\")"
prettyValue (HiValueAction (HiActionRead fp)) = pretty  "read(" <> pretty (show fp)<>pretty ")"
prettyValue (HiValueAction (HiActionWrite fp bs)) = pretty  "write("  <> pretty (show fp) <> pretty "\"," <> prettyValue (HiValueBytes bs) <>pretty ")"
prettyValue (HiValueAction (HiActionMkDir fp)) = pretty  "mkdir(" <> pretty (show fp) <> pretty ")"
prettyValue (HiValueAction (HiActionChDir fp)) = pretty  "cd(" <> pretty (show fp) <> pretty ")"
prettyValue (HiValueAction HiActionCwd) = pretty  "cwd"
prettyValue (HiValueAction HiActionNow) = pretty  "now"
prettyValue (HiValueAction (HiActionRand a b)) = pretty  "rand(" <+> pretty a <> pretty "," <+> pretty b <+> pretty ")"
prettyValue (HiValueAction (HiActionEcho t)) = pretty  "echo(" <> pretty (show t) <> pretty ")"
prettyValue (HiValueDict m) = pretty  "{" <> (foldl1 (\x y -> x <> pretty ", " <> y) $ fmap (\(a,b) -> prettyValue a <> pretty ": " <> prettyValue b) (Map.toList m) ) <> pretty "}"

--ple = fmap (`showHex` "") (B.unpack (encodeUtf8 (pack "aa") ))

seqViv :: Seq HiValue -> Doc AnsiStyle
seqViv (a :<| ost) = (prettyValue a) <> (helper ost)
seqViv (Empty ) = pretty ""


helperbyte :: String -> String -> String
helperbyte x y = let 
  xx = if length x == 1 then '0' : x else x
  yy = if length y == 1 then '0' : y else y
  in xx <> " " <> yy 


helper :: Seq HiValue -> Doc AnsiStyle
helper (a :<| ost) = pretty "," <+> (prettyValue a)  <> (helper ost)
helper (Empty ) = pretty ""


s = (Data.Text.pack "qwerty")


i0 = (42 % 1)
i1 = (9 % 2)
i2 = (-1 % 3)
i3 = (16 % 7)

v0 = (HiValueNumber i0)
v1 = (HiValueNumber i1)
v2 = (HiValueNumber i2)
v3 = (HiValueNumber i3)
s0 = (HiValueString s)

e0 = Scientific.fromRationalRepetendUnlimited i0
e1 = Scientific.fromRationalRepetendUnlimited i1
e2 = Scientific.fromRationalRepetendUnlimited i2
e3 = Scientific.fromRationalRepetendUnlimited i3

b0 = (HiValueBytes (B.concat[(encodeUtf8(pack "aa")), (encodeUtf8(pack "bb"))]  )   )


prov = foldl (+) 0 (fromList [1, 2, 3])
prov1 = length (fromList [1, 2, 3])
prov2 = cycleTaking 4 (fromList [1, 2, 3])
prov3 = stimes 3 (fromList [1, 2, 3])
prov4 = Data.Sequence.drop 1 (fromList [1, 2, 3])
prov5 = Data.Sequence.take 2 (fromList [1, 2, 3])
prov6 =  B.head (B.concat[(encodeUtf8(pack "aa")), (encodeUtf8(pack "bb"))]  )

seq1 = (singleton v0)
l1 = (HiValueList seq1)
t1 = seqViv seq1
test = do 
  putStr (show (prettyValue v0) ++ "\n")
  putStr (show (prettyValue v2) ++ "\n")
  putStr (show (prettyValue v3) ++ "\n")
  putStr (show (prettyValue v1) ++ "\n")
  putStr (show e0 ++ "\n")
  putStr (show e1 ++ "\n")
  putStr (show e2 ++ "\n")
  putStr (show e3 ++ "\n")
  putStr (show (prettyValue s0) ++ "\n")
  putStr (show (prettyValue l1) ++ "\n")













