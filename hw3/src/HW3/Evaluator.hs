module HW3.Evaluator (
    eval
  ) where
  
import HW3.Base
import Data.Ratio ((%), numerator, denominator)
import Control.Monad.Trans.Except 
import Control.Monad.Except
import qualified Data.Text as T (Text(..), unpack, index, length, toUpper, toLower, reverse, strip, replicate, concat, append, singleton, take, drop, pack )
import qualified Data.Sequence as S (Seq(..), fromList, reverse, (><), index, take, drop, index, singleton, length  )
import Data.Foldable (toList  )
import Data.Either (partitionEithers, fromRight)
import Data.List (foldl)
import Data.Semigroup (stimes)
import qualified Data.ByteString as B (ByteString(..), concat, append, writeFile, readFile, unpack, singleton, index, length   )
import qualified Data.ByteString.Lazy as BL (fromStrict, toStrict)
import Data.Text.Encoding (encodeUtf8, decodeUtf8 )
import qualified Data.Map as Map (Map (..), fromList, toList, (!), keys, elems, fromListWith, toList) -- 
import Text.Read (readMaybe)
import Data.Time.Clock (UTCTime(..), addUTCTime, diffUTCTime)
import Codec.Serialise (serialise, deserialise)
import Codec.Compression.Zlib
import Sound.OSC.Coding.Byte (encode_u8, decode_u8)
import Sound.OSC.Coding.Convert (word8_to_int)
--import qualified Data.ByteString.Internal as BI (toList) 
import Data.Word8 (Word8(..))

false :: Either HiError HiValue
false = Right $ (HiValueBool False)

true :: Either HiError HiValue
true = Right $ (HiValueBool True)

nullV :: HiValue
nullV = (HiValueNull)

{-

HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueBool Bool
  | HiValueNull
  | HiValueString T.Text
  | HiValueList (Seq HiValue)
  | HiValueBytes B.ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
-}




eval :: HiMonad m => HiExpr -> m (Either HiError HiValue) 
eval (HiExprValue a ) = return (Right a)
eval (HiExprApply (HiExprValue (HiValueNumber  _) ) _ ) = return (Left HiErrorInvalidFunction)
eval (HiExprApply (HiExprValue (HiValueBool  _) ) _ ) = return (Left HiErrorInvalidFunction)
eval (HiExprApply (HiExprValue HiValueNull ) _ ) = return (Left HiErrorInvalidFunction)
eval (HiExprApply (HiExprValue (HiValueBytes _) ) _ ) = return (Left HiErrorInvalidFunction)
eval (HiExprApply (HiExprValue (HiValueAction _) ) _ ) = return (Left HiErrorInvalidFunction)
eval (HiExprApply (HiExprValue (HiValueTime _) ) _ ) = return (Left HiErrorInvalidFunction)
eval (HiExprRun arg ) = do 
  act <- eval arg
  case act of
    (Right (HiValueAction (HiActionRead fp))) -> do hiV <- runAction (HiActionRead fp); return (Right hiV)
    (Right (HiValueAction (HiActionWrite fp bs))) -> do hiV <- runAction (HiActionWrite fp bs); return (Right hiV)
    (Right (HiValueAction (HiActionMkDir fp))) -> do hiV <- runAction (HiActionMkDir fp); return (Right hiV)
    (Right (HiValueAction (HiActionChDir fp))) -> do hiV <- runAction (HiActionChDir fp); return (Right hiV) 
    (Right (HiValueAction HiActionCwd)) -> do hiV <- runAction (HiActionCwd); return (Right hiV)
    (Right (HiValueAction HiActionNow)) -> do hiV <- runAction (HiActionNow); return (Right hiV)
    (Right (HiValueAction (HiActionRand a b))) -> do hiV <- runAction (HiActionRand a b); return (Right hiV)
    (Right (HiValueAction (HiActionEcho t))) -> do hiV <- runAction (HiActionEcho t); return (Right hiV)
    (Left err) -> return (Left err)
    _          -> return (Left HiErrorInvalidArgument)
eval (HiExprApply exp args ) =  do
  op <- eval exp
  case op of 
    (Right (HiValueFunction HiFunList)) -> do 
      ll <- mapM eval args--mapM :: Monad m => (a -> m b) -> t a -> m (t b)
      return (toL ll)
    (Right (HiValueFunction HiFunNot)) -> if (Prelude.length args == 1) 
      then do x <- eval (head args); return (nott x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunLength)) -> if (Prelude.length args == 1) 
      then do x <- eval (head args); return (lenT x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunToUpper)) -> if (Prelude.length args == 1) 
      then do x <- eval (head args); return (uppT x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunToLower)) -> if (Prelude.length args == 1) 
      then do x <- eval (head args); return (lowT x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunReverse)) -> if (Prelude.length args == 1) 
      then do x <- eval (head args); return (revT x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunTrim)) -> if (Prelude.length args == 1) 
      then do x <- eval (head args); return (trimT x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunPackBytes)) -> if (Prelude.length args == 1) 
      then do x <- eval (head args); return (packB x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunUnpackBytes)) -> if (Prelude.length args == 1) 
      then do x <- eval (head args); return (unPackB x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunEncodeUtf8)) -> if (Prelude.length args == 1) 
      then do x <- eval (head args); return (encodeB x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunDecodeUtf8)) -> if (Prelude.length args == 1) 
      then do x <- eval (head args); return (decodeB x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunZip)) -> if (Prelude.length args == 1) 
      then do x <- eval (head args); return (zipB x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunUnzip)) -> if (Prelude.length args == 1) 
      then do x <- eval (head args); return (unZipB x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunSerialise)) -> if (Prelude.length args == 1) 
      then do x <- eval (head args); return (serB x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunDeserialise)) -> if (Prelude.length args == 1) 
      then do x <- eval (head args); return (desB x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunRead)) -> if (Prelude.length args == 1)                  -- actions
      then do x <- eval (head args); return (readS x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunMkDir)) -> if (Prelude.length args == 1) 
      then do x <- eval (head args); return (mkdirS x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunChDir)) -> if (Prelude.length args == 1) 
      then do x <- eval (head args); return (cdS x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunParseTime)) -> if (Prelude.length args == 1) 
      then do x <- eval (head args); return (ttime x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunEcho)) -> if (Prelude.length args == 1) 
      then do x <- eval (head args); return (eecho x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueString str)) -> case (Prelude.length args) of
      1 -> do x <- eval (head args); return (indexT (HiValueString str) x)
      2 -> do x <- eval (head args) ; y <- eval (last args); return (substrT (HiValueString str) x y)
      _ -> return (Left HiErrorArityMismatch) 
    (Right (HiValueFunction HiFunWrite)) -> if (Prelude.length args == 2) 
      then do x <- eval (head args) ; y <- eval (last args); return (writeS x y)
      else return (Left HiErrorArityMismatch)  
    (Right (HiValueFunction HiFunAdd)) -> if (Prelude.length args == 2) 
      then do x <- eval (head args) ; y <- eval (last args); return (add x y)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunMul)) -> if (Prelude.length args == 2) 
      then do x <- eval (head args) ; y <- eval (last args); return (mul x y)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunDiv)) -> if (Prelude.length args == 2) 
      then do x <- eval (head args) ; y <- eval (last args); return (dif x y)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunSub)) -> if (Prelude.length args == 2) 
      then do x <- eval (head args) ; y <- eval (last args); return (sub x y)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunAnd)) -> if (Prelude.length args == 2) 
      then do 
        x <- eval (head args) 
        case x of 
          (Left err) -> return (Left err)
          (Right HiValueNull)  -> return x
          (Right (HiValueBool False)) -> return x
          _ -> eval (last args) 
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunOr)) -> if (Prelude.length args == 2) 
      then do 
        x <- eval (head args) 
        case x of 
          (Left err) -> return (Left err)
          (Right HiValueNull)  -> eval (last args)  
          (Right (HiValueBool False)) -> eval (last args) 
          _ -> return x
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunLessThan)) -> if (Prelude.length args == 2) 
      then do x <- eval (head args) ; y <- eval (last args); return (lt x y)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunGreaterThan)) -> if (Prelude.length args == 2) 
      then do x <- eval (head args) ; y <- eval (last args); return (gt x y)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunEquals)) -> if (Prelude.length args == 2) 
      then do x <- eval (head args) ; y <- eval (last args); return (eq x y)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunNotLessThan)) -> if (Prelude.length args == 2) 
      then do x <- eval (head args) ; y <- eval (last args); return (nlt x y)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunNotGreaterThan)) -> if (Prelude.length args == 2) 
      then do x <- eval (head args) ; y <- eval (last args); return (ngt x y)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunNotEquals)) -> if (Prelude.length args == 2) 
      then do x <- eval (head args) ; y <- eval (last args); return (neq x y)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunRange)) -> if (Prelude.length args == 2) 
      then do x <- eval (head args) ; y <- eval (last args); return (rangeS x y)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunFold)) -> if (Prelude.length args == 2) 
      then do x <- eval (head args) ; y <- eval (last args); return (foldS x y)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunRand)) -> if (Prelude.length args == 2) 
      then do x <- eval (head args) ; y <- eval (last args); return (randd x y)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunIf)) -> if (Prelude.length args == 3) 
      then do
        let (a:b:c:[]) = args  
        x <- eval a ; 
        case x of 
          (Left err) -> return (Left err)
          (Right (HiValueBool True))  -> eval b  
          (Right (HiValueBool False)) -> eval c 
          _ -> return (Left HiErrorInvalidArgument)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueList l)) -> case (Prelude.length args) of
      1 -> do x <- eval (head args); return (indexT (HiValueList l) x)
      2 -> do x <- eval (head args) ; y <- eval (last args); return (substrT (HiValueList l) x y)
      _ -> return (Left HiErrorArityMismatch)
    (Right (HiValueDict m)) -> if (Prelude.length args == 1) 
      then do x <- eval (head args); return (vByk m x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunKeys))  -> if (Prelude.length args == 1)    -- Maps
      then do x <- eval (head args); return ( keyss x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunValues))  -> if (Prelude.length args == 1)   
      then do x <- eval (head args); return ( vals x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunCount))  -> if (Prelude.length args == 1)    
      then do x <- eval (head args); return ( co x)
      else return (Left HiErrorArityMismatch)
    (Right (HiValueFunction HiFunInvert))  -> if (Prelude.length args == 1)    
      then do x <- eval (head args); return ( inv x)
      else return (Left HiErrorArityMismatch)
    (Left err) -> return (Left err)
    _          -> return (Left HiErrorInvalidArgument)
eval (HiExprDict args ) = do 
  let l1 = map f2 args  -- [(Either HiError (HiValue, HiValue))]
  let (ls, rs) = partitionEithers l1
  if (length ls == 0)
    then return (Right (HiValueDict (Map.fromList rs) ))
    else return (Left HiErrorInvalidArgument) 

f1 :: (HiExpr, HiExpr) -> ((Either HiError HiValue), (Either HiError HiValue))
f1 (a,b) = do
  x <- eval a
  y <- eval b
  (x, y)
 
 

f2 :: (HiExpr, HiExpr) -> (Either HiError (HiValue, HiValue) )
f2 (a,b) = do
  x <- eval a
  y <- eval b
  case x of 
    (Left err) -> (Left err)
    (Right xhv) -> case y of 
      (Left er) -> (Left er)
      (Right yhv) -> Right $ (xhv, yhv)
--toM :: [((Either HiError HiValue), (Either HiError HiValue))] -> (Either HiError [HiValue])

conc :: HiValue -> HiValue -> HiValue 
conc (HiValueList a) (HiValueList b) = HiValueList $ ( (S.><) a b ) 

inv :: (Either HiError HiValue)  -> (Either HiError HiValue)
inv  (Left err) = (Left err)
inv  (Right ( HiValueDict m)) = Right $ HiValueDict $ Map.fromListWith conc p
    where p = [(v, (HiValueList $ S.singleton $ k)) | (k, v) <- Map.toList m]
inv  _ = (Left HiErrorInvalidArgument)


 
ad :: HiValue -> HiValue -> HiValue 
ad (HiValueNumber a) (HiValueNumber b) = HiValueNumber $ (a + b) 




co :: (Either HiError HiValue)  -> (Either HiError HiValue)
co  (Left err) = (Left err)
co  (Right (HiValueString t)) = Right $ HiValueDict $ Map.fromListWith ad p
    where p = [((HiValueString $ T.singleton v), (HiValueNumber (1%1))) | v <- (T.unpack t)] 
co  (Right (HiValueList l)) = Right $ HiValueDict $ Map.fromListWith ad p
    where p = [(v, (HiValueNumber (1%1))) | v <- (toList l)] -- (toList a)
co  (Right ( HiValueBytes b)) = Right $ HiValueDict $ Map.fromListWith ad p
    where p = [((HiValueBytes $ B.singleton  v) , (HiValueNumber (1%1))) | v <- (B.unpack b)]
co  _ = (Left HiErrorInvalidArgument)





vals :: (Either HiError HiValue)  -> (Either HiError HiValue)
vals  (Left err) = (Left err)
vals  (Right ( HiValueDict m)) = (Right (HiValueList  (S.fromList (Map.elems m))) ) 
vals  _ = (Left HiErrorInvalidArgument)




keyss :: (Either HiError HiValue)  -> (Either HiError HiValue)
keyss  (Left err) = (Left err)
keyss  (Right ( HiValueDict m)) = (Right (HiValueList  (S.fromList (Map.keys m))) ) 
keyss  _ = (Left HiErrorInvalidArgument)



vByk :: (Map.Map HiValue HiValue) -> (Either HiError HiValue)  -> (Either HiError HiValue)
vByk  _ (Left err) = (Left err)
vByk  m (Right t) = (Right ((Map.!) m t))



eecho :: (Either HiError HiValue)  -> (Either HiError HiValue)
eecho  (Left err) = (Left err)
eecho  (Right (HiValueString  t)) = (Right (HiValueAction (HiActionEcho  t))) 
eecho  _ = (Left HiErrorInvalidArgument)



randd :: (Either HiError HiValue) -> (Either HiError HiValue) -> (Either HiError HiValue)
randd (Left err) _ = (Left err)
randd _ (Left err) = (Left err)
randd (Right (HiValueNumber a)) (Right (HiValueNumber b)) = if ((denominator a == 1) && (denominator b == 1))
  then (Right (HiValueAction (HiActionRand (fromIntegral (numerator a)) (fromIntegral (numerator b)) ))) 
  else (Left HiErrorInvalidArgument)
randd _ _ = (Left HiErrorInvalidArgument)

{-
if ((denominator a == 1) && (denominator b == 1))
  then 
    (Right (HiValueAction (HiActionRand (fromIntegral (numerator a)) (fromIntegral (numerator b)) ) 
  else 
    (Left HiErrorInvalidArgument)--(Right (HiValueAction (HiActionRand  a b )))
	-}



ttime :: (Either HiError HiValue)  -> (Either HiError HiValue)
ttime  (Left err) = (Left err)
ttime  (Right (HiValueString  t)) = let m = (readMaybe (T.unpack t) :: Maybe UTCTime) in
  case m of
    Nothing ->  (Right HiValueNull )
    (Just x) -> (Right  (HiValueTime x ))
ttime  _ = (Left HiErrorInvalidArgument)

readS :: (Either HiError HiValue)  -> (Either HiError HiValue)
readS  (Left err) = (Left err)
readS  (Right (HiValueString  t)) = (Right (HiValueAction (HiActionRead  (T.unpack t) )))
readS  _ = (Left HiErrorInvalidArgument)


writeS :: (Either HiError HiValue) -> (Either HiError HiValue) -> (Either HiError HiValue)
writeS (Left err) _ = (Left err)
writeS _ (Left err) = (Left err)
writeS (Right (HiValueString  t)) (Right (HiValueBytes bs)) = (Right (HiValueAction (HiActionWrite  (T.unpack t) (bs) )))
writeS _ _ = (Left HiErrorInvalidArgument)

mkdirS :: (Either HiError HiValue)  -> (Either HiError HiValue)
mkdirS  (Left err) = (Left err)
mkdirS  (Right (HiValueString  t)) = (Right (HiValueAction (HiActionMkDir  (T.unpack t) )))
mkdirS  _ = (Left HiErrorInvalidArgument)


cdS :: (Either HiError HiValue)  -> (Either HiError HiValue)
cdS  (Left err) = (Left err)
cdS  (Right (HiValueString  t)) = (Right (HiValueAction (HiActionChDir  (T.unpack t) )))
cdS  _ = (Left HiErrorInvalidArgument)

{-
serialise, deserialise
-}

desB :: (Either HiError HiValue)  -> (Either HiError HiValue)
desB  (Left err) = (Left err)
desB  (Right (HiValueBytes  bs)) = (Right (deserialise (BL.fromStrict bs)) )
desB  _ = (Left HiErrorInvalidArgument)


serB :: (Either HiError HiValue)  -> (Either HiError HiValue)
serB  (Left err) = (Left err)
serB  (Right a) = (Right (HiValueBytes (BL.toStrict (serialise a ))))

unZipB :: (Either HiError HiValue)  -> (Either HiError HiValue)
unZipB  (Left err) = (Left err)
unZipB  (Right (HiValueBytes  bs)) = (Right (HiValueBytes  (BL.toStrict( decompress (BL.fromStrict bs)))))
unZipB  _ = (Left HiErrorInvalidArgument)

zipB :: (Either HiError HiValue)  -> (Either HiError HiValue)
zipB  (Left err) = (Left err)
zipB  (Right (HiValueBytes  bs)) = (Right (HiValueBytes  (BL.toStrict( compressWith defaultCompressParams { compressLevel = bestCompression } (BL.fromStrict bs)))))
zipB  _ = (Left HiErrorInvalidArgument)

--encodeUtf8, decodeUtf8, word8_to_int
decodeB :: (Either HiError HiValue)  -> (Either HiError HiValue)
decodeB  (Left err) = (Left err)
decodeB  (Right (HiValueBytes  bs)) = 
  (Right (HiValueString (decodeUtf8 bs))) 
decodeB  _ = (Left HiErrorInvalidArgument)


encodeB :: (Either HiError HiValue)  -> (Either HiError HiValue)
encodeB  (Left err) = (Left err)
encodeB  (Right (HiValueString t)) = 
  (Right (HiValueBytes (encodeUtf8  t)  ) )
encodeB  _ = (Left HiErrorInvalidArgument)

unPackB :: (Either HiError HiValue)  -> (Either HiError HiValue)
unPackB  (Left err) = (Left err)
unPackB  (Right (HiValueBytes b)) = toL (fmap unpackhelper (B.unpack b))
unPackB  _ = (Left HiErrorInvalidArgument)

unpackhelper :: Word8 -> Either HiError HiValue
unpackhelper w = let n = word8_to_int w in if (0 <= n && n <= 255 )
  then 
    (Right (HiValueNumber (toRational n)))
  else 
    (Left HiErrorInvalidArgument)



packB :: (Either HiError HiValue)  -> (Either HiError HiValue)
packB (Left err) = (Left err)
packB (Right (HiValueList a)) = toB (fmap packhelper (toList a))
packB _ = (Left HiErrorInvalidArgument)


packhelper :: HiValue -> (Either HiError B.ByteString)
packhelper (HiValueNumber a) = if (denominator a == 1) 
  then let na = (numerator a) in if (0 <= na && na <= 255 )
    then 
      (Right (BL.toStrict (encode_u8 (fromIntegral na))))
    else 
      (Left HiErrorInvalidArgument)
  else 
    (Left HiErrorInvalidArgument)


toB :: [(Either HiError B.ByteString)] -> (Either HiError HiValue)
toB xs  = let (ls, rs) = partitionEithers xs in
  if (length ls == 0)
  then (Right (HiValueBytes (B.concat rs) ))
  else (Left HiErrorInvalidArgument)


toL :: [(Either HiError HiValue)] -> (Either HiError HiValue)
toL xs  = let (ls, rs) = partitionEithers xs in
  if (length ls == 0)
  then (Right (HiValueList (S.fromList rs) ))
  else (Left HiErrorInvalidArgument)

one :: Either HiError HiValue
one = (Right (HiValueNumber (1%1)))

zero :: Either HiError HiValue
zero = (Right (HiValueNumber (1%1)))
{-

HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  -}

foldS :: (Either HiError HiValue) -> (Either HiError HiValue) -> (Either HiError HiValue)
foldS (Left err) _ = (Left err)
foldS _ (Left err) = (Left err)
foldS (Right (HiValueFunction HiFunAdd)) (Right (HiValueList l)) = (foldl (\ a b -> add a (Right b)) zero l)
foldS (Right (HiValueFunction HiFunSub)) (Right (HiValueList l)) = (foldl (\ a b -> sub a (Right b)) zero l)
foldS (Right (HiValueFunction HiFunMul)) (Right (HiValueList l)) = (foldl (\ a b -> mul a (Right b)) one l)
foldS (Right (HiValueFunction HiFunDiv)) (Right (HiValueList l)) = (foldl (\ a b -> dif a (Right b)) (Right (S.index l 0)) (S.drop 1 l))
foldS _ _ = (Left HiErrorInvalidArgument)



range :: Rational -> Rational -> [HiValue]
range x y = if (x <= y) 
  then 
    (HiValueNumber x ) : (range (x + 1) y)
  else []



rangeS :: (Either HiError HiValue) -> (Either HiError HiValue) -> (Either HiError HiValue)
rangeS (Left err) _ = (Left err)
rangeS _ (Left err) = (Left err)
rangeS (Right (HiValueNumber a)) (Right (HiValueNumber b)) = 
  (Right (HiValueList (S.fromList  (range a b))) )
rangeS _ _ = (Left HiErrorInvalidArgument)


substrT :: HiValue  -> (Either HiError HiValue) -> (Either HiError HiValue) -> (Either HiError HiValue)
substrT _ (Left err) _  = (Left err)
substrT _ _ (Left err)  = (Left err)
substrT (HiValueString t) (Right (HiValueNumber a)) (Right (HiValueNumber b)) = if ((denominator a == 1) && (denominator b == 1))  
  then let
    ll = T.length t
    na = (fromIntegral (numerator a))
    nb = (fromIntegral (numerator b))
    aa = if na < 0 then ll + na else na
    bb = if nb < 0 then ll + nb else nb
    in
      (Right (HiValueString (T.drop aa (T.take bb t)) ))
  else 
    (Left HiErrorInvalidArgument)
substrT (HiValueString t) (Right HiValueNull) (Right (HiValueNumber b)) = if (denominator b == 1)  
  then
    (Right (HiValueString (T.take (fromIntegral (numerator b)) t)) )
  else 
    (Left HiErrorInvalidArgument) 
substrT (HiValueString t) (Right (HiValueNumber a)) (Right HiValueNull) = if (denominator a == 1)
  then
    Right $ HiValueString $ (T.drop (fromIntegral (numerator a)) t)
  else 
    (Left HiErrorInvalidArgument) 
substrT (HiValueList l) (Right (HiValueNumber a)) (Right (HiValueNumber b)) = if ((denominator a == 1) && (denominator b == 1))  
  then let
    ll = S.length l
    na = (fromIntegral (numerator a))
    nb = (fromIntegral (numerator b))
    aa = if na < 0 then ll + na else na
    bb = if nb < 0 then ll + nb else nb
    in
      (Right (HiValueList (S.drop (aa +1) (S.take (bb+1) l) ))) 
  else 
    (Left HiErrorInvalidArgument)
substrT tt (Right HiValueNull) (Right HiValueNull) = Right $ tt
substrT _ _ _ = (Left HiErrorInvalidArgument)



indexT :: HiValue -> (Either HiError HiValue) -> (Either HiError HiValue)
indexT _ (Left err)  = (Left err)
indexT (HiValueString t) (Right (HiValueNumber a)) = if (denominator a == 1) 
  then if ((a >= 0) && (fromIntegral (numerator a)) < (T.length t) )
    then 
      (Right (HiValueString (T.singleton (T.index t (fromIntegral (numerator a))))))
    else 
      (Right nullV)
  else 
    (Left HiErrorInvalidArgument)
indexT (HiValueList l) (Right (HiValueNumber a)) = if (denominator a == 1) 
  then if ((a >= 0) && (fromIntegral (numerator a)) < (Prelude.length l) )
    then 
      (Right (HiValueList (S.fromList [S.index l (fromIntegral (numerator a))])))
    else 
      (Right nullV)
  else 
    (Left HiErrorInvalidArgument)
indexT (HiValueBytes b) (Right (HiValueNumber a)) = if (denominator a == 1) 
  then if ((a >= 0) && (fromIntegral (numerator a)) < (B.length b) )
    then 
      (Right (HiValueBytes (B.singleton (B.index b (fromIntegral (numerator a)))   )))
    else 
      (Right nullV)
  else 
    (Left HiErrorInvalidArgument)
indexT _ _= (Left HiErrorInvalidArgument)
{-

packB :: (Either HiError HiValue)  -> (Either HiError HiValue)
packB (Left err) = (Left err)
packB (Right (HiValueList a)) = toB (fmap packhelper (toList a))
packB _ = (Left HiErrorInvalidArgument)


packhelper :: HiValue -> (Either HiError B.ByteString)
packhelper (HiValueNumber a) = if (denominator a == 1) 
  then let na = (numerator a) in if (0 <= na && na <= 255 )
    then 
      (Right (BL.toStrict (encode_u8 (fromIntegral na))))
    else 
      (Left HiErrorInvalidArgument)
  else 
    (Left HiErrorInvalidArgument)


toB :: [(Either HiError B.ByteString)] -> (Either HiError HiValue)
toB xs  = let (ls, rs) = partitionEithers xs in
  if (length ls == 0)
  then (Right (HiValueBytes (B.concat rs) ))
  else (Left HiErrorInvalidArgument)

-}




trimT :: (Either HiError HiValue) -> (Either HiError HiValue)
trimT (Left err)  = (Left err)
trimT (Right (HiValueString str)) = (Right (HiValueString (T.strip  str)))
trimT _ = (Left HiErrorInvalidArgument)


revT :: (Either HiError HiValue) -> (Either HiError HiValue)
revT (Left err)  = (Left err)
revT (Right (HiValueString str)) = (Right (HiValueString (T.reverse str)))
revT (Right (HiValueList l)) = (Right (HiValueList (S.reverse  l)))
revT _ = (Left HiErrorInvalidArgument)



lowT :: (Either HiError HiValue) -> (Either HiError HiValue)
lowT (Left err)  = (Left err)
lowT (Right (HiValueString str)) = (Right (HiValueString (T.toLower str)))
lowT _ = (Left HiErrorInvalidArgument)


uppT :: (Either HiError HiValue) -> (Either HiError HiValue)
uppT (Left err)  = (Left err)
uppT (Right (HiValueString str)) = (Right (HiValueString (T.toUpper str)))
uppT _ = (Left HiErrorInvalidArgument)



lenT :: (Either HiError HiValue) -> (Either HiError HiValue)
lenT (Left err)  = (Left err)
lenT (Right (HiValueString str)) = (Right (HiValueNumber ( (toInteger (T.length str)) % 1) ))
lenT (Right (HiValueList l)) = (Right (HiValueNumber ( (toInteger (Prelude.length l)) % 1) ))
lenT _ = (Left HiErrorInvalidArgument)



neq :: (Either HiError HiValue) -> (Either HiError HiValue) -> (Either HiError HiValue)
neq a b= nott (eq a b)


ngt :: (Either HiError HiValue) -> (Either HiError HiValue) -> (Either HiError HiValue)
ngt a b= nott (gt a b)

nlt :: (Either HiError HiValue) -> (Either HiError HiValue) -> (Either HiError HiValue)
nlt a b= nott (lt a b)


gt :: (Either HiError HiValue) -> (Either HiError HiValue) -> (Either HiError HiValue)
gt (Left err) _ = (Left err)
gt _ (Left err) = (Left err)
gt (Right (HiValueNumber a)) (Right (HiValueBool b)) = false 
gt (Right (HiValueBool a)) (Right (HiValueNumber b)) = true
gt (Right a) (Right b) = (Right (HiValueBool (a > b)) )


lt :: (Either HiError HiValue) -> (Either HiError HiValue) -> (Either HiError HiValue)
lt (Left err) _ = (Left err)
lt _ (Left err) = (Left err)
lt (Right (HiValueNumber a)) (Right (HiValueBool b)) = false 
lt (Right (HiValueBool a)) (Right (HiValueNumber b)) = true 
lt (Right a) (Right b) = (Right (HiValueBool (a < b)) )


eq :: (Either HiError HiValue) -> (Either HiError HiValue) -> (Either HiError HiValue)
eq (Left err) _ = (Left err)
eq _ (Left err) = (Left err)
eq (Right a) (Right b) = (Right (HiValueBool (a == b)) )

{-

eq (Right (HiValueNumber a)) (Right (HiValueNumber b)) = (Right (HiValueBool (a == b)) )
eq (Right (HiValueFunction a)) (Right (HiValueFunction b)) = (Right (HiValueBool (a == b)) )
eq (Right (HiValueNull)) (Right (HiValueNull)) = true 
eq (Right (HiValueString a)) (Right (HiValueString b)) = (Right (HiValueBool (a == b)) )
eq (Right (HiValueList a)) (Right (HiValueList b)) = (Right (HiValueBool (a == b)) )
eq (Right (HiValueBytes a)) (Right (HiValueBytes b)) = (Right (HiValueBool (a == b)) )
eq (Right (HiValueAction a)) (Right (HiValueAction b)) = (Right (HiValueBool (a == b)) )
eq (Right (HiValueTime a)) (Right (HiValueTime b)) = (Right (HiValueBool (a == b)) )
eq (Right (HiValueDict a)) (Right (HiValueDict b)) = (Right (HiValueBool (a == b)) )
  | HiValueList (Seq HiValue)
  | HiValueBytes B.ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
orr :: (Either HiError HiValue) -> (Either HiError HiValue) -> (Either HiError HiValue)
orr (Right (HiValueBool False)) b = b
orr (Right HiValueNull) b = b
orr  a _ = a
-}
{-
false = (HiValueBool False)
true = (HiValueBool True)
nullV = (HiValueNull)
-}

andd :: (Either HiError HiValue) -> (Either HiError HiValue) -> (Either HiError HiValue)
andd (Left err) _ = (Left err)
andd a@(Right (HiValueBool False)) _ = a
andd a@(Right HiValueNull) _ = a
andd _ b = b


nott :: (Either HiError HiValue) -> (Either HiError HiValue)
nott (Left err)  = (Left err)
nott (Right (HiValueBool b)) = (Right (HiValueBool (not b)))
nott _ = (Left HiErrorInvalidArgument)
 
  
add :: (Either HiError HiValue) -> (Either HiError HiValue) -> (Either HiError HiValue)
add (Left err) _ = (Left err)
add _ (Left err) = (Left err)
add (Right (HiValueNumber a)) (Right (HiValueNumber b)) = (Right (HiValueNumber (a+b)) )
add (Right (HiValueString a)) (Right (HiValueString b)) = (Right (HiValueString (T.append a b)) )
add (Right (HiValueList a)) (Right (HiValueList b)) = (Right (HiValueList ((S.><) a b)) )
add (Right (HiValueBytes a)) (Right (HiValueBytes b)) = (Right (HiValueBytes (B.append a b)) )
add (Right (HiValueTime a)) (Right (HiValueNumber b)) = (Right (HiValueTime (addUTCTime (fromIntegral (numerator b)) a)) )
add x@(Right (HiValueNumber b)) y@(Right (HiValueTime a))  = add y x
add _ _ = (Left HiErrorInvalidArgument)

--diffUTCTime
sub :: (Either HiError HiValue) -> (Either HiError HiValue) -> (Either HiError HiValue)
sub (Left err) _ = (Left err)
sub _ (Left err) = (Left err)
sub (Right (HiValueNumber a)) (Right (HiValueNumber b)) = (Right (HiValueNumber (a-b)) )
sub (Right (HiValueTime a)) (Right (HiValueTime b)) = (Right (HiValueNumber  (toRational(diffUTCTime a b))))
sub _ _ = (Left HiErrorInvalidArgument)

mul :: (Either HiError HiValue) -> (Either HiError HiValue) -> (Either HiError HiValue)
mul (Left err) _ = (Left err)
mul _ (Left err) = (Left err)
mul (Right (HiValueNumber a)) (Right (HiValueNumber b)) = (Right (HiValueNumber (a*b)) )
mul (Right (HiValueNumber a)) (Right (HiValueString b)) = if (denominator a == 1)
  then 
    (Right (HiValueString (T.replicate (fromIntegral (numerator a)) b)) )
  else 
    (Left HiErrorInvalidArgument)
mul x@(Right (HiValueString a)) y@(Right (HiValueNumber b)) = mul y x
mul (Right (HiValueNumber a)) (Right (HiValueList b)) = if (denominator a == 1) 
  then 
    (Right (HiValueList(stimes (numerator a) b)) ) -- cycleTaking = O (log k*n)
  else 
    (Left HiErrorInvalidArgument)
mul x@(Right (HiValueList a)) y@(Right (HiValueNumber b)) = mul y x
mul (Right (HiValueNumber a)) (Right (HiValueBytes b)) = if (denominator a == 1) 
  then 
    (Right (HiValueBytes (stimes (numerator a) b)) ) -- cycleTaking = O (log k*n)
  else 
    (Left HiErrorInvalidArgument)
mul x@(Right (HiValueBytes a)) y@(Right (HiValueNumber b)) = mul y x
mul _ _ = (Left HiErrorInvalidArgument)

dif :: (Either HiError HiValue) -> (Either HiError HiValue) -> (Either HiError HiValue)
dif (Left err) _ = (Left err)
dif _ (Left err) = (Left err)
dif (Right (HiValueString a)) (Right (HiValueString b)) = 
  (Right (HiValueString (T.concat [a, (T.singleton '/'), b]))) 
dif (Right (HiValueNumber a)) (Right (HiValueNumber b)) = if (b == 0) 
  then (Left HiErrorDivideByZero)
  else (Right (HiValueNumber (a/b)) )
dif _ _ = (Left HiErrorInvalidArgument)



-- fst ( quotRem (numerator a) (denominator a))



t1 = HiExprValue (HiValueNumber (1 % 1))
t2 = HiExprValue (HiValueNumber (2 % 1))

e = HiExprApply (HiExprValue (HiValueNumber (15 % 1))) [e1, HiExprValue (HiValueNumber (3 % 1))]
e1 = HiExprApply (HiExprValue(HiValueFunction HiFunAdd)) [HiExprValue (HiValueNumber (1 % 1)),HiExprValue (HiValueNumber (3 % 1))]
e2 = HiExprApply (HiExprValue (HiValueFunction HiFunSub)) [HiExprValue (HiValueNumber (10 % 1)),HiExprValue (HiValueFunction HiFunAdd)]



r0 = HiExprApply (HiExprValue (HiValueFunction HiFunEquals)) [HiExprValue (HiValueFunction HiFunAdd),HiExprValue (HiValueFunction HiFunAdd)]
r1 = HiExprValue (HiValueFunction HiFunAdd)
t3 = HiFunAdd == HiFunAdd
t4 = (HiValueFunction HiFunAdd) == (HiValueFunction HiFunAdd)


--s3 = HiExprApply (HiExprValue (HiValueFunction HiFunDiv)) [HiExprApply (HiExprValue (HiValueFunction HiFunLength)) [HiExprApply (HiExprValue (HiValueFunction HiFunMul)) [HiExprValue (HiValueString "hehe"),HiExprValue (HiValueNumber (5 % 1))]],HiExprValue (HiValueNumber (3 % 1))]

te = (T.pack "hitxt")
hiv = HiExprApply (HiExprValue (HiValueFunction HiFunRead)) [HiExprValue (HiValueString te)]
list0 = HiExprApply (HiExprValue (HiValueFunction HiFunList)) [HiExprValue (HiValueNumber (1 % 1)),HiExprValue (HiValueNumber (2 % 1)),HiExprValue (HiValueNumber (3 % 1))] 
test = do 
  a <- eval list0 
  show a
{-
test = do 
  a <- eval list0 
  show a
-}

{-
instance HiMonad [] where
  return a = [a]
  xs >>= k = join $ fmap k xs
-}
{-
 
test1 = do
  b <- eval e1 
  show b 
test2 = do
  c <- eval e2 
  show c
testr = do 
  a <- eval r0
  show a
testr1 = do 
  a <- eval r1
  show a
-}
--except :: Monad m => Either e a -> ExceptT e m a
--ExceptT :: m (Either e a) -> ExceptT e m a
--runExceptT :: ExceptT e m a -> m (Either e a)


