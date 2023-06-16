{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module HW3.Base 
  ( HiFun(..)
  , HiValue(..)
  , HiExpr(..)
  , HiError(..)
  , HiAction(..)
  , HiMonad(..)
  ) where
  
import qualified Data.Text as T 
import Data.Sequence (Seq(..))
import qualified Data.ByteString as B 
import Control.Monad.Trans.Reader
import System.Directory (Permissions(..))
import Data.Time.Clock (UTCTime (..))
import Data.Map (Map(..))
import System.Directory (createDirectory, getCurrentDirectory, setCurrentDirectory)
import Data.Time.Clock (getCurrentTime )
import System.Random (getStdRandom, randomR)
import Control.Monad.IO.Class (liftIO)
import Data.Ratio ((%))
import Codec.Serialise (Serialise (..))
import GHC.Generics (Generic)

nullV :: HiValue
nullV = (HiValueNull)


data HiFun = HiFunDiv
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
  deriving (Show, Eq, Ord, Serialise, Generic)  



data HiValue = HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueBool Bool
  | HiValueNull
  | HiValueString T.Text
  | HiValueList (Seq HiValue)
  | HiValueBytes B.ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Show, Eq, Ord, Serialise, Generic)

data HiExpr = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving Show

data HiError = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq, Ord)


data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath B.ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho T.Text
  deriving (Show, Eq, Ord, Serialise, Generic)
  
class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue  
  


instance HiMonad IO where
  runAction (HiActionRead fp) = do bs <- B.readFile fp; return (HiValueBytes bs)
  runAction (HiActionWrite fp bs) = (B.writeFile fp bs) >> return nullV
  runAction (HiActionMkDir fp) = createDirectory fp >> return nullV
  runAction (HiActionChDir fp) = do c <- getCurrentDirectory; setCurrentDirectory (concat [c, "\\" ,fp]) ; return nullV
  runAction (HiActionCwd) = do s <- getCurrentDirectory; return (HiValueString (T.pack s))
  runAction (HiActionNow) = do t <- getCurrentTime; return (HiValueTime t)
  runAction (HiActionRand a b ) = do
    v <- liftIO $ getStdRandom (randomR (a, b))
    return (HiValueNumber ((toInteger v) % 1))
  runAction (HiActionEcho t ) = putStrLn (T.unpack t) >> return nullV 

  
instance HiMonad [] where
  runAction = undefined
  
instance HiMonad (Either HiError) where
  runAction = undefined
  
  
  
instance HiMonad  ((,) (Either HiError HiValue)) where
  runAction = undefined 
  
  
  
  
instance (Monoid (Either HiError HiValue)) where
  mempty = undefined
  mappend = undefined
{-
  
  
instance HiMonad (,) where
  runAction = undefined 
  -}
  