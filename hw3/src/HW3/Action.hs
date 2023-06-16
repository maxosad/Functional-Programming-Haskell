module HW3.Action
  (
  HiPermission(..)
  , PermissionException(..)
  , HIO(..)
  ) where
import Control.Exception
import Data.Set
import HW3.Base
import Control.Monad (ap)
import Control.Monad.IO.Class

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving Show

data PermissionException =
  PermissionRequired HiPermission
  deriving Show

instance Exception PermissionException 

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }


instance Functor HIO where
  fmap f (HIO hio) = HIO (fmap (fmap f) hio)

instance Monad HIO where
  return x = HIO $ \_ -> return x
  m >>= k  = HIO $ \w -> do
                 x <- m `runHIO` w
                 k x `runHIO` w


instance Applicative HIO where
  pure  = return
  f <*> x = Control.Monad.ap f x


instance HiMonad HIO where
  runAction a = HIO $ \per -> runAction a

