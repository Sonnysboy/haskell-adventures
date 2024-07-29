{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use join" #-}


module Imperative
  ( def,
    var,
    lit,
    while,
    (+=)
  )
where
import Control.Monad.IO.Class
import Control.Monad.State
import Data.IORef
import GHC.IO (unsafePerformIO)
import Data.STRef (STRef, readSTRef, modifySTRef, newSTRef)
import Control.Monad.ST
import GHC.ST (liftST)


newtype Comp a = Comp {runComp :: StateT Integer IO a} deriving (Functor, Applicative, Monad, MonadIO)
data Val s a 
 = Ref (STRef s a)
 | Lit a

var :: Integer -> ST s (Val s Integer)
var x = newSTRef $ Ref 

lit :: Integer -> ST thug (STRef thug Integer)
lit = newSTRef . Lit

while :: forall thug. STRef thug Integer -> (Integer -> Bool) -> ST thug () -> ST thug ()
while what cond body = do
  fate <- readSTRef what
  when (cond fate) $ do
    body
    while what cond body

-- Define the 'def' function
def :: (forall s. ST s (STRef s a)) -> a
def action = runST $ do 
    x <- action
    readSTRef x

(+=) :: forall s. (STRef s Integer) -> (STRef s Integer) -> ST s ()
ref += what = do 
  x1 <- readSTRef ref 
  modifySTRef ref (+ readSTRef what)