module Rx where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.Maybe

newtype Observable a = Observable { subscribe :: Observer a -> IO () }

data Observer a = Observer { onNext :: a -> IO (), 
                             onError :: SomeException -> IO (),
                             onCompleted :: IO () }

data Subscription = Subscription { unsubscribe :: IO (),
                                   isUnsubscribed :: IO Bool }

instance Functor Observable where
  fmap f = lift (coMap f)
    where
      coMap :: (a -> b) -> Observer b -> Observer a
      coMap f ob = Observer (\a -> onNext ob (f a)) 
                            (onError ob) 
                            (onCompleted ob)

instance Applicative Observable where
  pure a = Observable (\obr -> onNext obr a >> onCompleted obr)
  f <*> a = rxCombineLatestWith ($) f a

lift :: (Observer b -> Observer a) -> (Observable a -> Observable b)
lift f = \obsA -> Observable (\obrB -> subscribe obsA (f obrB))

rxCombineLatestWith :: (a -> b -> c) -> Observable a -> Observable b -> Observable c
rxCombineLatestWith f oa ob = uncurry f <$> rxCombineLatest oa ob

rxCombineLatest :: Observable a -> Observable b -> Observable (a,b)
rxCombineLatest oa ob = Observable onSubscribe
  where
    onSubscribe obr = do 
        varA <- newIORef Nothing
        varB <- newIORef Nothing
        subscribe oa $ Observer (onNextA varA varB obr) (onError obr) (onCompleted obr)
        subscribe ob $ Observer (onNextB varA varB obr) (onError obr) (onCompleted obr)
    onNextA varA varB obr a = do writeIORef varA (Just a)
                                 Just b <- readIORef varB
                                 onNext obr (a, b)
    onNextB varA varB obr b = do writeIORef varB (Just b)
                                 Just a <- readIORef varA
                                 onNext obr (a, b)

rxFilter :: (a -> Bool) -> Observable a -> Observable a
rxFilter p = lift (coFilter p)
  where
    coFilter :: (a -> Bool) -> Observer a -> Observer a  
    coFilter p oa = Observer (\a -> if p a then onNext oa a else return ()) 
                             (onError oa) 
                             (onCompleted oa)

stream :: Observable Integer
stream = Observable (\observer -> do onNext observer 1
                                     onNext observer 2
                                     onNext observer 3
                                     onCompleted observer)

main :: IO ()
main = do putStrLn "hello friend"
          subscribe stream (Observer print print (print "done 1"))
          subscribe (fmap (*1337) stream) (Observer print print (print "done 2"))
          subscribe (rxFilter (> 1) stream) (Observer print print (print "done 3"))
          return ()

