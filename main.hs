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
      coMap f ob = Observer (onNext ob . f) 
                            (onError ob) 
                            (onCompleted ob)

instance Applicative Observable where
  pure a = Observable (\obr -> onNext obr a >> onCompleted obr)
  f <*> a = rxCombineLatestWith ($) f a

lift :: (Observer b -> Observer a) -> Observable a -> Observable b
lift f oa = Observable (subscribe oa . f)

rxCombineLatestWith :: (a -> b -> c) -> Observable a -> Observable b -> Observable c
rxCombineLatestWith f oa ob = uncurry f <$> rxCombineLatest oa ob

rxCombineLatest :: Observable a -> Observable b -> Observable (a,b)
rxCombineLatest oa ob = Observable onSubscribe
  where
    onSubscribe obr = do 
        refA <- newIORef Nothing
        refB <- newIORef Nothing
        doneA <- newIORef False
        doneB <- newIORef False
        subscribe oa $ Observer (onNextA refA refB obr) (onError obr) (handleOnCompleted doneB doneA obr)
        subscribe ob $ Observer (onNextB refA refB obr) (onError obr) (handleOnCompleted doneA doneB obr)
    handleOnCompleted readRef writeRef obr = do done <- readIORef readRef 
                                                if done then onCompleted obr else writeIORef writeRef True
    combine readRef writeRef val = writeIORef writeRef (Just val) >> readIORef readRef
    onNextA refA refB obr a = do maybeB <- combine refB refA a
                                 when (isJust maybeB) $ onNext obr (a, fromJust maybeB)
    onNextB refA refB obr b = do maybeA <- combine refA refB b
                                 when (isJust maybeA) $ onNext obr (fromJust maybeA, b)

rxFilter :: (a -> Bool) -> Observable a -> Observable a
rxFilter p = lift (coFilter p)
  where
    coFilter :: (a -> Bool) -> Observer a -> Observer a  
    coFilter p oa = Observer (\a -> when (p a) $ onNext oa a) 
                             (onError oa) 
                             (onCompleted oa)

stream :: Observable Integer
stream = Observable (\observer -> do onNext observer 1
                                     onNext observer 2
                                     onNext observer 3
                                     onCompleted observer)

printObserver :: Show a => String -> Observer a
printObserver doneMsg = Observer print print (print doneMsg)

main :: IO ()
main = do putStrLn "hello friend"
          subscribe stream $ printObserver "done 1"
          subscribe (fmap (*1337) stream) $ printObserver "done 2"
          subscribe (rxFilter (> 1) stream) $ printObserver "done 3"
          subscribe (pure (\x -> "Transformed " ++ show x) <*> stream) $ printObserver "done 4"
          return ()

