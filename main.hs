module Rx where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.Maybe

-------------------- TYPES ----------------------

newtype Observable a = Observable { onSubscribe :: Observer a -> IO () }

data Observer a = Observer { onNext :: a -> IO (), 
                             onError :: SomeException -> IO (),
                             onCompleted :: IO (),
                             addSubscription :: IO () -> IO (),
                             subscription :: IO Subscription }

type Subscription = IORef (Maybe [IO ()])

---------------- INSTANCE DECLARATIONS -----------------

instance Functor Observable where
  fmap f = lift (coMap f)
    where
      coMap :: (a -> b) -> Observer b -> Observer a
      coMap f ob = createObserver (onNext ob . f) 
                                  (onError ob)
                                  (onCompleted ob)

instance Applicative Observable where
  pure a = Observable (\obr -> onNext obr a >> onCompleted obr)
  f <*> a = rxCombineLatestWith ($) f a

instance Monad Observable where
  o >>= f = rxMerge $ fmap f o

--------------- GENERAL FUNCTIONS -----------------

createObserver :: (a -> IO ()) -> (SomeException -> IO ()) -> IO () -> Observer a
createObserver onNext onError onCompleted = Observer onNext onError onCompleted addSubscription subscription
  where
    subscription = newIORef $ Just []
    addSubscription action = do s <- subscription
                                actions <- readIORef s
                                when (isJust actions) . writeIORef s $ fmap (action :) actions 

subscribe :: Observable a -> Observer a -> IO Subscription
subscribe obl obr = onSubscribe obl obr >> subscription obr

unsubscribe :: Subscription -> IO ()
unsubscribe s = do subs <- readIORef s
                   when (isJust subs) $ sequence_ (fromJust subs) >> writeIORef s Nothing

isUnsubscribed :: Subscription -> IO Bool
isUnsubscribed s = isNothing <$> readIORef s

-------------------- OPERATORS --------------------

lift :: (Observer b -> Observer a) -> Observable a -> Observable b
lift f oa = Observable (onSubscribe oa . f)

-- TODO: `onCompleted obr` is never called now
rxMerge :: Observable (Observable a) -> Observable a
rxMerge oas = Observable (\obr -> onSubscribe oas $ createObserver (\oa -> onSubscribe oa $ createObserver (onNext obr) (onError obr) (return ())) (onError obr) (return ()))

rxCombineLatestWith :: (a -> b -> c) -> Observable a -> Observable b -> Observable c
rxCombineLatestWith f oa ob = uncurry f <$> rxCombineLatest oa ob

rxCombineLatest :: Observable a -> Observable b -> Observable (a,b)
rxCombineLatest oa ob = Observable (\obr -> do refA <- newIORef Nothing
                                               refB <- newIORef Nothing
                                               doneA <- newIORef False
                                               doneB <- newIORef False
                                               onSubscribe oa $ createObserver (onNextA refA refB obr) (onError obr) (handleOnCompleted doneB doneA obr)
                                               onSubscribe ob $ createObserver (onNextB refA refB obr) (onError obr) (handleOnCompleted doneA doneB obr))
  where
    handleOnCompleted readRef writeRef obr = do done <- readIORef readRef 
                                                if done then onCompleted obr else writeIORef writeRef True
    combine readRef writeRef val = writeIORef writeRef (Just val) >> readIORef readRef
    onNextA refA refB obr a = do maybeB <- combine refB refA a
                                 when (isJust maybeB) $ onNext obr (a, fromJust maybeB)
    onNextB refA refB obr b = do maybeA <- combine refA refB b
                                 when (isJust maybeA) $ onNext obr (fromJust maybeA, b)

-- or just use mfilter (MonadPlus)?
rxFilter :: (a -> Bool) -> Observable a -> Observable a
rxFilter p = lift (coFilter p)
  where
    coFilter :: (a -> Bool) -> Observer a -> Observer a
    coFilter p oa = createObserver (\a -> when (p a) $ onNext oa a)
                                   (onError oa)
                                   (onCompleted oa)

------------------------------ TEST -------------------------------

stream :: Observable Integer
stream = Observable (\observer -> do onNext observer 1
                                     onNext observer 2
                                     onNext observer 3
                                     onCompleted observer)

printObserver :: Show a => String -> Observer a
printObserver doneMsg = createObserver print print (print doneMsg)

main :: IO ()
main = do putStrLn "hello friend"
          subscribe stream $ printObserver "done 1"
          subscribe (fmap (*1337) stream) $ printObserver "done 2"
          subscribe (rxFilter (> 1) stream) $ printObserver "done 3"
          subscribe (pure (\x -> "Transformed " ++ show x) <*> stream) $ printObserver "done 4"
          subscribe (stream >>= (\x -> Observable (\obr -> onNext obr x >> onNext obr x >> onCompleted obr))) $ printObserver "done 5"
          return ()

