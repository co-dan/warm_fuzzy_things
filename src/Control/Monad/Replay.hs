{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{- | The replay monad, for computations that can be replayed using traces

 Example usage:


> running :: Replay Question Answer a -> IO a
> running prog = play emptyTrace
>  where
>   play t = do
>     (eqa,t') <- run prog t    this is the same prog every time!
>     case eqa of
>       Left q -> do
>         putStr ("Question: " ++ q ++ " ")
>         r <- getLine
>         play (addAnswer t' r)
>       Right x -> return x
>
> askAge :: Replay String String Int
> askAge = cut $ do
>   birth <- ask "What is your birth year?"
>   now   <- ask "What is the current year?"
>   return (read now - read birth)
>
> getTime :: IO Integer
> getTime = do
>   TOD secs _ <- getClockTime
>   return secs
>
> example1 :: Replay Question Answer Int
> example1 = do
>   t0 <- lift getTime
>   lift (putStrLn "Hello!")
>   age <- ask "What is your age?"
>   lift (putStrLn ("You are " ++ age))
>   name <- ask "What is your name?"
>   lift (putStrLn (name ++ " is " ++ age ++ " years old"))
>   t1 <- lift getTime
>   lift (putStrLn ("Total time: " ++ show (t1 - t0) ++ " seconds"))
>   return (read age)
>
> example2 :: Replay Question Answer Int
> example2 = do
>   t0 <- lift getTime
>   age <- askAge
>   lift (putStrLn ("You are " ++ (show age)))
>   name <- ask "What is your name?"
>   lift (putStrLn (name ++ " is " ++ (show age) ++ " years old"))
>   t1 <- lift getTime
>   lift (putStrLn ("Total time: " ++ show (t1 - t0) ++ " seconds"))
>   return age
>

-}
module Control.Monad.Replay
       ( Replay, lift, ask, run, cut
       , emptyTrace, addAnswer
       , Trace, Question, Answer, Item(..)) where

import           Control.Monad
import           Control.Monad.Error    (ErrorT)
import qualified Control.Monad.Error    as CME
import           Control.Monad.IO.Class
import           Control.Monad.Supply
import           Control.Monad.Writer   (WriterT)
import qualified Control.Monad.Writer   as CMW
import           Data.List


{- | The `Replay` monad is parametrized over two types:

 * q - The type of questions (usually `String`)

 * r - The type of answers (usually `String`)
-}
newtype Replay q r a = R { replay :: ErrorT q (WriterT (Trace r) (SupplyT (Item r) IO)) a }
                     deriving (Monad, MonadSupply (Item r), MonadIO
                              , CME.MonadError q, CMW.MonadWriter (Trace r))

type Trace r = [Item r]
type Question = String
type Answer = String
data Item r = Answer r
            | Result String
            | Cut Bool String
            deriving (Show,Read)

supplyM :: MonadSupply s m => m (Maybe s)
supplyM = do
  x <- exhausted
  if x then return Nothing else
      liftM Just supply

lift :: (CME.Error q, Show a, Read a) => IO a -> Replay q r a
lift a = do
  x <- supplyM
  case x of
    Nothing -> do
      res <- liftIO a
      CMW.tell [Result (show res)]
      return res
    Just (Result r) -> return (read r)
    Just _          -> error "Expecting result"

ask  :: (CME.Error q) => q -> Replay q r r
ask q = do
  x <- supplyM
  case x of
    Nothing -> CME.throwError q
    Just (Answer a) -> return a
    Just _          -> error "Expecting answer"

cut :: (Read a, Show a, CME.Error q) => Replay q r a -> Replay q r a
cut m = do
  x <- supplyM
  case x of
    Just (Cut True r)  -> return (read r)
    Just (Cut False _) -> do
      res <- CMW.censor (const []) m
      CMW.tell [Cut True (show res)]
      return res
    _                  -> do
      CMW.tell [Cut False ""]
      res <- CMW.censor (const []) m
      CMW.tell [Cut True (show res)]
      return res

emptyTrace :: Trace r
emptyTrace = []

addAnswer  :: Trace r -> r -> Trace r
addAnswer tr t = tr ++ [Answer t]

run :: Replay q r a -> Trace r -> IO (Either q a, Trace r)
run r tr = do
  (res, tr') <- evalSupplyT (CMW.runWriterT (CME.runErrorT $ replay r)) tr
  return (res, filterCuts (tr ++ tr'))

isFinal :: Item r -> Bool
isFinal (Cut x _) = x
isFinal _         = False

filterCuts :: Trace r -> Trace r
filterCuts xs =
  case findIndex isFinal xs of
    Nothing -> xs
    Just i  -> filterCuts' xs i

filterCuts' :: Trace r -> Int -> Trace r
filterCuts' [] _ = []
filterCuts' ((Cut False _):xs) i = filterCuts (drop (i-1) xs)
filterCuts' (x:xs) i = x:filterCuts' xs (i-1)


