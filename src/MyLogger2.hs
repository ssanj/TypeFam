{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

-- https://github.com/MondayMorningHaskell/HaskellData/blob/db4f1408c5df66bbd82e5bb609d40083ce94dec0/haskell/TypeFamilies.hs

module MyLogger2 (main)  where

import Control.Monad.State.Lazy (StateT, State, evalState, evalStateT)
import Control.Monad.State.Lazy (MonadState(get, put))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Time
import qualified Data.Map as M
import Control.Monad.Trans (lift)
import System.IO (openFile, IOMode(WriteMode), hClose)

class (Monad m) => MyLoggerMonad m where
  type LogState m :: *
  retrieveState :: m (LogState m)
  logString :: String -> m ()


newtype ListWrapper a = ListWrapper (State [String] a)
  deriving (Functor, Applicative, Monad)

instance MyLoggerMonad ListWrapper where
  type LogState ListWrapper = [String]

  retrieveState = ListWrapper get

  logString s = ListWrapper $ do
    prev <- get
    put $ (s : prev)



type TimeMsgMap = M.Map UTCTime String
newtype StampedMessages a = StampedMessages (StateT TimeMsgMap IO a)
 deriving (Functor, Applicative, Monad)

instance MyLoggerMonad StampedMessages where
  type LogState StampedMessages = TimeMsgMap

  retrieveState = StampedMessages get

  logString msg = StampedMessages $ do
    ts <- lift getCurrentTime
    lift (print ts)
    prev <- get
    put (M.insert ts msg prev)

listWrapper :: [String]
listWrapper = runListWrapper produceStringsList

stampWrapper :: IO [String]
stampWrapper = M.elems <$> (runStampWrapper produceStringsMap)

runListWrapper :: ListWrapper a -> a
runListWrapper (ListWrapper action) = evalState action []

produceStringsList :: ListWrapper [String]
produceStringsList = do
  logString "Hello"
  logString "World"
  retrieveState

produceStringsMap :: StampedMessages TimeMsgMap
produceStringsMap = do
  logString "Hello"
  logString "World"
  retrieveState


newtype FileLogger a = FileLogger (ReaderT FilePath IO a)
  deriving (Functor, Applicative, Monad)

-- class (Monad m) => MyLoggerMonad m where
--   type LogState m :: *
--   retrieveState :: m (LogState m)
--   logString :: String -> m ()

instance MyLoggerMonad FileLogger where
  type LogState FileLogger = [String]

  retrieveState = FileLogger $ do
    fp <- ask
    lines <$> lift (readFile fp)

  logString msg = FileLogger $ do
    lift (putStrLn msg)
    fp <- ask
    lift (appendFile fp (msg ++ "\n"))

fileLogger :: IO [String]
fileLogger = runFileLogger productFileLogs "logs.txt"

runFileLogger :: FileLogger a -> FilePath -> IO a
runFileLogger (FileLogger action) fp = do
  h <- openFile fp WriteMode
  hClose h -- this is done to create a writeable file
  runReaderT action fp


productFileLogs :: FileLogger [String]
productFileLogs = do
  logString "Hello"
  logString "World"
  retrieveState

useAnyLogger :: (MyLoggerMonad m) => m (LogState m)
useAnyLogger = do
  logString "Hello"
  logString "World"
  logString "!"
  retrieveState

runStampWrapper :: StampedMessages a -> IO a
runStampWrapper (StampedMessages action) = evalStateT action M.empty

runListGeneric :: [String]
runListGeneric = runListWrapper useAnyLogger

main :: IO ()
main =
  fileLogger >>= mapM_ putStrLn
