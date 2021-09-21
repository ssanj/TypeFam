{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE  AllowAmbiguousTypes #-}
{-# LANGUAGE  FlexibleContexts  #-}


-- https://mmhaskell.com/blog/2019/2/4/why-haskell-v-type-families
-- Unfortunately this didn't quite work

module MyLogger  where

-- import Control.Monad.State.Lazy (StateT)
-- import Control.Monad.State.Lazy (State)
-- import Control.Monad.State.Lazy (MonadState(..))
-- import Control.Monad.State.Lazy (lift)
-- import Control.Monad.State.Lazy (evalStateT)
-- import qualified Data.Map.Strict as M
-- import Data.Time.Clock (UTCTime(..), getCurrentTime)

-- class MyLogger logger where
--   type LoggerMonad logger :: * -> *
--   prevMessage :: logger -> [String]
--   logString :: String -> (LoggerMonad logger) ()

-- newtype ListWrapper = ListWrapper [String]
-- instance MyLogger ListWrapper where
--   type (LoggerMonad ListWrapper) = State ListWrapper
--   prevMessage (ListWrapper msgs) = reverse msgs
--   logString s = do
--     (ListWrapper msgs) <- get
--     put $ ListWrapper (s : msgs)


-- -- newtype StampedMessages = StampedMessages (M.Map UTCTime String)
-- -- instance MyLogger StampedMessages where
-- --   type (LoggerMonad StampedMessages) = StateT StampedMessages IO
-- --   prevMessage (StampedMessages msgs) = M.elems msgs
-- --   logString s = do
-- --     (StampedMessages msgs)  <- get
-- --     currentTime <- lift getCurrentTime
-- --     put $ StampedMessages (M.insert currentTime s msgs)

-- newtype ConsoleLogger = ConsoleLogger [String]
-- instance MyLogger ConsoleLogger where
--   type (LoggerMonad ConsoleLogger) = StateT ConsoleLogger IO

--   prevMessage (ConsoleLogger msgs) = reverse msgs
--   logString s = do
--     (ConsoleLogger msgs) <- get
--     lift $ putStrLn "=================="
--     lift $ putStrLn s
--     lift $ putStrLn "=================="
--     lift $ putStrLn ""
--     put $ ConsoleLogger (s : msgs)


-- firstFunction :: Int -> Int
-- firstFunction = (+1)

-- secondFunction :: Int -> String
-- secondFunction = show

-- runComputations :: (MyLogger m, Monad (LoggerMonad m)) => Int -> (LoggerMonad m) String
-- runComputations input = do
--   logString "Starting Computation!"
--   let x = firstFunction input
--   logString "Finished First Computation!"
--   let y = secondFunction x
--   logString "Finished Second Computation!"
--   return y

-- main :: IO ()
-- main =
--   let comp = runComputations 10
--       console = ConsoleLogger (["Hello", "World"])
--   in do
--       result <- evalStateT comp console
--       putStrLn $ "Result: " <> result
