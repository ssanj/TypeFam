{-# LANGUAGE FunctionalDependencies      #-}
{-# LANGUAGE FlexibleInstances           #-}

-- https://functor.tokyo/blog/2015-03-14-multi-parameter-type-classes
-- With Functional Deps

module Lib2
    ( someFunc
    ) where

import Data.Text (Text, pack)

someFunc :: IO ()
someFunc = do
    return ()
    print $ plus (5 :: Int) (6 :: Double)
    print $ plus (5.0 :: Double) (6 :: Int)
    -- print $ plus (7 :: Integer) (8 :: Integer)


class Add a b c  | a b -> c where
    plus :: a -> b -> c

instance Add Int Double Double where
    plus x y =  fromIntegral x + y

instance Add Double Int Double where
    plus x y = x + (fromIntegral y)

instance (Num a) => Add a a a where
    plus x y = x + y

