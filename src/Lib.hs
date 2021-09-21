{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}

-- https://cdepillabout.github.io/haskell-type-families-presentation/#/1
-- https://functor.tokyo/blog/2015-03-14-multi-parameter-type-classes

module Lib
    ( someFunc
    ) where

import Data.Text (Text, pack)

someFunc :: IO ()
someFunc = do
    print $ plus (5 :: Int) (6 :: Double)
    print $ plus (5.0 :: Double) (6 :: Int)
    print $ plus (7 :: Integer) (8 :: Integer)


class Add a b where
    type SumType a b
    plus :: a -> b -> SumType a b

instance Add Int Double where
    type SumType Int Double = Double
    plus x y =  fromIntegral x + y

instance Add Double Int where
    type SumType Double Int = Double
    plus x y = x + (fromIntegral y)


instance (Num a) => Add a a where
    type SumType a a = a
    plus x y = x + y


data Creds a = Creds a

data BrowserId
data Auth2

credsTextIdent :: Text -> Maybe Text
credsTextIdent = undefined

credsStringIdent :: String -> Maybe String
credsStringIdent = undefined

class YesodAuth master where
    type AuthId master
    getAuthId :: Creds master -> Maybe (AuthId master)

instance YesodAuth BrowserId where
    type AuthId BrowserId = Text
    getAuthId (Creds t) = credsTextIdent . pack $ "blee"

instance YesodAuth Auth2 where
    type AuthId Auth2 = String
    getAuthId (Creds s) = credsStringIdent "blah"
