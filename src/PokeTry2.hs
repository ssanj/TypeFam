{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

-- https://www.schoolofhaskell.com/user/nubis/type-families-and-pokemon

module PokeTry2 (main) where

import Data.Tuple (swap)

data Fire = Charmander | Charmeleon | Charizard deriving Show
data Water = Squirtle | Wartortle | Blastoise deriving Show
data Grass = Bulbasaur | Ivysaur | Venusaur deriving Show


data FireMove = Ember | FlameThrower | FireBlast deriving Show
data WaterMove = Bubble | WaterGun deriving Show
data GrassMove = VineWhipe deriving Show

class Pokemon pokemon move where
  pickMove :: pokemon -> move

instance Pokemon Fire FireMove where
  pickMove Charmander = Ember
  pickMove Charmeleon = FlameThrower
  pickMove Charizard  = FireBlast

instance Pokemon Water WaterMove where
  pickMove Squirtle = Bubble
  pickMove _ = WaterGun


instance Pokemon Grass GrassMove where
  pickMove _ = VineWhipe


class (Pokemon pokemon move, Pokemon foe foeMove, Show pokemon, Show move, Show foe, Show foeMove) => Battle pokemon move foe foeMove where
  battle :: pokemon -> foe ->  IO (move, foeMove)
  battle pokemon foe = do
    printBattle (show pokemon) (show move) (show foe) (show foeMove) (show pokemon)
    return (move, foeMove)
    where
      move = pickMove pokemon
      foeMove = pickMove foe

instance Battle Water WaterMove Fire FireMove where
instance Battle Fire FireMove Water WaterMove where
  battle a b = fmap swap $ flip battle a b

instance Battle Grass GrassMove Water WaterMove where
instance Battle Water WaterMove Grass GrassMove where
  battle a b = fmap swap $ flip battle a b


instance Battle Fire FireMove Grass GrassMove where
instance Battle Grass GrassMove Fire FireMove where
  battle a b = fmap swap $ flip battle a b


printBattle :: String -> String -> String -> String -> String -> IO ()
printBattle pokemonOne moveOne pokemonTwo moveTwo winner = do
  putStrLn $ pokemonOne <> " used " <> moveOne
  putStrLn $ pokemonTwo <> " used " <> moveTwo
  putStrLn $ "Winner is: " <> winner <> "\n"


main :: IO ()
main = do
  battle Squirtle Charmander :: IO (WaterMove, FireMove)
  battle Charmeleon Wartortle :: IO (FireMove, WaterMove)
  battle Bulbasaur Blastoise :: IO (GrassMove, WaterMove)
  battle Wartortle Ivysaur :: IO (WaterMove, GrassMove)
  battle Charmeleon Ivysaur :: IO (FireMove, GrassMove)
  battle Venusaur Charizard :: IO (GrassMove, FireMove)
  putStrLn "Done Fighting"
