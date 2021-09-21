-- https://www.schoolofhaskell.com/user/nubis/type-families-and-pokemon

module PokeTry1 (main) where

data Fire = Charmander | Charmeleon | Charizard deriving Show
data Water = Squirtle | Wartortle | Blastoise deriving Show
data Grass = Bulbasaur | Ivysaur | Venusaur deriving Show


data FireMove = Ember | FlameThrower | FireBlast deriving Show
data WaterMove = Bubble | WaterGun deriving Show
data GrassMove = VineWhipe deriving Show

pickFireMove :: Fire -> FireMove
pickFireMove Charmander = Ember
pickFireMove Charmeleon = FlameThrower
pickFireMove Charizard  = FireBlast

pickWaterMove :: Water -> WaterMove
pickWaterMove Squirtle = Bubble
pickWaterMove _ = WaterGun

pickGrassMove :: Grass -> GrassMove
pickGrassMove _ = VineWhipe


printBattle :: String -> String -> String -> String -> String -> IO ()
printBattle pokemonOne moveOne pokemonTwo moveTwo winner = do
  putStrLn $ pokemonOne <> " used " <> moveOne
  putStrLn $ pokemonTwo <> " used " <> moveTwo
  putStrLn $ "Winner is: " <> winner <> "\n"


battleWaterVsFire :: Water -> Fire -> IO ()
battleWaterVsFire water fire = do
  printBattle (show water) (moveOne) (show fire) (moveTwo) (show water)
  where
    moveOne = show $ pickWaterMove water
    moveTwo = show $ pickFireMove fire

battleFireVsWater = flip battleWaterVsFire


battleGrassVsWater :: Grass -> Water -> IO ()
battleGrassVsWater grass water = do
  printBattle (show grass) moveOne (show water) moveTwo (show grass)
  where
    moveOne = show $ pickGrassMove grass
    moveTwo = show $ pickWaterMove water

battleWaterVsGrass = flip battleGrassVsWater

battleFireVsGrass :: Fire -> Grass -> IO ()
battleFireVsGrass fire grass = do
  printBattle (show fire) (moveOne) (show grass) (moveTwo) (show fire)
  where
    moveOne = show $ pickFireMove fire
    moveTwo = show $ pickGrassMove grass

battleGrassVsFire = flip battleFireVsGrass

main :: IO ()
main = do
  battleWaterVsFire Squirtle Charmander
  battleFireVsWater Charmeleon Wartortle
  battleGrassVsWater Bulbasaur Blastoise
  battleWaterVsGrass Wartortle Ivysaur
  battleFireVsGrass Charmeleon Ivysaur
  battleGrassVsFire Venusaur Charizard
