{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Data.Foldable (fold)

data Consumption = Wattage Int

instance Show Consumption where
  show (Wattage x) = show x ++ "W"

instance Semigroup Consumption where
  (Wattage x) <> (Wattage y) = Wattage (x + y)

instance Monoid Consumption where
  mempty = Wattage 0

data EcoHome     = EcoHome

data PowerSource = Solar [Consumption]

instance Show PowerSource where
  show (Solar gains) = show (fold gains)

data TVMode = Performance
            | Balanced
            | Eco
            deriving (Show)

data Television = Television {
    mode :: TVMode,
    isOn :: Bool
  }

tvPowerUsage :: Television -> Consumption
tvPowerUsage (Television _ False) = Wattage 0
tvPowerUsage (Television mode _)  = evalMode mode
  where
    evalMode :: TVMode -> Consumption
    evalMode Performance = Wattage 100
    evalMode Balanced    = Wattage 75
    evalMode Eco         = Wattage 50

class SmartHome p d h | h -> p, h -> d where
  currentConsumption :: h -> d -> p -> Consumption

instance SmartHome PowerSource Television EcoHome where
  currentConsumption :: EcoHome -> Television -> PowerSource -> Consumption
  currentConsumption EcoHome tv (Solar gains) =
     let (Wattage solarGain) = fold gains
         (Wattage tvUsage) = tvPowerUsage tv
      in Wattage (solarGain - tvUsage)

main :: IO ()
main = do
  let myHome   = EcoHome
  let myTv     = Television Eco True -- An Eco-friendly TV that is turned ON

  -- 2. Define Solar Gains (a list of Consumption values)
  -- We use fold to sum these up in the instance
  let mySolar  = Solar [Wattage 200, Wattage 150, Wattage 50]

  -- 3. Calculate the current consumption
  -- This triggers the 'SmartHome' instance logic
  let balance = currentConsumption myHome myTv mySolar

  putStrLn "--- SmartHome Energy Report ---"
  putStrLn $ "TV Mode:      " ++ show (myTv.mode)
  putStrLn $ "Solar Input:  " ++ show mySolar
  putStrLn $ "TV Usage:     " ++ show (tvPowerUsage myTv)
  putStrLn $ "Net Balance:  " ++ show balance
