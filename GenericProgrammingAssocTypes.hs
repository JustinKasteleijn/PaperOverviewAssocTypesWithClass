{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Data.Foldable (fold)

data Wattage = Wattage Int

instance Show Wattage where
  show (Wattage x) = show x ++ "W"

instance Semigroup Wattage where
  (Wattage x) <> (Wattage y) = Wattage (x + y)

instance Monoid Wattage where
  mempty = Wattage 0

data EcoHome     = EcoHome

data TVMode = Performance
            | Balanced
            | Eco
            deriving (Show)

data Television = Television {
    mode :: TVMode,
    isOn :: Bool
  }

tvPowerUsage :: Television -> Wattage
tvPowerUsage (Television _ False) = Wattage 0
tvPowerUsage (Television mode _)  = evalMode mode
  where
    evalMode :: TVMode -> Wattage
    evalMode Performance = Wattage 100
    evalMode Balanced    = Wattage 75
    evalMode Eco         = Wattage 50

class SmartHome h where
  data PowerSource h
  data Device      h
  currentConsumption :: h -> Device h -> PowerSource h -> Wattage

instance SmartHome EcoHome where
  data PowerSource EcoHome = Solar [Wattage]
  data Device      EcoHome = TV Television

  currentConsumption :: EcoHome -> Device EcoHome -> PowerSource EcoHome -> Wattage
  currentConsumption EcoHome (TV tv) (Solar gains) =
    let (Wattage solarGains) = fold gains
        (Wattage tvUsage)    = tvPowerUsage tv
     in Wattage (solarGains - tvUsage)

instance Show (PowerSource EcoHome) where
  show (Solar gains) = show (fold gains)

instance Show (Device EcoHome) where
  show (TV tv) = show tv.mode

main :: IO ()
main = do
  let myHome   = EcoHome
  let myTv     = TV (Television Eco True) -- An Eco-friendly TV that is turned ON

  -- 2. Define Solar Gains (a list of Consumption values)
  -- We use fold to sum these up in the instance
  let mySolar  = Solar [Wattage 200, Wattage 150, Wattage 50]

  -- 3. Calculate the current consumption
  -- This triggers the 'SmartHome' instance logic
  let balance = currentConsumption myHome myTv mySolar
  let TV rawTv = myTv

  putStrLn "--- SmartHome Energy Report ---"
  putStrLn $ "TV Mode:      " ++ show (rawTv.mode)
  putStrLn $ "Solar Input:  " ++ show mySolar
  putStrLn $ "TV Usage:     " ++ show (tvPowerUsage rawTv)
  putStrLn $ "Net Balance:  " ++ show balance
