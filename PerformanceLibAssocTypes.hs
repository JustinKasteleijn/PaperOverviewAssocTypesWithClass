{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

data Bit = Zero | One

class ArrayElem e where
  data Array e
  index :: Array e -> Int -> e

instance ArrayElem Int where
  data Array Int = Wrapper [Int]
  index :: Array Int -> Int -> Int
  index (Wrapper xs) n = xs !! n

instance ArrayElem Bit where
  data Array Bit = End
                 | Cons Bit (Array Bit)
  index :: Array Bit -> Int -> Bit
  index End _         = error "Index out of bounds"
  index (Cons b _) 0  = b
  index (Cons b xs) n = index xs (n - 1)

instance Show (Array Int) where
  show (Wrapper xs) = show xs

showBits :: Array Bit -> String
showBits End = ""
showBits (Cons bit rest) =
  showBit bit ++ showBits rest
 where
    showBit Zero = "0"
    showBit One  = "1"

find :: (ArrayElem e)
     => e
     -> Array e
     -> Bool
find elem arr = undefined

funcThatUsesFind :: (ArrayElem e)
                 => e
                 -> Bool
funcThatUsesFind = undefined

main :: IO ()
main = do
  let bv :: Array Bit
         = Cons One (Cons Zero (Cons One End))
  putStrLn $ "BitVector example: " ++ showBits bv
  putStrLn $ "Index 2 in to the BitVector returns '" ++ showBits (Cons (index bv 1) End) ++ "'"

  let intArr :: Array Int
             = Wrapper [1, 2, 3, 4]
  putStrLn $ "Int array example: " ++ show intArr
  putStrLn $ "Index 2 in to the BitVector returns '" ++ show ((index intArr 1) :: Int) ++ "'"
