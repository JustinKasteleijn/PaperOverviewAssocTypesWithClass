{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}

data UIntArr   = UIntArr [Int]

data Bit = Zero | One
data BitVector = End | Cons Bit BitVector

class ArrayElem e arr | e -> arr where
  index :: arr -> Int -> e

instance ArrayElem Int UIntArr where
  index :: UIntArr -> Int -> Int
  index (UIntArr arr) n = arr !! n

instance ArrayElem Bit BitVector where
  index :: BitVector -> Int -> Bit
  index End _ = error "Index out of bounds"
  index (Cons b _) 0  = b
  index (Cons _ bs) n = index bs (n - 1)

instance Show UIntArr where
  show (UIntArr xs) = show xs

showBits :: BitVector -> String
showBits End = ""
showBits (Cons bit rest) =
  showBit bit ++ showBits rest
 where
    showBit Zero = "0"
    showBit One  = "1"

find :: (ArrayElem e arr)
     => e
     -> arr
     -> Bool
find elem arr = undefined

funcThatUsesFind :: (ArrayElem e arr)
                 => e
                 -> Bool
funcThatUsesFind = undefined

main :: IO ()
main = do
  let bv :: BitVector
         = Cons One (Cons Zero (Cons One End))
  putStrLn $ "BitVector example: " ++ showBits bv
  putStrLn $ "Index 2 in to the BitVector returns '" ++ showBits (Cons (index bv 1) End) ++ "'"

  let intArr :: UIntArr
             = UIntArr [1, 2, 3, 4]
  putStrLn $ "Int array example: " ++ show intArr
  putStrLn $ "Index 2 in to the BitVector returns '" ++ show ((index intArr 1) :: Int ) ++ "'"
