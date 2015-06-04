x :: Integer
x = 8

add8 :: Integer -> Integer
add8 q = q + 8

abc :: Integer -> Integer -> Integer -> Integer
abc a b c = a * b + c

def :: (Integer -> Integer) -> Integer
def f = f 9

-- data type
data Person = 
  Person 
    String -- firstnamw 
    String -- last bname 
    Int --age
  deriving (Eq, Show, Ord)
 
-- sum type
data MaybeInteger =
  IsInteger Integer
  | IsNotInteger String
  deriving (Eq, Show)
  
divide :: Integer -> Integer -> MaybeInteger
divide x y =
  if y == 0
     then
      IsNotInteger "Can't divide by zero"
     else
      IsInteger (x `div` y)

whatdoyouwanttodo :: MaybeInteger -> String
whatdoyouwanttodo (IsInteger p) =
  "yay " ++ show p
whatdoyouwanttodo (IsNotInteger q) =
  q

-- using type variables
data ThisOrThat x y = 
  This x
  | That y
  deriving (Eq, Show)


divide2 :: Integer -> Integer -> ThisOrThat String Integer
divide2 _ 0 = This "Can't divide by 0"
divide2 x y =
    That (x `div` y)

-- type classes: defn then instances. Think interfaces
class Join2 a where
  (<>) :: a -> a -> a

instance Join2 Int where
  (<>) = (+)

instance Join2 [a] where
  (<>) = (++)

data Endo a = Endo (a -> a)
instance Join2 (a -> a) where
  Endo f <> Endo g = Endo (f . g)

collapse :: Join2 => a -> [a] -> a
collapse d [] = d
collapse _ (h:t) = h <> collapse d t
