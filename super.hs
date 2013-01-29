{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Super where

data Zero
data Succ n
type One = Succ Zero

class Nat a
instance Nat Zero
instance Nat a => Nat (Succ a) 

  

data Less x y where
  LSame :: Less n n
  LT :: Less n (Succ n)
data Between n x y where
  Between :: Less x n -> Less n y -> Between n x y

data Plus x y z where
  PZero :: Nat y => Plus Zero y y
  PSucc :: (Nat x, Nat y, Nat z) => Plus x y z -> Plus (Succ x) y (Succ z)

data Vec n a where
  Empty :: Vec Zero a
  Cons :: Nat n => a -> Vec n a -> Vec (Succ n) a
instance Show a => Show (Vec n a) where
  show a = "fromList " ++ (show $ vecToList a)

data Stream a where
  SCons :: a -> Stream a -> Stream a

data SuperOption n a where
  None :: SuperOption n a
  Some :: Vec l a -> Between l One n -> SuperOption n a
  Lots :: Vec l a -> Less (Succ n) l -> SuperOption n a
  All  :: Stream a -> SuperOption n a

-------- --------

vecToList :: Vec n a -> [a]
vecToList (Empty) = []
vecToList (Cons x xs) = x : vecToList xs

insertionSort :: (Ord a) => Vec l a -> Vec l a
insertionSort Empty = Empty
insertionSort (Cons x xs) = insert x (insertionSort xs) where
  insert :: (Ord a) => a -> Vec l a -> Vec (Succ l) a
  insert x Empty = Cons x Empty
  insert x (Cons y xs) =
    if x > y then
      Cons y $ insert x xs
    else
      Cons x $ Cons y xs

append :: Vec x a -> Vec y a -> Plus x y s -> Vec s a
append Empty y PZero = y
append (Cons x xs) y (PSucc p) = Cons x (append xs y p)

data VecSum x y a where 
  VecSum :: Nat s => Plus x y s -> Vec s a -> VecSum x y a
  
class (Nat x, Nat y) => PlusSucc x y where
  plusSucc :: Plus x y s -> Plus x (Succ y) (Succ s)
instance PlusSucc Zero Zero where  
  plusSucc PZero = PZero
instance (PlusSucc x y) => PlusSucc (Succ x) y where  
  plusSucc (PSucc x) = PSucc $ plusSucc x

class (Nat x, Nat y) => Merge x y where
  merge :: Ord a => Vec x a -> Vec y a -> VecSum x y a
instance Merge Zero Zero where  
  merge Empty Empty = VecSum PZero Empty
instance (Merge a Zero) => Merge (Succ a) Zero where
  merge (Cons v c) Empty = case merge c Empty of
    VecSum prf vec -> VecSum (PSucc prf) (Cons v vec)
instance (PlusSucc Zero a, Merge Zero a) => Merge Zero (Succ a) where
  merge Empty (Cons v c) = case merge Empty c of
    VecSum prf vec -> VecSum (plusSucc prf) (Cons v vec)    
    
instance (PlusSucc a a', Merge a (Succ a'), Merge (Succ a) a') => Merge (Succ a) (Succ a') where
  merge ca@(Cons v c) ca'@(Cons v' c') | v <= v' = case merge c ca' of
    VecSum prf vec -> VecSum (PSucc prf) (Cons v vec)
  merge ca@(Cons v c) ca'@(Cons v' c') = case merge ca c' of
    VecSum prf vec -> VecSum (plusSucc prf) (Cons v' vec)

split :: Vec s a -> Plus x y s -> (Vec x a, Vec y a)
split Empty PZero = (Empty, Empty)
split (Cons x xs) PZero = (Empty, Cons x xs)
split (Cons x xs) (PSucc p) = (Cons x y, z) where (y, z) = split xs p

mergeSort :: (Ord a) => Vec l a -> Vec l a
mergeSort Empty = Empty

sorted :: (Ord a) => Vec l a -> Bool
sorted Empty = True
sorted (Cons x Empty) = True
sorted (Cons x (Cons y xs)) = x <= y && sorted (Cons y xs)

sort :: (Ord a) => SuperOption n a -> SuperOption n a
sort None = None
sort (Some v p) = Some (insertionSort v) p
sort (Lots v p) = Lots (mergeSort v) p

main :: IO ()
main = undefined
