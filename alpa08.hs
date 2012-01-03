--Aufgabe 56
import Data.List

data Tree a = Leaf a | Branch a (Tree a) (Tree a)

testTree :: (Tree Int)
testTree = Branch 29 (Branch 14 (Leaf 2) (Leaf 19)) (Branch 32 (Leaf 30) (Leaf 100))

listTree :: Ord a => (Tree a)->[a]
listTree (Branch a b1 b2) = (listTree b1) ++ [a] ++ (listTree b2)
listTree (Leaf a) = [a] 

isSearchTree :: (Tree Int) -> Bool
isSearchTree b = listTree b == sort (listTree b)
