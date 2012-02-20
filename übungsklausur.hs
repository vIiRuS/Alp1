import SimpleDB

-- Aufgabe 1.1
sumQuad :: Int -> Int
sumQuad n = foldr (+) 0 (map (^2) [1..n])


-- Aufgabe 1.2
data N = Zero | S (N) deriving (Show)
data Z = Z (N,N)      deriving (Show)

class Rechnen a where
	addition :: a -> a -> a


instance Rechnen N where
	--addition :: N -> N -> N
	addition Zero x = x
	addition x Zero = x
	addition x (S n) = S (addition x (n))

instance Rechnen Z where
	addition (Z (x1, y1)) (Z (x2,y2)) = Z (helpAdd x1 x2, helpAdd y1 y2)
		where 
			helpAdd Zero x = x
			helpAdd x Zero = x
			helpAdd x (S n) = S (helpAdd x (n))

-- Aufgabe 1.3
f :: [[Int]] -> Int
f = sum . map sum

g :: [String] -> [String]
g = map (show . length)

--Aufgabe 3.1
insSort :: Ord a => [a] -> [a]
insSort [] = []
insSort (x:xs) = einfg x (insSort xs)

einfg :: Ord a => a -> [a] -> [a]
einfg x [] = [x]
einfg x (y:ys)
	| x <= y = (x:y:ys)
	| otherwise = y : einfg x ys

