-- -----------------------------------------------------------------------------
-- "THE NERD-WARE LICENSE" (Revision 2):
-- [<viirus@pherth.net>] wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me/us a beer, mate or some food in return
-- [Phillip Thelen]
-- -----------------------------------------------------------------------------

-- ALP I - Übungszettel 9

-- Aufgabe 58: Die Ackermannfunktion (10 Punkte)


-- Aufgabe 59: Die Ackermannfunktion (1 Punkt)


-- Aufgabe 60: Wörterbuchoperationen (10 Punkte)
data Wbuch a b =
   Einf (Wbuch a b) a b |
   WLeer
   deriving Show

buch= Einf (Einf (Einf (Einf (WLeer)  1 "test1") 2 "test2") 3 "test3") 4 "test4"

wanzahl :: (Wbuch a b) -> Int
wanzahl (WLeer) = 0
wanzahl (Einf w _ _) = 1 + (wanzahl w)

-- Aufgabe 61: Datenstruktur für ein Wörterbuch (10 Punkte)
data WBuchAlg a b =
   EinfAlg (WBuchAlg a b) a b |
   LeerAlg
   deriving Show

buch2= EinfAlg (EinfAlg (EinfAlg (EinfAlg (LeerAlg)  8 "test1") 2 "test2") 3 "test3") 4 "test4"
buch3= EinfAlg (EinfAlg (EinfAlg (LeerAlg)  5 "test5") 6 "test2") 7 "test3"

wdelete :: (Eq a) => (WBuchAlg a b) -> a -> (WBuchAlg a b)
wdelete LeerAlg _ = LeerAlg
wdelete (EinfAlg w a b) x
	| a == x = w
	| otherwise = EinfAlg (wdelete w x) a b


-- Aufgabe 62: Vereinigung von Wörterbüchern. (10 Punkte)
vereinige:: (Ord a ) => WBuchAlg a b -> WBuchAlg a b -> WBuchAlg a b
vereinige LeerAlg a = a
vereinige (EinfAlg w1 a1 b1) (EinfAlg w2 a2 b2)
	| a1 > a2 = EinfAlg (vereinige w1 (EinfAlg w2 a2 b2)) a1 b1
	| otherwise = EinfAlg (vereinige w2 (EinfAlg w1 a1 b1)) a2 b2

-- Aufgabe 63: Bäume (5 Punkte)
data (Ord a) =>
  Suchbaum a b = Leer | Knoten a b (Suchbaum a b) (Suchbaum a b)

b = Knoten 10 "xy" (Knoten 4 "ab" Leer Leer) Leer

height :: Ord a => (Suchbaum a b) -> Int
height Leer = (-1)
height (Knoten _ _ lb rb) = 1 + max (height lb) (height rb)

-- Aufgabe 64: Bäume (10 Punkte)
data Baum a = BLeer | BKnoten a (Baum a) (Baum a)
	deriving Show

heightBaum :: (Ord a) => (Suchbaum a b) -> Int -> (Baum Int)
heightBaum Leer _ = BLeer
heightBaum (Knoten _ _ lb rb) i= BKnoten i (heightBaum lb (i+1)) (heightBaum rb (i+1))