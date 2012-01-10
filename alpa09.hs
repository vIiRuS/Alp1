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


-- Aufgabe 61: Datenstruktur für ein Wörterbuch (10 Punkte)


-- Aufgabe 62: Vereinigung von Wörterbüchern. (10 Punkte)


-- Aufgabe 63: Bäume (5 Punkte)
data (Ord a) =>
  Suchbaum a b = Leer | Knoten a b (Suchbaum a b) (Suchbaum a b)

b = Knoten 10 "xy" (Knoten 4 "ab" Leer Leer) Leer

height :: Ord a => (Suchbaum a b) -> Int
height Leer = (-1)
height (Knoten a b lb rb)
	| (height lb + 1) > (height rb +1) = (height lb +1)
	| otherwise = (height rb +1)

-- Aufgabe 64: Bäume (10 Punkte)