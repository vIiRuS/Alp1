-- ----------------------------------------------------------------------------
-- "THE BEER-WARE LICENSE" (Revision 42):
-- <viirus@pherth.net> wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me a beer in return Phillip Thelen
-- ----------------------------------------------------------------------------

-- ALP I - Übungszettel 1

-- Aufgabe I: Dreiecke (10 Punkte) 
--Schreiben Sie eine Funktion zum Testen, ob x, y, z die Seitenlängen eines Dreiecks
--(mit positiver Fläche) sind.
--(Entartete Dreiecke, wo zum Beispiel zwei Ecken zusammenfallen, sind ausgeschossen)

aufgabe1 :: Int -> Int -> Int -> Bool
aufgabe1 x y z = x < ( y + z) && y < (  x + z) && z < ( x + y ) && x > 0 && y > 0 && z > 0


-- Aufgabe II: Negative Argumente (10 Punkte) 
-- Die zweistelligen Funktionen mod x y und div x y bestimmen den Rest von x bei Division durch y, beziehungsweise den ganzzahligen Quotienden der Division unter Vernachlässigung des Restes. 
--  (a)  Finden Sie durch Probieren heraus, was passiert, wenn für x und y auch negative Werte oder 0 eingesetzt werden. 
--  (b)  Welche Beziehung gilt immer (mit wenigen Ausnahmen; mit welchen?) zwischen x - mod x y und div x y? 
-- (c)  Fassen Sie Ihre Ergebnisse zu Aufgabe (a) in möglichst einfache Regeln. 

-- a:
-- x = 0: mod und div liefern für jeden Wert von y den Wert 0 zurück
-- x < 0: rechnet wie zu erwarten
-- y = 0:  mod und div erzeugen den Fehler "*** Exception: divide by zero" (Da Division durch 0 nicht definiert ist)
-- y < 0: 

--  (b) Es gilt  (x - mod x y) == (div x y)*y
--        Ausnahme ist y=0, dann da sind beide Funktionen nicht definiert!

-- c:
-- siehe a



-- Aufgabe III: Überlauf (10 Punkte)
-- Im Gegensatz zum Typ Integer haben Größen vom Typ Int einen Wert von höchstens 2^31 − 1 = 2.147,483.647 beziehungsweise 2^63 − 1 = 9,223.372,036.854,775.807. 
--(a)  Was passiert, wenn bei einer Rechnung mit Größen vom Type Int diese Grenze überschritten wird? 
--(b)  Welches ist der kleinste Wert, der im Typ Int dargestellt werden kann? 

-- a:
-- Das bit, das zum Festlegen des Vorzeichens dient, wird geändert. -> Int läuft über.
overflow :: Int -> Int
overflow x = x

-- b:
-- -2^63 (bei 64 bit), -2^31 (bei 32 bit)



-- Aufgabe IV: Zinseszinsberechnung (10 Punkte) 
-- Die folgende Funktion bestimmte die Zinsen einer Anlage von zinsfuß %. 

zinsen :: Int -> Int -> Float
zinsen kapital zinsfuß = kapital * zinsfuß * 0.01 

-- (a)  Definieren Sie unter Zuhilfenahme von zinsen eine Funktion endwert (mit geeigneten Parametern), die den Wert der Anlage (Kapital+Zinsen) am Ende einer Zinsperiode bestimmt. 
-- (b)  Definieren Sie eine Funktion endwert2, die den Wert nach zwei Zinsperioden be- rechnet, wenn die Zinsen am Ende der ersten Periode wieder angelegt werden. 
-- (c)  Funktioniert es auch mit der folgenden Definition? 
--              zinsen kapital zinsfuß = kapital * zinsfuß / 100

-- a:
endwert kapital zinsfuß = kapital + zinsen kapital zinsfuß

--b:
endwert2 kapital zinsfuß = endwert (endwert kapital zinsfuß) zinsfuß

--c: JA!



-- Aufgabe V: Gleitkommarechnungen (10 Punkte)
-- Vom mathematischen Standpunkt aus müsste die folgende Funktion immer 0 liefern: 

zero:: Float -> Float
zero x = (1/x)*x - 1

-- Wegen Rundungsfehlern ist dies nicht immer der Fall. Finden Sie Werte x, bei denen das Ergebnis von 0 verschieden ist. (Ändert sich das Ergebnis beim Übergang zu Double?) 


--zero 3.4
--zero 3.7
--zero 3.9
-- ...

zero2 :: double -> double
zero2 x = (1/x)*x -1

-- zero 3.7
-- zero 7.4
-- zero 14.8
-- Bei Doulbe entstehen weniger Rundungsfehler als bei Float was an der Größeren Präzision liegt.


-- Aufgabe VI: (10 Punkte) Schreiben Sie eine Funktion von drei Integer-Parametern, die ausgibt, wie viele der Eingabeparameter größer als der Durchschnittswert sind. 

ueberdurchschnitt :: Integer -> Integer -> Integer -> Int
ueberdurchschnitt x y z = length [ i | i <- [x,y,z], i > (div (x+y+z) 3)]

grAvg x y z = length [ a | a <- xs, a*3 > sum xs]
        where xs = [x, y, z]

-- Aufgabe 6
anzUebDurch :: Integer -> Integer -> Integer -> Int
anzUebDurch x y z 
    | (sum [x,y,z])*2 > (maximum [x,y,z] + minimum [x,y,z])*3 = 2
    | (x == y) && (y == z) = 0
    | otherwise = 1

