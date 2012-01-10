module SpeicherAlg(WBUCH,
  finde, -- Eq a => WBUCH a b -> a -> Maybe b
  einf,
  entf,
  leer)
    where

data WBUCH a b =
   Einf (WBUCH a b) a b |
   Entf (WBUCH a b) a |
   Leer
--     deriving Show
-- Woerterbuch wird als algebraischer Datentyp
-- direkt nach der Spezifikation
-- dargestellt.

finde::  Eq a => WBUCH a b -> a -> Maybe b
finde (Einf w s t) x
  | s==x   = Just t
  | otherwise = finde w x

finde (Entf w s) x
  | s==x   = Nothing
  | otherwise = finde w x

finde Leer x = Nothing

einf:: Eq a => WBUCH a b -> a -> b -> WBUCH a b
einf = Einf

entf:: Eq a => WBUCH a b -> a -> WBUCH a b
entf = Entf

leer:: WBUCH a b
leer = Leer

vereinige:: WBUCH a b -> WBUCH a b -> WBUCH a b

-- Folgende Bedingungen sollen gelten:
{-
finde (einf w s t) x
  s==x   = Just t
  otherwise = finde w x

finde (entf w s) x
  s==x   = Nothing
  otherwise = finde w x

finde leer x = Nothing
-}
