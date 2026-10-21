-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Alejandro Villamizar
--  Carnet: 2210439
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):

--  Uso de gemini web para correcion de errores busqueda de funciones desconocidas(isSpace,fromIntegral) y manejo de casos borde
-- tambien manejo de case y uso de listas por comprension

-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt, isSpace)
import Distribution.Compat.Lens (_1)
import Control.Arrow (ArrowChoice(right))

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1
esPalindromo :: String -> Bool
esPalindromo x = comparar(eliminarEspacios x )

comparar :: String -> Bool
comparar [] = True
comparar [_] = True
comparar (x:xs) = toLower x == toLower(last xs) && comparar (init xs)

eliminarEspacios :: String -> String
eliminarEspacios "" = ""
eliminarEspacios (x:xs) 
  | isSpace x = eliminarEspacios xs
  | otherwise = x: eliminarEspacios xs

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez:
--  Esta funcion la recorre completa, llega hasta el ultimo elento y este pasa a ser el mayor y el menor, posteriormente
--  solo vamos haciendo volviendo hacia atras para comparar

mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor [x] = Just (x,x)
mayorYMenor (x:xs) = Just (maximoAxiliar x a,minimoAxiliar x b) where Just (a,b) = mayorYMenor xs

maximoAxiliar ::Ord x => x -> x -> x
maximoAxiliar a b = 
  if a > b then a 
  else b 

minimoAxiliar ::Ord x => x -> x -> x
minimoAxiliar a b = 
  if a > b then b 
  else a   

-- A3

leerHora :: String -> Either String (Int, Int)
leerHora (a:b:':':c:d) = 
  if length d > 1 then Left "formato inválido"
  else if (read [a,b] :: Int) > 23 then Left "hora fuera de rango"
  else if (read ([c]++d) :: Int) > 59 then Left "minutos fuera de rango"
  else Right (read [a,b] :: Int ,read ([c]++d) :: Int)
leerHora _ = Left "formato inválido"

duracion :: String -> String -> Either String Int
duracion a b = case (leerHora a,leerHora b) of 
  (Left error,Right hora) -> Left "formato inválido"
  (Right hora,Left error) -> Left "formato inválido"
  (Right hora1,Right hora2) -> if fst hora2 > fst hora1 then Right (((fst hora2 - fst hora1)*60) + (snd hora2 - snd hora1))
                               else if fst hora1 == fst hora2 then if snd hora2 > snd hora1 then Right(snd hora2 - snd hora1) 
                                                                   else Right(24*60 -(snd hora1-snd hora2) )
                               else Right (((fst hora1 - fst hora2)-24)*(-60) - (snd hora1 - snd hora2))

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral x ys = if null mayores then Nothing 
                           else Just ((foldr (+) 0 mayores) / fromIntegral(length mayores)) where mayores = sumarMayores x ys

sumarMayores :: Double -> [Double] ->  [Double]
sumarMayores _ [] = []
sumarMayores x ys =  [y | y <- ys, y > x]

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo (x,y) = [ (x+r,y+c) | (r,c) <-[(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)], (0 < x+r && x+r <= 8) && (0 < y+c && y+c <= 8) ]

enDosSaltos :: (Int, Int) -> Int
enDosSaltos x = length (eliminarRepeticion movimientos) where movimientos = concatMap movimientosCaballo (movimientosCaballo x)

eliminarRepeticion :: (Eq a) => [a] -> [a]
eliminarRepeticion [] = []
eliminarRepeticion (x:xs) = x: eliminarRepeticion(filter (/= x) xs)

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap f xs = foldr ((:) . f) []  xs

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter p xs = foldr ((\x acc -> if p x then x : acc else acc)) [] xs

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl = undefined
