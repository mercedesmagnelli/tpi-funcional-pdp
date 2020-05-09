module Taller where

type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)
type Costo = Int 
 
-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year
 
data Auto = Auto {
 patente :: Patente,
 desgasteLlantas :: [Desgaste],
 rpm :: Float,
 temperaturaAgua :: Float,
 ultimoArreglo :: Fecha
} deriving Show

costoDeReparacion :: Auto -> Costo
costoDeReparacion auto
 | longitudDeLaPatente auto == 7 = 12500
 | {- longitudDeLaPatente auto == 6 && -} (patenteEstaEnRango.patente) auto = calculoPatental auto
 | otherwise = 15000

longitudDeLaPatente :: Auto -> Int
longitudDeLaPatente auto = (length.patente) auto

patenteEstaEnRango :: Patente -> Bool
patenteEstaEnRango patente = patente >= "DJ" && patente <= "NB"

calculoPatental :: Auto -> Int
calculoPatental auto
 | (terminaEnCuatro.patente) auto = ((*3000).(longitudDeLaPatente)) auto
 | otherwise = 20000

terminaEnCuatro :: Patente -> Bool
terminaEnCuatro = ( == '4').(last)

esPeligroso :: Auto -> Bool
esPeligroso = (> 0.5).primeraLlanta.desgasteLlantas

primeraLlanta :: [Desgaste] -> Desgaste
primeraLlanta = head
