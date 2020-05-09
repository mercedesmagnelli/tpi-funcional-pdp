module Taller where

type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)
type Costo = Int
type Reparacion = (Auto -> Auto)
 
-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year
 
data Auto = Auto {
 patente :: Patente,
 desgasteLlantas :: [Desgaste],
 rpm :: Float,
 temperaturaAgua :: Float,
 ultimoArreglo :: Fecha
} deriving (Show, Eq)

--- Punto 1 ---

costoDeReparacion :: Auto -> Costo
costoDeReparacion auto
 | longitudDeLaPatente auto == 7 = 12500
 | (patenteEstaEnRango.patente) auto = calculoPatental auto
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

--- Punto 2 ---
--- Parte A: Matías ---

esPeligroso :: Auto -> Bool
esPeligroso = (> 0.5).primeraLlanta.desgasteLlantas

primeraLlanta :: [Desgaste] -> Desgaste
primeraLlanta = head

--- Parte B: Mercedes --- 

necesitaRevision :: Auto -> Bool
necesitaRevision = (<= 2015).anio.ultimoArreglo

--- Punto 3 ---
--- Primera parte A: Matías ---

data Mecanico = Mecanico {
    nombre :: String,
    reparacion :: Reparacion
}

alfa = Mecanico "Alfa" regularRevoluciones
bravo = Mecanico "Bravo" cambiarCubiertas
charly = Mecanico "Charly" regularRevolucionesYCambiarCubiertas

regularRevoluciones :: Reparacion
regularRevoluciones auto 
 | rpm auto > 2000 = auto {rpm = 2000}
 | otherwise = auto

cambiarCubiertas :: Reparacion
cambiarCubiertas auto = auto {desgasteLlantas = [0,0,0,0]}

regularRevolucionesYCambiarCubiertas :: Reparacion
regularRevolucionesYCambiarCubiertas = cambiarCubiertas.regularRevoluciones

reparar :: Mecanico -> Auto -> Auto
reparar mecanico = reparacion mecanico

