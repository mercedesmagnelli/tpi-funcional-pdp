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

-- Parte 1

--- Punto 1 ---

costoDeReparacion :: Auto -> Costo
costoDeReparacion auto
 | longitudDeLaPatente auto == 7 = 12500
 | (patenteEstaEntre.patente) auto = calculoPatental auto
 | otherwise = 15000

longitudDeLaPatente :: Auto -> Int
longitudDeLaPatente = (length.patente) 

patenteEstaEntre :: Patente -> Bool
patenteEstaEntre patente = patente >= "DJ" && patente <= "NB"

calculoPatental :: Auto -> Int
calculoPatental auto
 | (terminaEnCuatro.patente) auto = ((*3000).(longitudDeLaPatente)) auto
 | otherwise = 20000

terminaEnCuatro :: Patente -> Bool
terminaEnCuatro = ( == '4').(last)

--- Punto 2 ---
--- Parte A: Matías ---

esPeligroso :: Auto -> Bool
esPeligroso = (> 0.5).head.desgasteLlantas

--- Parte B: Mercedes --- 

necesitaRevision :: Auto -> Bool
necesitaRevision = (<= 2015).anio.ultimoArreglo

--- Punto 3 ---
--- Parte A: Matías ---

data Mecanico = Mecanico {
    nombre :: String,
    reparacion :: Reparacion
}

alfa = Mecanico "Alfa" regularRevoluciones
bravo = Mecanico "Bravo" cambiarCubiertas
charly = Mecanico "Charly" tareaDeAlfaYTareaDeBravo

regularRevoluciones :: Reparacion
regularRevoluciones auto = auto {rpm = min 2000 (rpm auto)}

cambiarCubiertas :: Reparacion
cambiarCubiertas auto = auto {desgasteLlantas = replicate 4 0}

tareaDeAlfaYTareaDeBravo :: Reparacion
tareaDeAlfaYTareaDeBravo = tareaDeAlfa.tareaDeBravo

tareaDeAlfa :: Reparacion
tareaDeAlfa = reparacion alfa

tareaDeBravo :: Reparacion
tareaDeBravo = reparacion bravo

--- Parte B: Mercedes ---
tango = Mecanico "Tango" noHaceNada
lima = Mecanico "Lima" cambioCubiertasDelanteras
zulu = Mecanico "Zulu" tareaDeLimaYRevisarTemperatura

noHaceNada :: Reparacion
noHaceNada = id

cambioCubiertasDelanteras :: Reparacion
cambioCubiertasDelanteras auto = auto {desgasteLlantas = [0,0] ++ (llantasTraseras auto)}

llantasTraseras :: Auto -> [Desgaste]
llantasTraseras= (drop 2.desgasteLlantas) 

tareaDeLima :: Reparacion
tareaDeLima = reparacion lima

tareaDeLimaYRevisarTemperatura :: Reparacion
tareaDeLimaYRevisarTemperatura = tareaDeLima.revisarTemperatura

revisarTemperatura :: Reparacion
revisarTemperatura auto = auto {temperaturaAgua = 90}

reparar :: Mecanico -> Auto -> Auto
reparar mecanico = reparacion mecanico

-- Parte 2

-- Punto 4

estaOrdenada :: [Auto] -> Bool
estaOrdenada [x] = criterioImpar x
estaOrdenada [] = True 
estaOrdenada (x:xs) = (criterioImpar x) && (criterioPar (head xs)) && estaOrdenada (tail xs)

criterioImpar :: Auto -> Bool
criterioImpar = odd . cantidadDesgaste 

criterioPar :: Auto -> Bool
criterioPar = even . cantidadDesgaste

cantidadDesgaste :: Auto -> Int
cantidadDesgaste = round . sum . map (* 10) . desgasteLlantas

-- Punto 5

data OrdenReparacion = UnaOrdenReparacion {
    fecha :: Fecha,
    tecnicos :: [Mecanico]
} 

aplicarOrdenReparacion :: OrdenReparacion -> Auto -> Auto
aplicarOrdenReparacion orden = actualizarFecha (fecha orden) . aplicarReparaciones orden

aplicarReparaciones :: OrdenReparacion -> Auto -> Auto
aplicarReparaciones orden auto = foldl (flip reparar) auto (tecnicos orden)

actualizarFecha :: Fecha -> Auto  -> Auto
actualizarFecha fechaActual auto = auto {ultimoArreglo = fechaActual}

-- Punto 6 -- 
-- Parte 1: Matías -- 

losQueLoDejanEnCondiciones :: [Mecanico] -> Auto -> [String]
losQueLoDejanEnCondiciones mecanicos auto = map nombre (filter (loDejaEnCondiciones auto) mecanicos)

loDejaEnCondiciones :: Auto -> Mecanico -> Bool
-- loDejaEnCondiciones auto mecanico = not (esPeligroso (flip reparar auto mecanico))
loDejaEnCondiciones auto = not . esPeligroso . flip reparar auto

costoTotalDeReparacion :: [Auto] -> Costo
costoTotalDeReparacion listaAutos = foldl calculoCostoIndividual  0 (filter necesitaRevision listaAutos)

-- Punto 6 -- 
-- Parte 2: Mercedes --

calculoCostoIndividual :: Costo -> Auto -> Costo
-- calculoCostoIndividual costo auto =  costo + costoDeReparacion auto
calculoCostoIndividual costo = (+ costo).costoDeReparacion

-- Punto 7 --

-- Parte 1: Matías --

{- Supongamos que generamos una lista infinita a partir de la aplicación de la función cycle a la lista [alfa, bravo, charly, tango, zulu, lima] por 
ejemplo. Gracias a la estrategia de evaluación perezosa o lazy evaluation que utiliza Haskell, es posible obtener el primer técnico que deja en 
condiciones a un auto dado, siempre y cuando uno de los técnicos de la lista lo deje en condiciones. De lo contrario Haskell se "colgará".

Por ejemplo, si contamos con la siguiente función y el siguiente auto:

elPrimero :: [a] -> a
elPrimero lista = lista !! 0

autoDePrueba = Auto "XSE376" [1, 1.2, 0.7, 0.8] 2000 70 (08,02,2019)

Sea la siguiente consulta:

elPrimero (losQueLoDejanEnCondiciones (cycle [alfa, bravo, charly, tango, zulu, lima]) autoDePrueba)

Dicha consulta retornará "Bravo", pues no se necesitará conocer la lista (infinita) completa (ni la de mecánicos ni la de los mecánicos que dejan en 
condiciones el auto) para saber cuál es el primero que cumple la condición especificada.

En cambio, si por ejemplo tenemos la siguiente consulta:

elPrimero (losQueLoDejanEnCondiciones (cycle [alfa, tango]) autoDePrueba)

Como ni Alfa ni Tango dejan en condiciones al auto en cuestión, entonces va a suceder lo dicho anteriormente: Haskell tratará de encontrar el primer 
mecánico (de la lista infinita de mecánicos) que deja en condiciones el auto, pero como ninguno de los dos lo hace, la evaluación seguirá realizándose de
forma indefinida. Algo similar (y más fácil de ver) sucederá si por ejemplo se tratara de aplicar la función any a la lista de números naturales con la 
condición (< 0).
-}
