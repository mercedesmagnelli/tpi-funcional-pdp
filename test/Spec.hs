import Test.Hspec
import Taller
-- Autos de prueba para primera parte

autoDeAlf = Auto "BC658GB" [0.5,0.1,0,0.2] 2500 76 (07,05,2014)
autoDeAlfRegulandoA2000 = autoDeAlf {rpm = 2000}
autoDeAlfRegulandoA2000YSinDesgaste = autoDeAlf {rpm = 2000, desgasteLlantas = [0,0,0,0]}
autoDeAlfSinDesgasteEnLasRuedasDelanteras = autoDeAlf {desgasteLlantas = [0,0, 0, 0.2]}

autoDeRasta = Auto "JJU564" [0.7,1.3,0.8,0] 1600 103 (23,01,2018)
autoDeRastaSinDesgaste = autoDeRasta {desgasteLlantas = [0,0,0,0]}

autoDeSanti = Auto "HGB475" [0,0.4,0.3,1] 1800 116 (11, 12, 2013)
autoDeSantiSinDesgaste = autoDeSanti {desgasteLlantas = [0,0,0,0]}

autoDeDani = Auto "XSE376" [1, 1.2, 0.7, 0.8] 2000 70 (08,02,2019)
autoDeDaniSinDesgaste = autoDeDani {desgasteLlantas = [0,0,0,0]}
autoDeDaniConTempA90YLlantasDelanterasSinDesgaste = autoDeDani {desgasteLlantas = [0,0,0.7,0.8], temperaturaAgua = 90}

-- Autos de prueba para la segunda parte

listaDeAutos1 = [autoDeDani, autoDeAlf] --cumple / cumple alf
listaDeAutos2 = [autoDeDani, autoDeSanti] -- no cumple / 
listaDeAutos3 = [autoDeRasta, autoDeAlf] -- no cumple
listaDeAutos4 = [autoDeDani] -- cumple // da 0
listaDeAutos5 = [autoDeAlf] --  no 
listaDeAutos8 = []
listaDeAutos9 = [autoDeDani, autoDeRasta, autoDeAlfArregladoSegunOrdenReparacion1]
listaDeAutos11 = [autoDeSanti, autoDeRastaConFechaModificada]
listaDeAutos12= [autoDeAlfArregladoSegunOrdenReparacion1, autoDeAlf, autoDeRastaConFechaModificada, autoDeRasta ]

autoDeAlfArregladoSegunOrdenReparacion1 = Auto "BC658GB" [0,0,0,0] 2000 90 (27,05,2020)
autoDeRastaArregladoSegunOrdenReparacion2 = Auto "JJU564" [0.7,1.3,0.8,0] 1600 103 (04,06,2020)
autoDeRastaConFechaModificada = Auto "JJU564" [0.7,1.3,0.8,0] 1600 103 (23,01,2014)
ordenReparacion1 = UnaOrdenReparacion (27, 05, 2020) [charly, zulu]
ordenReparacion2 = UnaOrdenReparacion (04, 06, 2020) [alfa, tango]


listaMecanicos1 = [alfa, tango]
listaMecanicos2 = [alfa, bravo, charly, tango, zulu, lima]
listaMecanicos2' = ["Alfa", "Bravo", "Charly", "Tango", "Zulu", "Lima"]
listaMecanicos3 = [bravo, charly, zulu, lima]
listaMecanicos3' = ["Bravo", "Charly", "Zulu", "Lima"]
listaVaciaDeMecanicos = []



main :: IO()   
main = hspec $ do

-- Tests correspondientes a la primera parte:

   describe "Pruebas de costo de reparación" $ do
      it "El costo de reparación de un auto cuya aptente tiene 7 dígitos es $12500" $ do
         costoDeReparacion autoDeAlf `shouldBe` 12500
         
      it "El costo de reparación de un auto cuya patente tiene 6 dígitos, está entre DJ y NB, y termina en 4 es $18000" $ do
         costoDeReparacion autoDeRasta `shouldBe` 18000
         
      it "El costo de reparacion de un auto cuya patente tiene 6 dígitos, está entre DJ y NB y no termina en 4 es de $20000" $ do
         costoDeReparacion autoDeSanti `shouldBe` 20000
         
      it "El costo de reparación de un auto cuya patente tiene 6 dígitos y no está ente DJ y NB es de $15000" $ do
        costoDeReparacion autoDeDani `shouldBe` 15000
   
   describe "Pruebas de autos peligrosos" $ do
      it "Un auto con la primera llanta poca desgastada no es peligroso" $ do
         autoDeAlf `shouldNotSatisfy` esPeligroso
      
      it "Un auto con la primera llanta muy desgastada es peligroso" $ do
         autoDeRasta `shouldSatisfy` esPeligroso

   describe "Pruebas necesidad de revision" $ do
      it "Un auto cuyo último arreglo fue hace poco no necesita revisión" $ do
         autoDeSanti `shouldSatisfy` necesitaRevision
      
      it "Un auto cuyo último arreglo fue hace mucho necesita revisión" $ do
         autoDeDani `shouldNotSatisfy` necesitaRevision

   describe "Pruebas de reparación" $ do
      it "Si Alfa repara un auto que regula a más de 2000 rpm, lo dejará regulando a 2000 rpm " $ do
         (reparacion alfa autoDeAlf) `shouldBe` autoDeAlfRegulandoA2000
      
      it "Si Alfa repara un auto que regula a menos 2000 rpm, no le hará modificaciones" $ do
         (reparacion alfa autoDeRasta) `shouldBe` autoDeRasta

      it "Si Alfa repara un auto que regula a 2000 rpm, no le hará modificaciones" $ do
         (reparacion alfa autoDeDani) `shouldBe` autoDeDani    
      
      it "Si Bravo repara un auto, dejará sus cuatro cubiertas sin desgaste" $ do
         (reparacion bravo autoDeSanti) `shouldBe` autoDeSantiSinDesgaste
      
      it "Si Charly repara un auto que regula a más de 2000 rpm, lo dejará regulando a 2000 rpm y sus cuatro llantas no tendrán desgaste" $ do
         (reparar charly autoDeAlf) `shouldBe` autoDeAlfRegulandoA2000YSinDesgaste
      
      it "Si Charly repara un auto que regula a menos 2000 rpm, sólo dejará sus llantas sin desgaste" $ do
         (reparar charly autoDeDani) `shouldBe` autoDeDaniSinDesgaste
      
      it "Si Charly repara un auto que regula a 2000 rpm, sólo dejará sus llantas sin desgaste" $ do
         (reparar charly autoDeDani) `shouldBe` autoDeDaniSinDesgaste
      
      it "Si Tango repara un auto, no le hará modificaciones" $ do
         (reparar tango autoDeSanti) `shouldBe` autoDeSanti

      it "Si Lima repara un auto, sólo dejará sus llantas delanteras sin desgaste" $ do
         (reparar lima autoDeAlf) `shouldBe` autoDeAlfSinDesgasteEnLasRuedasDelanteras
      
      it "Si Zulu repara un auto, dejará sus llantas delanteras sin desgaste y la tempertura a 90" $ do
         (reparar zulu autoDeDani) `shouldBe` autoDeDaniConTempA90YLlantasDelanterasSinDesgaste        

-- Tests correspondientes a la segunda parte:

   describe "Pruebas de lista ordenada" $ do
      it "Una lista con dos autos donde el primero tiene una cantidad de desgaste impar y el segundo par está ordenada según el criterio solicitado" $ do
         listaDeAutos1 `shouldSatisfy` estaOrdenada
         
      it "Una lista con dos autos donde ambos tienen una cantidad de desgaste impar no está ordenada según el criterio solicitado" $ do
         listaDeAutos2 `shouldNotSatisfy` estaOrdenada
         
      it "Una lista con dos autos donde ambos tienen una cantidad de desgaste par no está ordenada según el criterio solicitado" $ do
         listaDeAutos3 `shouldNotSatisfy` estaOrdenada
      
      it "Una lista con un solo auto que tiene una cantidad de desgaste impar está ordenada según el criterio solicitado" $ do
         listaDeAutos4 `shouldSatisfy` estaOrdenada

      it "Una lista con un solo auto que tiene una cantidad de desgaste par no está ordenada según el criterio solicitado" $ do
         listaDeAutos5 `shouldNotSatisfy` estaOrdenada
      
      it "Una lista sin autos está ordenada según el criterio solicitado" $ do
         listaDeAutos8 `shouldSatisfy` estaOrdenada
      
   describe "Pruebas de orden de reparación" $ do
   
      it "Si un auto regulando a menos de 2000 vueltas pasa por la orden de reparación con Alfa y Tango, sólo se actualizará su última fecha de reparación" $ do
         aplicarOrdenReparacion ordenReparacion2 autoDeRasta `shouldBe` autoDeRastaArregladoSegunOrdenReparacion2

      it "Si un auto que regula a menos de 2000 vueltas pasa por la orden de reparación con charly y zulu, su temperatura quedará en 90 y sus cuatro cubiertas no tendrán desgaste" $ do
         aplicarOrdenReparacion ordenReparacion1 autoDeAlf `shouldBe` autoDeAlfArregladoSegunOrdenReparacion1

   describe "Pruebas de mecanicos que dejan un auto en condiciones" $ do

      it "Si un auto que no está en condiciones es reparado por Alfa o Tango, seguirá no estando en condiciones" $ do -- Peligroso -> Peligroso
         losQueLoDejanEnCondiciones listaMecanicos1 autoDeRasta `shouldBe` listaVaciaDeMecanicos
      
      it "Si un auto en condiciones es reparado por cualquier mecánico, seguirá estando en condiciones" $ do -- No peligroso -> No peligroso (es imposible que se dé el caso No peligroso -> Peligroso)
         losQueLoDejanEnCondiciones listaMecanicos2 autoDeSanti `shouldBe` listaMecanicos2'
      
      it "Si un auto que no está en condiciones es reparado Alfa, Bravo, Charly, Tango, Zulu o Lima, sólo Bravo, Charly, Zulu y Lima lo dejarán en condiciones" $ do -- Peligroso -> No peligroso
         losQueLoDejanEnCondiciones listaMecanicos2 autoDeDani `shouldBe` listaMecanicos3'
   
   describe "Pruebas de costo total de revision" $ do

      it "Si a varios autos que no necesitan revision se les calcula el costo total, el mismo será $0 " $ do --ninguno
         costoTotalDeReparacion listaDeAutos9`shouldBe` 0
      
      it "Si dos autos de patentes de 6 letras que necesitan revision y cuyas patentes terminan una en 4 y la otra no, entonces el costo total es $38000 " $ do 
         costoTotalDeReparacion listaDeAutos11 `shouldBe` 38000 -- todos

      it "Si en un conjunto de autos dos con patente 7, uno de ellos necesitando reparacion, y dos con patente de 6, y el que necesita reparacion su patente termina en 4, el costo entonces es de $31500 " $ do 
         costoTotalDeReparacion listaDeAutos12 `shouldBe` 30500 --unos si y otros no 























