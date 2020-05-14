import Test.Hspec
import Taller

autoDeAlf = Auto "BC658GB" [0.5,0.1,0,0.2] 2500 76 (07,05,2020)
autoDeAlfRegulandoA2000 = autoDeAlf {rpm = 2000}
autoDeAlfRegulandoA2000YSinDesgaste = autoDeAlf {rpm = 2000, desgasteLlantas = [0,0,0,0]}
autoDeAlfSinDesgasteEnLasRuedasDelanteras = autoDeAlf {desgasteLlantas = [0,0, 0, 0.2]}

autoDeRasta = Auto "JJU564" [0.7,1.3,0.8,0] 1600 103 (23,01,2018)
autoDeRastaSinDesgaste = autoDeRasta {desgasteLlantas = [0,0,0,0]}

autoDeSanti = Auto "HGB475" [0,0.5,0.3,1] 1800 116 (11, 12, 2013)
autoDeSantiSinDesgaste = autoDeSanti {desgasteLlantas = [0,0,0,0]}

autoDeDani = Auto "XSE376" [1, 1.2, 0.7, 0.8] 2000 70 (08,02,2019)
autoDeDaniSinDesgaste = autoDeDani {desgasteLlantas = [0,0,0,0]}
autoDeDaniConTempA90YLlantasDelanterasSinDesgaste = autoDeDani {desgasteLlantas = [0,0,0.7,0.8], temperaturaAgua = 90}

main :: IO()
main = hspec $ do
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

   describe "Pruebas de reparacion" $ do
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
