import Test.Hspec
import Taller

autoDeAlf = Auto "BC658GB" [0.5,0.1,0,0.2] 2500 90 (07,05,2020)
autoDeAlfRegulandoA2000 = autoDeAlf {rpm = 2000}
autoDeAlfRegulandoA2000YSinDesgaste = autoDeAlf {rpm = 2000, desgasteLlantas = [0,0,0,0]}

autoDeRasta = Auto "JJU564" [0.7,1.3,0.8,0] 1600 90 (23,01,2018)
autoDeRastaSinDesgaste = autoDeRasta {desgasteLlantas = [0,0,0,0]}

autoDeSanti = Auto "HGB475" [0,0.5,0.3,1] 1800 90 (11, 12, 2013)
autoDeSantiSinDesgaste = autoDeSanti {desgasteLlantas = [0,0,0,0]}

autoDeDani = Auto "XSE376" [1, 1.2, 0.7, 0.8] 2000 90 (08,02,2019)
autoDeDaniSinDesgaste = autoDeDani {desgasteLlantas = [0,0,0,0]}

main :: IO()
main = hspec $ do
   describe "Pruebas de costo de reparación" $ do
      it "El costo de reparación del auto de Alf es $12500" $ do
         costoDeReparacion autoDeAlf `shouldBe` 12500
         
      it "El costo de reparación del auto del Rasta es $18000" $ do
         costoDeReparacion autoDeRasta `shouldBe` 18000
         
      it "El costo de reparacion del auto de Santi es de $20000" $ do
         costoDeReparacion autoDeSanti `shouldBe` 20000
         
      it "El costo de reparacion del auto de Dani es de $15000" $ do
        costoDeReparacion autoDeDani `shouldBe` 15000
   
   describe "Pruebas de autos peligrosos" $ do
      it "El auto de Alf no es peligroso" $ do
         autoDeAlf `shouldNotSatisfy` esPeligroso
      
      it "El auto del Rasta es peligroso" $ do
         autoDeRasta `shouldSatisfy` esPeligroso

   describe "Pruebas necesidad de revision" $ do
      it "El auto de Santi necesita revision" $ do
         autoDeSanti `shouldSatisfy` necesitaRevision
      
      it "El auto del Rasta es peligroso" $ do
         autoDeDani `shouldNotSatisfy` necesitaRevision

   describe "Pruebas de reparacion" $ do
      it "Si Alfa repara el auto de Alf, lo dejará regulando a 2000 rpm " $ do
         (reparacion alfa autoDeAlf) `shouldBe` autoDeAlfRegulandoA2000
      
      it "Si Alfa repara el auto del Rasta, no le hará modificaciones" $ do
         (reparacion alfa autoDeRasta) `shouldBe` autoDeRasta
      
      it "Si Bravo repara el auto de Santi, sus cuatro cubiertas no tendrán desgaste" $ do
         (reparacion bravo autoDeSanti) `shouldBe` autoDeSantiSinDesgaste
      
      it "Si Charly repara el auto de Alf, lo dejará regulando a 2000 rpm y sus cuatro llantas no tendrán desgaste" $ do
         (reparar charly autoDeAlf) `shouldBe` autoDeAlfRegulandoA2000YSinDesgaste
      
      it "Si Charly repara el auto de Dani, sólo dejará sus llantas sin desgaste" $ do
         (reparar charly autoDeDani) `shouldBe` autoDeDaniSinDesgaste
         