import Test.Hspec
import Taller

autoDeAlf = Auto "BC658GB" [0.5,0.1,0,0.2] 1700 90 (07,05,2020)
autoDeRasta = Auto "JJU564" [0.7,1.3,0.8,0] 1600 90 (23,01,2020)
autoDeSanti = Auto "HGB475" [0,0.5,0.3,1] 1800 90 (11, 12, 2019)
autoDeDani = Auto "XSE376" [1, 1.2, 0.7, 0.8] 1900 90 (08,02,2020)


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

