module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2 
  describe "Test Integrante 1" $ do
    it "Punto de Integrante 1 funciona correctamente" $ do
      dañoPotencialAmenaza mojojojo `shouldBe` 64     
  describe "Test Integrante 2" $ do
    it "Punto de Integrante 2 funciona correctamente" $ do
       amenazaPuedeAtacarCiudad bandaGangrena saltadilla `shouldBe` False
       amenazaPuedeAtacarCiudad mojojojo saltadilla `shouldBe` True
  describe "Test Integrante 3" $ do
    it "Punto de Integrante 3 funciona correctamente" $ do
       puedeVencerAmenaza silico bandaGangrena `shouldBe` True
       puedeVencerAmenaza burbuja mojojojo `shouldBe` False
       puedeVencerAmenaza beyota mojojojo `shouldBe` True
  describe "Test Integrante 4" $ do
    it "Punto de Integrante 4 funciona correctamente" $ do
       tieneKryptonita (debilidades bandaGangrena) `shouldBe` True
       amenazaDeNivelAlto mojojojo `shouldBe` True
       amenazaDeNivelAlto bandaGangrena `shouldBe` False
       
