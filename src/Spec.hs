module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test Integrante 1" $ do
    it "Punto de Integrante 1 funciona correctamente" $ do
      danioPotencialAmenaza mojojojo `shouldBe` 64   
      danioPotencialAmenaza princesa `shouldBe` 89 
      danioPotencialAmenaza bandaGangrena `shouldBe` 40   
  describe "Test Integrante 2" $ do
    it "Punto de Integrante 2 funciona correctamente" $ do
       amenazaPuedeAtacarCiudad bandaGangrena saltadilla `shouldBe` False
       amenazaPuedeAtacarCiudad mojojojo saltadilla `shouldBe` True
       amenazaPuedeAtacarCiudad princesa saltadilla `shouldBe` True
  describe "Test Integrante 3" $ do
    it "Punto de Integrante 3 funciona correctamente" $ do
       puedeVencerAmenaza burbuja princesa `shouldBe` False
       propositoEsPar mojojojo `shouldBe` True
       puedeVencerAmenaza bombon mojojojo `shouldBe` True
  describe "Test Integrante 4" $ do
    it "Punto de Integrante 4 funciona correctamente" $ do
       (tieneKryptonita.debilidades) bandaGangrena `shouldBe` True
       amenazaDeNivelAlto princesa `shouldBe` True
       amenazaDeNivelAlto bandaGangrena `shouldBe` False
       
