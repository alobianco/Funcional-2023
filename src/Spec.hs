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
       amenazaPuedeAtacarCiudad saltadilla bandaGangrena `shouldBe` False
       amenazaPuedeAtacarCiudad saltadilla mojojojo `shouldBe` True
       amenazaPuedeAtacarCiudad saltadilla princesa `shouldBe` True
  describe "Test Integrante 3" $ do
    it "Punto de Integrante 3 funciona correctamente" $ do
       princesa `shouldSatisfy` propositoEsPar
       puedeVencerAmenaza burbuja princesa `shouldBe` False
       mojojojo `shouldSatisfy` propositoEsPar
       puedeVencerAmenaza bombon mojojojo `shouldBe` True
  describe "Test Integrante 4" $ do
    it "Punto de Integrante 4 funciona correctamente" $ do
       princesa `shouldSatisfy` amenazaDeNivelAlto 
       bandaGangrena `shouldNotSatisfy` amenazaDeNivelAlto
       
