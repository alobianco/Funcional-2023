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
       bandaGangrena `shouldNotSatisfy` amenazaPuedeAtacarCiudad saltadilla
       mojojojo `shouldSatisfy` amenazaPuedeAtacarCiudad saltadilla
       princesa `shouldSatisfy` amenazaPuedeAtacarCiudad saltadilla
  describe "Test Integrante 3" $ do
    it "Punto de Integrante 3 funciona correctamente" $ do
       princesa `shouldSatisfy` propositoEsPar
       princesa `shouldNotSatisfy` puedeVencerAmenaza burbuja
       mojojojo `shouldSatisfy` propositoEsPar
       mojojojo `shouldSatisfy` puedeVencerAmenaza bombon
  describe "Test Integrante 4" $ do
    it "Punto de Integrante 4 funciona correctamente" $ do
       princesa `shouldSatisfy` amenazaDeNivelAlto 
       bandaGangrena `shouldNotSatisfy` amenazaDeNivelAlto
       
