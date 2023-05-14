module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Tests de la primera entrega" $ do
    context "Integrante 1: danioPotencialAmenaza/1" $ do
      it "El danio potencial de mojojojo da 64" $ do
        danioPotencialAmenaza mojojojo `shouldBe` 64   
      it "El danio potencial de princesa da 89" $ do
        danioPotencialAmenaza princesa `shouldBe` 89 
      it "El danio potencial de bandaGangrena da 40" $ do
        danioPotencialAmenaza bandaGangrena `shouldBe` 40   
    context "Integrante 2: amenazaPuedeAtacarCiudad/2" $ do
      it "bandaGangrena no puede atacar saltadilla" $ do
        bandaGangrena `shouldNotSatisfy` amenazaPuedeAtacarCiudad saltadilla
      it "mojojojo puede atacar saltadilla" $ do
        mojojojo `shouldSatisfy` amenazaPuedeAtacarCiudad saltadilla
      it "princesa puede atacar saltadilla" $ do   
        princesa `shouldSatisfy` amenazaPuedeAtacarCiudad saltadilla
    context "Integrante 3: propositoEsPar/1 y puedeVencerAmenaza/2" $ do
      it "El proposito de princesa es par" $ do
        princesa `shouldSatisfy` propositoEsPar
      it "Burbuja no puede vencer a princesa" $ do  
        princesa `shouldNotSatisfy` puedeVencerAmenaza burbuja
      it "El proposito de mojojojo es par" $ do  
        mojojojo `shouldSatisfy` propositoEsPar
      it "bombon puede vencer a mojojojo" $ do  
        mojojojo `shouldSatisfy` puedeVencerAmenaza bombon
    context "Integrante 4: amenazaDeNivelAlto/1" $ do
      it "El nivel de amenaza de princesa es alto" $ do
        princesa `shouldSatisfy` amenazaDeNivelAlto 
      it "El nivel de amenaza de bandaGangrena no es alto" $ do  
        bandaGangrena `shouldNotSatisfy` amenazaDeNivelAlto
       
--Traten de cambiar la estructura en la que plantean los tests, que no sea un simple "it" que diga "Punto de Integrante 2 funciona correctamente", 
--sino que por cada it se haga un testeo y que la descripción diga lo que realmente esta haciendo