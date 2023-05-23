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

{-
 Yendo al Nutricionista casos de prueba: 
- Cualquier chica debe quedarse sin resistencia luego de consumir la sustancia X.
- Burbuja debe tener 20 de resistencia luego de consumir el caramelo líquido. 
- La señorita Belo debe tener 0 de resistencia luego de consumir caramelo líquido.
- Bellota luego de tomar Gatorei queda igual.
- Bombón luego de tomar Gatorei queda con 65 puntos de resistencia.
- Burbuja toma una cerveza con el señor Silico y queda con 2 amigos, ya que no era amiga del mismo.
- Bellota se toma un ferne’ y tiene su nueva habilidad de “Chef de Asados”.
- Burbuja toma una cerveza con la señorita Belo y queda con un amigo, pues ya era amiga de ella.
- Bombón toma 3 shots de vodka y pasa a llamarse “Bom”... por suerte, los otros efectos no los necesitamos  modelar.
- Señor Cerdo le entra a la cocucha y queda igual.
- Burbuja se toma una cocucha y ya no tiene velocidad.
-}

{-
Mi villano Favorito casos de prueba
-Mojo Jojo ataca Saltadilla. Su daño potencial es de 64, por lo que con la fuga se reduce la población en 6, y como efectivamente puede atacar, luego realiza un rumor de un segundo ataque, el cual reduce en otros 6. La población final de Saltadilla es 9.
-Princesa ataca Saltadilla. Su daño potencial es de 89, por lo que reduce la población en 8. Puede atacar a la ciudad, pero no hace nada adicional. La población final de Saltadilla es 13.
-Banda Gangrena ataca Saltadilla. Su daño potencial es de 40, por lo que reduce la población en 4. No puede atacar a la ciudad con su población inicial, por lo que no se aplica su efecto secundario. La población final de Saltadilla es 17.
-Banda Gangrena ataca Saltadilla dos veces consecutivas. El primer ataque no puede realizarse (caso anterior), pero en el segundo intento, que inicia con la población en 17 por la fuga del primero, sí puede atacar. Su segundo ataque deja a la población en 13 por la fuga, y luego (ahora sí) aplica su efecto secundario y duplica su población, quedando la misma en 26.
-}

{-
darlePlay casos de prueba
-Cada integrante debe plantear un capítulo de ejemplo distinto y armar un caso de prueba con el mismo, considerando 2 ciudades en total: una afectada por el capítulo y otra que no lo es.
-El grupo debe plantear una temporada y armar dos casos de prueba para una maratón de la misma, con las 2 ciudades.
-Para armar los distintos casos, se pueden usar valores de los puntos anteriores.
-}