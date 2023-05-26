module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Tests de la primera entrega" $ do
    context "Integrante 1: danioPotencialAmenaza/1" $ do
      it "El danio potencial de Mojo Jojo da 64" $ do
        danioPotencialAmenaza mojojojo `shouldBe` 64   
      it "El danio potencial de Princesa da 89" $ do
        danioPotencialAmenaza princesa `shouldBe` 89 
      it "El danio potencial de Banda Gangrena da 40" $ do
        danioPotencialAmenaza bandaGangrena `shouldBe` 40   
    context "Integrante 2: amenazaPuedeAtacarCiudad/2" $ do
      it "Banda Gangrena no puede atacar Saltadilla" $ do
        bandaGangrena `shouldNotSatisfy` amenazaPuedeAtacarCiudad saltadilla
      it "Mojo Jojo puede atacar Saltadilla" $ do
        mojojojo `shouldSatisfy` amenazaPuedeAtacarCiudad saltadilla
      it "Princesa puede atacar Saltadilla" $ do   
        princesa `shouldSatisfy` amenazaPuedeAtacarCiudad saltadilla
    context "Integrante 3: propositoEsPar/1 y puedeVencerAmenaza/2" $ do
      it "El proposito de Princesa es par" $ do
        princesa `shouldSatisfy` propositoEsPar
      it "Burbuja no puede vencer a Princesa" $ do  
        princesa `shouldNotSatisfy` puedeVencerAmenaza burbuja
      it "El proposito de Mojo Jojo es par" $ do  
        mojojojo `shouldSatisfy` propositoEsPar
      it "Bombon puede vencer a Mojo Jojo" $ do  
        mojojojo `shouldSatisfy` puedeVencerAmenaza bombon
    context "Integrante 4: amenazaDeNivelAlto/1" $ do
      it "El nivel de amenaza de Princesa es alto" $ do
        princesa `shouldSatisfy` amenazaDeNivelAlto 
      it "El nivel de amenaza de Banda Gangrena no es alto" $ do  
        bandaGangrena `shouldNotSatisfy` amenazaDeNivelAlto
 -- ======================================================================  
 --                         Segunda Parte
 -- ======================================================================
  
{-
 Yendo al Nutricionista casos de prueba: 
- Cualquier chica debe quedarse sin resistencia luego de consumir la sustancia X.
- 4) Burbuja debe tener 20 de resistencia luego de consumir el caramelo líquido. 
- 4)La señorita Belo debe tener 0 de resistencia luego de consumir caramelo líquido.
- Bellota luego de tomar Gatorei queda igual.
- Bombón luego de tomar Gatorei queda con 65 puntos de resistencia.
- Burbuja toma una cerveza con el señor Silico y queda con 2 amigos, ya que no era amiga del mismo.
- Bellota se toma un ferne’ y tiene su nueva habilidad de “Chef de Asados”.
- Burbuja toma una cerveza con la señorita Belo y queda con un amigo, pues ya era amiga de ella.
- Bombón toma 3 shots de vodka y pasa a llamarse “Bom”... por suerte, los otros efectos no los necesitamos  modelar.
- 4)Señor Cerdo le entra a la cocucha y queda igual.
- 4)Burbuja se toma una cocucha y ya no tiene velocidad.
-}

  describe "Tests Yendo al Nutricionista" $ do
      context "Integrante 4" $ do
        it "Burbuja debe tener 20 de resistencia luego de consumir el caramelo líquido. " $ do
          nivelResistencia (consumeCarameloLiquido burbuja) `shouldBe` 20   
        it "La señorita Belo debe tener 0 de resistencia luego de consumir caramelo líquido. " $ do
          nivelResistencia (consumeCarameloLiquido senioritaBelo) `shouldBe` 0   
        it "Señor Cerdo le entra a la cocucha y queda igual. " $ do
          consumeCocucha seniorCerdo `shouldBe` seniorCerdo   
        it "Burbuja se toma una cocucha y ya no tiene velocidad. " $ do
            (head . habilidades) (consumeCocucha burbuja) `shouldNotBe` "Velocidad"
{-
Mi villano Favorito casos de prueba
-Mojo Jojo ataca Saltadilla. Su daño potencial es de 64, por lo que con la fuga se reduce la población en 6, y como efectivamente puede atacar, luego realiza un rumor de un segundo ataque, el cual reduce en otros 6. La población final de Saltadilla es 9.
-Princesa ataca Saltadilla. Su daño potencial es de 89, por lo que reduce la población en 8. Puede atacar a la ciudad, pero no hace nada adicional. La población final de Saltadilla es 13.
-Banda Gangrena ataca Saltadilla. Su daño potencial es de 40, por lo que reduce la población en 4. No puede atacar a la ciudad con su población inicial, por lo que no se aplica su efecto secundario. La población final de Saltadilla es 17.
-Banda Gangrena ataca Saltadilla dos veces consecutivas. El primer ataque no puede realizarse (caso anterior), pero en el segundo intento, que inicia con la población en 17 por la fuga del primero, sí puede atacar. Su segundo ataque deja a la población en 13 por la fuga, y luego (ahora sí) aplica su efecto secundario y duplica su población, quedando la misma en 26.
-}
  describe "Tests de la segunda entrega" $ do
     context "Mi villano favorito: amenazaAtacaCiudad/2" $ do
        it "El ataque de Mojo Jojo a Saltadilla la deja con 9 habitantes restantes" $ do 
          villanoAtacaCiudad mojojojo saltadilla `shouldBe` Ciudad {nombreCiudad = "Saltadilla", cantidadDeHabitantes = 9}
        it "El ataque de Princesa a Saltadilla la deja con 13 habitantes restantes" $ do
          villanoAtacaCiudad princesa saltadilla `shouldBe` Ciudad {nombreCiudad = "Saltadilla", cantidadDeHabitantes = 13}
        it "El ataque de Banda Gangrena a Saltadilla la deja con 17 habitantes restantes" $ do
          villanoAtacaCiudad bandaGangrena saltadilla `shouldBe` Ciudad {nombreCiudad = "Saltadilla", cantidadDeHabitantes = 17}
        it "La Banda Gangrena ataca dos veces consecutivas a Saltadilla. La deja con 26 habitantes y su nombre cambia a Gangrena City" $ do
          villanoAtacaCiudad bandaGangrena (villanoAtacaCiudad bandaGangrena saltadilla) `shouldBe` Ciudad {nombreCiudad = "Gangrena City", cantidadDeHabitantes = 26}

{-
darlePlay casos de prueba
-Cada integrante debe plantear un capítulo de ejemplo distinto y armar un caso de prueba con el mismo, considerando 2 ciudades en total: una afectada por el capítulo y otra que no lo es.
-El grupo debe plantear una temporada y armar dos casos de prueba para una maratón de la misma, con las 2 ciudades.
-Para armar los distintos casos, se pueden usar valores de los puntos anteriores.
-}