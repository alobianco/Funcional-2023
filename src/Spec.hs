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
  describe "Tests de la segunda entrega" $ do
    describe "Yendo al Nutricionista" $ do
      context "Integrante 1: consumeSustX/1 y consumeCerveza/2" $ do
        it "Cualquier chica debe quedarse sin resistencia luego de consumir la sustancia X." $ do
          map (nivelResistencia.consumeSustX) [burbuja,bombon,bellota,senioritaBelo,seniorCerdo,silico] `shouldSatisfy` all (==0)
        it "Burbuja toma una cerveza con el señor Silico y queda con 2 amigos, ya que no era amiga del mismo." $ do
          (length.amigos.consumeCerveza [silico] $ burbuja) `shouldBe` 2
        it "Burbuja toma una cerveza con la señorita Belo y queda con un amigo, pues ya era amiga de ella." $ do
          (length.amigos.consumeCerveza [senioritaBelo] $ burbuja) `shouldBe` 1
      context "Integrante 2: consumeFerne/1" $ do
        it "Bellota se toma un ferne' y tiene su nueva habilidad de 'Chef de Asados'." $ do
          (head.habilidades) (consumeFerne bellota) `shouldBe` "Chef de Asados"
      context "Integrante 3: consumeGatorei/1 y consumeShotVodka/2" $ do
        it "Bombón luego de tomar Gatorei queda con 65 puntos de resistencia." $ do
          nivelResistencia (consumeGatorei bombon) `shouldBe` 65
        it "Bombón toma 3 shots de vodka y pasa a llamarse 'Bom'." $ do
          nombre (consumeShotVodka 3 bombon) `shouldBe` "Bom"
      context "Integrante 4: consumeCarameloLiquido/1 y consumeCocucha/1" $ do
        it "Burbuja debe tener 20 de resistencia luego de consumir el caramelo líquido. " $ do
          nivelResistencia (consumeCarameloLiquido burbuja) `shouldBe` 20   
        it "La señorita Belo debe tener 0 de resistencia luego de consumir caramelo líquido. " $ do
           nivelResistencia (consumeCarameloLiquido senioritaBelo) `shouldBe` 0   
        it "Señor Cerdo le entra a la cocucha y queda igual." $ do
          consumeCocucha seniorCerdo `shouldBe` seniorCerdo   
        it "Burbuja se toma una cocucha y ya no tiene velocidad. " $ do
          (head.habilidades) (consumeCocucha burbuja) `shouldNotBe` "Velocidad"
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
    describe "¿Vemos uno mas?: darlePlay/2" $do
      context "Capitulo 1, Primera Parte: Princesa intenta atacar Springfield. Bombon falla al detenerla por haber consumido dos caramelos liquidos" $ do
        context "Sin embargo Princesa no es lo suficientemente fuerte para atacar una ciudad de otra animacion como Springfield" $ do
          it "Como resultado la paz sigue en Springfield y nadie se entero de nada (salvo de una niña gritona disfrazada de abeja que tuvo que ser llevada a la carcel estatal por 8 policias o asi dicen los rumores)" $ do
            darlePlay springfield capitulo1 `shouldBe` Ciudad { nombreCiudad = "Springfield" ,cantidadDeHabitantes = 42}
      context "Capitulo 1, Segunda Parte: Princesa intenta atacar Saltadilla" $ do
          context "Princesa no dudó en cambiar de plan al verse incapaz de atacar Springfield, pasando a la acción en Saltadilla." $ do
            it "Pero Saltadilla se mantiene a salvo y sin rumores gracias al poder del guion." $ do
              darlePlay saltadilla capitulo1 `shouldBe` Ciudad {nombreCiudad = "Saltadilla", cantidadDeHabitantes = 21}
      context "Capitulo 2, Primera Parte: Mojo Jojo intenta atacar Saltadilla. Bellota no puede defenderla por haber consumido ferne' y la sustancia X." $ do
        context "Mojo Jojo hace correr el rumor de un segundo ataque, ya que asume que las Chicas Superpoderosas van a acudir más rápidamente y su objetivo es destruirlas." $ do
          it "Como resultado de esto se fuga el doble de población, quedando así solo 9 habitantes en Saltadilla." $ do 
            darlePlay saltadilla capitulo2 `shouldBe` Ciudad {nombreCiudad = "Saltadilla", cantidadDeHabitantes = 9}
      context "Capitulo 2, Segunda Parte: Mojo Jojo intenta atacar Springfield" $ do
          context "Mojo Jojo, insatisfecho al no haber aparecido ninguna Chica Superpoderosa, decide dirigirse a una famosa ciudad llamada 'Springfield'." $ do
            it "Lo que no sabía es que sería incapaz de encontrarla. Nuevamente un capitulo con final feliz... o al menos lo es para Springfield que no se entera de nada." $ do
              darlePlay springfield capitulo2 `shouldBe` Ciudad {nombreCiudad = "Springfield", cantidadDeHabitantes = 50}
      context "Capitulo 3, Primera Parte: La Banda Gangrena lanza su ataque en Springfield seguros de que tendran exito porque Bombon y Bellota no estan para defenderla. Burbuja sabiendo que no puede defender la ciudad sola, se junta con Silico y el Señor Cerdo a tomar unas cervezas y ganar su amistad" $ do
        context "Luego se toma un Gatorei para recuperar su resistencia y vence a la Banda Gangrena." $ do
          it "Gracias al rapido pensar de Burbuja, al tomarse unas cervecitas y hacer nuevos amigos, consigue la resistencia necesaria (gracias al Gatorei del Doctor Bilardo) para salvar a Springfield." $ do 
            darlePlay springfield capitulo3 `shouldBe` Ciudad {nombreCiudad = "Springfield", cantidadDeHabitantes = 50}
      context "Capitulo 3, Segunda Parte: La Banda Gangrena intenta atacar Saltadilla. Apenas habían podido escapar de ser arrestados luego de ser vencidos por Burbuja y sus amigos. Aún estándo con pocas fuerzas para caminar, deciden atacar Saltadilla sabiendo que ahora sí no habrá nadie allí para defenderla." $ do
        context "Sin recorrer la mitad del camino a la ciudad, la Banda Gangrena no puede más y se desploma. ¿En qué pensaban?" $ do
          it "Cuando despiertan se dan cuenta que están tras las rejas. Estaban en Saltadilla, una ciudad en donde reinaba la paz y el orden... Al menos por ahora" $ do
            darlePlay saltadilla capitulo3 `shouldBe` Ciudad {nombreCiudad = "Saltadilla", cantidadDeHabitantes = 21}
      context "Capitulo 4, Primera Parte: Mojo Jojo intenta atacar Saltadilla" $ do
          context "Ante esta situación Bombon se toma una cocucha para no distraerse escuchando a Luciano Pereyra. Se equivoca de botella y toma un poco de sustancia x que la deja sin resistencia." $ do
            it "Como Bombon no puede vencer a Mojo Jojo entonces este logra atacar la ciudad de Saltadilla con exito." $ do 
              darlePlay saltadilla capitulo4 `shouldBe` Ciudad {nombreCiudad = "Saltadilla", cantidadDeHabitantes = 9}
      context "Capitulo 4, Segunda Parte: Mojo Jojo intenta atacar Springfield" $ do
          context "Mojo Jojo estaba cansado de esperar. Habían pasado horas y no había señales de bellota o burbuja. Decide no dejar pasar un segundo más y atacar Springfield, dónde creía se encontraban las otras dos Chicas Superpoderosas" $ do
            context "Él había aprendido de sus errores, y esta vez tenía una máquina que lo teletransportaba directamente a esta ciudad." $ do
              it "Lo que no sabía es que su destino era no poder llegar a Springfield. Al encenderla algo sale mal y su maquina lo lleva a una isla totalmente desierta. ¿Qué será de Mojo Jojo?" $ do
                darlePlay springfield capitulo4 `shouldBe` Ciudad {nombreCiudad = "Springfield", cantidadDeHabitantes = 50}
    describe "¿Vemos uno mas?: maraton/2" $do  
      context "Ahora que tenemos nuestros capítulos podemos hacer la Primera Temporada de la serie!! Hagamos una prueba de cómo quedarían las ciudades al final de la temporada" $ do
        context "Primero vamos con Saltadilla. Se ve afectada por Mojo Jojo en el capítulo 2, quedando con 9 habitantes. Luego tenemos un nuevo ataque de Mojo Jojo en el capítulo 4." $ do 
          it "Resulta que al final de la temporada no quedan habitantes." $ do
            maraton temporada1 saltadilla `shouldBe` Ciudad {nombreCiudad = "Saltadilla", cantidadDeHabitantes = 0}
      context "Puede que tengamos que hacer cambios en el guión, no puede ser que no queden habitantes. ¿Quién aprobó esto?" $ do
        context "Y ahora Springfield. En el capítulo 1, el rumor del ataque de princesa deja a la ciudad con 42 habitantes. Y luego... ¿Nunca más es afectada? Veo un poco de favoritismo aquí." $ do 
          it "Su población al final de la temporada es de 42 habitantes." $ do
            maraton temporada1 springfield `shouldBe` Ciudad {nombreCiudad = "Springfield", cantidadDeHabitantes = 42}