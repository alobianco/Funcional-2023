module Library where
import PdePreludat

-- ====================================================================== --
--                  MODELADO
-- ====================================================================== --

data Persona = Persona {
    nombre :: String,
    nivelResistencia :: Number,
    habilidades :: [String],
    amigos :: [String]
} deriving (Show)

type ChicaSuperPoderosa = Persona

data Amenaza = Amenaza{
    nombreA :: String,
    proposito :: String,
    nivelDePoder :: Number,
    debilidades :: [String]
} deriving (Show)

data Ciudad = Ciudad {
    nombreCiudad :: String,
    cantidadDeHabitantes :: Number
} deriving (Show)

-- ====================================================================== --
--                 Chicas Super Poderosas
-- ====================================================================== --
bombon = Persona {
    nombre = "Bombon",
    nivelResistencia = 55,
    habilidades = ["Escuchar canciones de Luciano Pereyra","Golpes fuertes"],
    amigos = ["Senior Cerdo", "Silico"]
}
burbuja = Persona {
    nombre = "Burbuja",
    nivelResistencia = 30,
    habilidades = ["Velocidad","Lanzas burbujas"],
    amigos = ["Seniorita Belo"]
}
bellota = Persona {
    nombre = "Bellota",
    nivelResistencia = 75,
    habilidades = ["Superfuerza","Velocidad"],
    amigos = []
}
senioritaBelo = Persona {
    nombre = "Seniorita Belo",
    nivelResistencia = 10,
    habilidades = ["No mostrar la cara"],
    amigos = ["Burbuja"]
}
seniorCerdo = Persona {
    nombre = "Senior Cerdo",
    nivelResistencia = 0,
    habilidades = [],
    amigos = ["Bellota"]
}
silico = Persona {
    nombre = "Silico",
    nivelResistencia = 29,
    habilidades = ["Tolerar numeros impares"],
    amigos = ["Bombon","Burbuja"]
}

-- ====================================================================== --
--                Amenazas
-- ====================================================================== --

mojojojo = Amenaza{
    nombreA = "Mojo Jojo",
    proposito = "Destruir a las Chicas Superpoderosas",
    nivelDePoder =70,
    debilidades =["Velocidad","Superfuerza"]
}
princesa = Amenaza{
    nombreA = "Princesa",
    proposito = "Ser la unica Chica Superpoderosa",
    nivelDePoder =95,
    debilidades =["Burbujas","Golpes fuertes"]
}
bandaGangrena = Amenaza{
    nombreA = "Banda Gangrena",
    proposito = "Esparcir el caos y Hacer que todos sean flojos y peleen entre ellos",
    nivelDePoder =49,
    debilidades =["Escuchar canciones de Luciano Pereyra","Superfuerza", "Kryptonita"]
}

-- ====================================================================== --
--                Ciudades
-- ====================================================================== --
saltadilla = Ciudad {
    nombreCiudad = "Saltadilla",
    cantidadDeHabitantes = 21
}

-- ====================================================================== --
--                 Integrante 1
-- ====================================================================== --
{- Calcular el daño potencial de una amenaza, el cual se calcula como el 
    nivel de poder, menos el triple de su cantidad de debilidades. -}

operacionDeDebilidades :: (Number -> c) -> Amenaza -> c
operacionDeDebilidades operacion = operacion.length.debilidades

danioPotencialAmenaza :: Amenaza -> Number
danioPotencialAmenaza amenaza = (+nivelDePoder amenaza).operacionDeDebilidades (*(-3)) $amenaza

-- ====================================================================== --
--                 Integrante 2
-- ====================================================================== --
{- Modelar todo lo necesario para saber si una amenaza puede atacar una ciudad. 
Si una amenaza tiene un daño potencial mayor al doble del número de 
habitantes de la ciudad, entonces puede atacar a la ciudad. -}

amenazaPuedeAtacarCiudad :: Ciudad -> Amenaza -> Bool
amenazaPuedeAtacarCiudad ciudad = ((<).(*2).cantidadDeHabitantes $ciudad).danioPotencialAmenaza

-- ====================================================================== --
--                 Integrante 3
-- ====================================================================== --
{- Saber si una chica puede vencer a una amenaza. 
    Si tiene longitud de propósito par, ocurre si la resistencia es mayor 
    a la mitad del daño potencial de una amenaza. 
    Si el propósito tiene longitud impar entonces, es suficiente que 
    la resistencia sea mayor al daño potencial.    -}

propositoEsPar :: Amenaza -> Bool
propositoEsPar = even.length.proposito

mitadDeDanio :: Amenaza -> Number
mitadDeDanio = (/2).danioPotencialAmenaza

resistenciaMasQueMitadDeDanio :: ChicaSuperPoderosa -> Amenaza -> Bool
resistenciaMasQueMitadDeDanio chicasuperpoderosa amenaza = nivelResistencia chicasuperpoderosa > mitadDeDanio amenaza
resistenciaMasQueDanio :: ChicaSuperPoderosa -> Amenaza -> Bool
resistenciaMasQueDanio chicasuperpoderosa amenaza = nivelResistencia chicasuperpoderosa > danioPotencialAmenaza amenaza

puedeVencerAmenaza :: ChicaSuperPoderosa -> Amenaza -> Bool
puedeVencerAmenaza chicasuperpoderosa amenaza
    | propositoEsPar amenaza = resistenciaMasQueMitadDeDanio chicasuperpoderosa amenaza
    | not(propositoEsPar amenaza) = resistenciaMasQueDanio chicasuperpoderosa amenaza
    | otherwise = False

-- ====================================================================== --
--                 Integrante 4 
-- ====================================================================== --
tieneKryptonita = elem "Kriptonita"

amenazaDeNivelAlto :: Amenaza -> Bool
amenazaDeNivelAlto amenaza = operacionDeDebilidades even amenaza
                    && (not.tieneKryptonita.debilidades) amenaza
                    && ((>50).danioPotencialAmenaza) amenaza

-- ====================================================================== --
--                 Integrante 1- Entrada en calor
-- ====================================================================== --
 {-Determinar si una amenaza es invulnerable para una chica, lo cual ocurre 
 si la chica no tiene habilidades que sean debilidad de la amenaza-}

chequeoElem :: [String] -> [String] -> [Bool]
chequeoElem [] _ = []
chequeoElem _ [] = []
chequeoElem (x:xs) (y:ys) = elem x (y:ys) : chequeoElem xs (y:ys)
amenazaInvulnerable :: ChicaSuperPoderosa -> Amenaza -> Bool
amenazaInvulnerable chicasuperpoderosa amenaza = not.or $ chequeoElem (habilidades chicasuperpoderosa) (debilidades amenaza)

-- ====================================================================== --
--                 Integrante 2 - Entrada en calor
-- ====================================================================== --
{-Dada una lista de amenazas, determinar cuál es la amenaza preponderante, 
que es aquella que tenga más nivel de poder.-}


--amenazaPreponderante lista = (maximum.map nivelDePoder) lista

amenazaPreponderante amenazas = unwords.map nombreA.filter ((==(maximum.map nivelDePoder) amenazas).nivelDePoder) $amenazas
-- ====================================================================== --
--                 Integrante 3 - Entrada en calor
-- ====================================================================== --
{-Dada una lista de chicas y una amenaza, determinar los nombres de aquellas 
que pueden vencer la amenaza.-}

-- ====================================================================== --
--                 Integrante 4 - Entrada en calor
-- ====================================================================== --
{-En base a una lista de chicas, saber cuáles tienen nombre que empieza 
con B y más de una habilidad.-}

-- ====================================================================== --
--                 Integrante 1- Yendo al nutricionista
-- ====================================================================== --
 {- + La sustancia X la deja sin resistencia.
    + La cerveza siempre se toma con amigos, aunque sean potenciales, así 
    para esta bebida se conoce las personas con las que se está tomando, y 
    las que no sean amigas, se agregan como tales.-}

consumirSustX chicasuperpoderosa = chicasuperpoderosa { nivelResistencia = 0 } 
generarLista [] = []
generarLista (y:ys) = map nombre (y:ys)
sinRepetir [] = []
sinRepetir (x:xs) = x : sinRepetir (filter (/=x) xs)
nuevosAmigos :: [String] -> [Persona] -> [String]
nuevosAmigos (x:xs) [] = x:xs
nuevosAmigos [] (x:xs) = generarLista (x:xs)
nuevosAmigos [] [] = []
nuevosAmigos (x:xs) (y:ys) = sinRepetir (x:xs) ++ generarLista (y:ys)

tomarCerveza :: Persona -> [Persona] -> Persona 
tomarCerveza (Persona nom resis hab amigos) [] = Persona nom resis hab amigos
tomarCerveza (Persona nom resis hab amigos) (x:xs)  = Persona nom resis hab (nuevosAmigos amigos (x:xs))
-- ====================================================================== --
--                 Integrante 2 - Yendo al nutricionista
-- ====================================================================== --
{- + El saborizador tiene diferentes gustos, como "Fresa", "Arándano" y "Cereza", 
entre otros. Un saborizador reduce la resistencia de la Chica que lo consume 
tanto como la cantidad de letras que tenga el sabor. Por ejemplo, si alguien 
toma saborizador de Fresa, su resistencia se reduciría en 5 unidades.
+ El querido ferne’, donde al consumirlo obtiene automáticamente la habilidad 
de “Chef de Asados”. Si ya la tiene, no se agrega.-}

tomarFerne :: Persona -> Persona
tomarFerne persona
    |elem "Chef de Asados"  $ habilidades persona = persona
    |otherwise = persona {habilidades =  "Chef de Asados" : habilidades persona }

-- ====================================================================== --
--                 Integrante 3 - Yendo al nutricionista
-- ====================================================================== --
{- + El Gatorei, cuyo consumo está recomendado por el doctor Bilardo, es una bebida que
recupera 5 puntos de resistencia a la Chica que lo consume por cada amiga que tenga.
+ El vodka afecta seriamente a una Chica Superpoderosa, haciendo que cambie su nombre 
perdiendo la última letra del mismo por cada shot que tome. Si toma más shots que los 
que su nombre acepta según su longitud, simplemente queda con nombre vacío, no hace falta nada especial.-}

-- ====================================================================== --
--                 Integrante 4 - Yendo al nutricionista
-- ====================================================================== --
{- + El caramelo líquido reduce 10 unidades de resistencia a la Chica que lo consume.
+ La cocucha (que sabemos que saca hasta el óxido), le elimina la primera habilidad 
a quien la ingiera. Si no tiene habilidades, no causa ningún efecto.-}

-- ====================================================================== --
--                 Todos - Mi villano favorito
-- ====================================================================== --
{-
Queremos representar el efecto que tiene un villano al atacar una ciudad. Cuando un villano intenta atacar una ciudad, pueda o no y sólo por motivos del rumor, su población escapa según una cantidad igual a la décima parte de su daño potencial (división entera). Además, si el villano efectivamente puede atacar la misma antes de esta fuga, luego de la misma se aplica un efecto adicional que depende del villano. Antes no lo sabíamos, pero los villanos pueden tener distintos efectos:
Mojo Jojo hace correr el rumor de un segundo ataque, ya que asume que las Chicas Superpoderosas van a acudir más rápidamente y su objetivo es destruirlas, por lo que se fuga el doble de población.
Princesa no hace nada, ya que su objetivo es ser la única Chica Superpoderosa y no le interesa tanto afectar a la ciudad.
Banda Gangrena cambia el nombre de la ciudad por “Gangrena City” y duplica a la población, ya que clona a todos los habitantes para que todo sea más caótico.
En ninguno de los casos la población puede quedar negativa, a lo sumo la ciudad quedará desierta (con población de 0).
-}
calculoEvac :: Amenaza -> Number -> (Number -> Number) -> Number
calculoEvac amenaza x f | x - f (div (danioPotencialAmenaza amenaza) 10) >= 0 = x - f (div (danioPotencialAmenaza amenaza) 10)
                        | otherwise = 0
rumorAtaque :: Amenaza -> Ciudad -> Ciudad
rumorAtaque amenaza (Ciudad nom canthabit)  | amenazaPuedeAtacarCiudad (Ciudad nom canthabit) amenaza = efectoSecundario (Ciudad nom canthabit) amenaza
                                            | otherwise = Ciudad nom (calculoEvac amenaza canthabit (*1))
efectoSecundario :: Ciudad -> Amenaza -> Ciudad
efectoSecundario (Ciudad nom canthabit) amenaza | nombreA amenaza == "Mojo Jojo" = Ciudad nom (calculoEvac amenaza canthabit (*2))
                                                | nombreA amenaza == "Banda Gangrena" = Ciudad "Gangrena City" (calculoEvac amenaza canthabit (*1)*2)
                                                | nombreA amenaza == "Princesa" = Ciudad nom (calculoEvac amenaza canthabit (*1))
                                                | otherwise = Ciudad nom (calculoEvac amenaza canthabit (*1))
-- ====================================================================== --
--                 Todos - DarlePlay
-- ====================================================================== --
{-
Para contarnos la historia de las aventuras de estas chicas, esta serie está dividida en capítulos. Un capítulo incluye a un villano, el nombre de una ciudad que pretende atacar, una Chica Superpoderosa que la defiende y alimentos.

Queremos poder darlePlay a un capítulo para saber cómo afecta a una ciudad dada, que puede o no coincidir con la mencionada en el capítulo. Un villano intentará atacar a la ciudad (si es la mencionada en el capítulo) siempre que la Chica Superpoderosa, habiendo consumido sus alimentos, no pueda vencerlo. Como resultado de darlePlay a un capítulo y ciudad, vamos a conocer cómo cambia la ciudad que indicamos.
Aclaración: Un capítulo sólo puede afectar a la ciudad que nombra. Si es otra ciudad y no la que se nombra en el capítulo, entonces no le hace nada..

A su vez, los capítulos están agrupados por temporada. Una temporada no es más que una serie ordenada de capítulos. Como somos grandes maratoneros de series copadas y nos gusta que todo tenga un hilo conductor, queremos saber cómo afecta una temporada a una ciudad. Para mantener el mencionado hilo conductor, la ciudad inicia con el estado en que quedó al finalizar el capítulo anterior.

Se pide modelar el capítulo, la temporada y las funciones darlePlay/2 y maraton/2 que representan, respectivamente, el paso de un capítulo y de una temporada para una ciudad.
-}
