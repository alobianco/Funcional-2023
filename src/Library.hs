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
    habilidades = ["Escuchar canciones de Luciano Pereyra","Dar golpes fuertes"],
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
    proposito = "Destruir a las Chicas Superpoderosas",
    nivelDePoder =70,
    debilidades =["Velocidad","Superfuerza"]
}
princesa = Amenaza{
    proposito = "Ser la unica Chica Superpoderosa",
    nivelDePoder =95,
    debilidades =["Burbujas","Golpes fuertes"]
}
bandaGangrena = Amenaza{
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
amenazaPuedeAtacarCiudad ciudad = ($(*2).cantidadDeHabitantes $ciudad).(>).danioPotencialAmenaza

-- ====================================================================== --
--                 Integrante 3
-- ====================================================================== --
{-
    Saber si una chica puede vencer a una amenaza. 
    Si tiene longitud de propósito par, ocurre si la resistencia es mayor 
    a la mitad del daño potencial de una amenaza. 
    Si el propósito tiene longitud impar entonces, es suficiente que 
    la resistencia sea mayor al daño potencial.    
-}

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
{- Determinar si una amenaza es de nivel alto, lo que ocurre si tiene 
    una cantidad par de debilidades, las mismas no incluyen kryptonita 
    y su daño potencial es mayor a 50.-}


tieneKryptonita = elem "Kriptonita"

amenazaDeNivelAlto :: Amenaza -> Bool
amenazaDeNivelAlto amenaza = operacionDeDebilidades even amenaza
                    && (not.tieneKryptonita.debilidades) amenaza
                    && ((>50).danioPotencialAmenaza) amenaza

