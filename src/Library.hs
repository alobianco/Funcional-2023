module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

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
    proposito :: [String],
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
    proposito = ["Destruir a las Chicas Superpoderosas"],
    nivelDePoder =70,
    debilidades =["Velocidad","Superfuerza"]
}
princesa = Amenaza{
    proposito = ["Quiere ser la unica Chica Superpoderosa"],
    nivelDePoder =95,
    debilidades =["Burbujas","Golpes fuertes"]
}
bandaGangrena = Amenaza{
    proposito = ["Esparcir el caos","Hacer que todos sean flojos y peleen entre ellos"],
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

calcDebilidadesAmenaza :: Amenaza -> Number
calcDebilidadesAmenaza = (*(-3)).length.debilidades
danioPotencialAmenaza :: Amenaza -> Number 
danioPotencialAmenaza amenaza = ((+nivelDePoder amenaza).calcDebilidadesAmenaza) amenaza

-- ====================================================================== --
--                 Integrante 2
-- ====================================================================== --
{- Modelar todo lo necesario para saber si una amenaza puede atacar una ciudad. 
Si una amenaza tiene un daño potencial mayor al doble del número de 
habitantes de la ciudad, entonces puede atacar a la ciudad. -}

amenazaPuedeAtacarCiudad :: Amenaza -> Ciudad -> Bool
amenazaPuedeAtacarCiudad amenaza ciudad = ((> (cantidadDeHabitantes ciudad * 2)) . danioPotencialAmenaza) amenaza

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
propositoEsPar = even.sum.map length.proposito

puedeVencerAmenaza :: ChicaSuperPoderosa -> Amenaza -> Bool
puedeVencerAmenaza chicasuperpoderosa amenaza
    | propositoEsPar amenaza && (nivelResistencia chicasuperpoderosa > danioPotencialAmenaza amenaza/2) = True
    | not(propositoEsPar amenaza) && (nivelResistencia chicasuperpoderosa > danioPotencialAmenaza amenaza) = True
    | otherwise = False

-- ====================================================================== --
--                 Integrante 4
-- ====================================================================== --
{- Determinar si una amenaza es de nivel alto, lo que ocurre si tiene 
    una cantidad par de debilidades, las mismas no incluyen kryptonita 
    y su daño potencial es mayor a 50.-}

tieneKryptonita :: [String] -> Bool
tieneKryptonita [] = False
tieneKryptonita (x:xs)
  | x == "Kryptonita" = True
  | otherwise = tieneKryptonita xs

amenazaDeNivelAlto :: Amenaza -> Bool
amenazaDeNivelAlto amenaza = (even . length . debilidades) amenaza 
                    && (not.tieneKryptonita.debilidades) amenaza 
                    && ((>50).danioPotencialAmenaza) amenaza

