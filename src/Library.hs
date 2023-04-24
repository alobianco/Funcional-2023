module Library where
import PdePreludat


doble :: Number -> Number
doble numero = numero + numero

------------------DATAS------------------
data ChicasSuperpoderosas = ChicasSuperpoderosas {
    nombre :: String,
    nivelResistencia :: Number,
    habilidades :: [String],
    amigos :: [String]
} deriving (Show)

data Amenaza = Amenaza{
    objetivo :: [String],
    nivelDePoder :: Number,
    debilidades :: [String]
} deriving (Show)

data Ciudad = Ciudad {
    nombreCiudad :: String,
    poblacion :: Number
} deriving (Show)

------------------Chicas Superpoderosas------------------
bombon = ChicasSuperpoderosas {
    nombre = "Bombon",
    nivelResistencia = 55,
    habilidades = ["Escuchar canciones de Luciano Pereyra","Dar golpes fuertes"],
    amigos = ["Senior Cerdo", "Silico"]
}
burbuja = ChicasSuperpoderosas {
    nombre = "Burbuja",
    nivelResistencia = 30,
    habilidades = ["Velocidad","Lanzas burbujas"],
    amigos = ["Seniorita Belo"]
}
beyota = ChicasSuperpoderosas {
    nombre = "Beyota",
    nivelResistencia = 75,
    habilidades = ["Superfuerza","Velocidad"],
    amigos = []
}
senioritaBelo = ChicasSuperpoderosas {
    nombre = "Seniorita Belo",
    nivelResistencia = 10,
    habilidades = ["No mostrar la cara"],
    amigos = ["Burbuja"]
}
seniorCerdo = ChicasSuperpoderosas {
    nombre = "Senior Cerdo",
    nivelResistencia = 0,
    habilidades = [],
    amigos = ["Bellota"]
}
silico = ChicasSuperpoderosas {
    nombre = "Silico",
    nivelResistencia = 29,
    habilidades = ["Tolerar numeros impares"],
    amigos = ["Bombon","Burbuja"]
}

------------------Amenazas------------------
mojojojo = Amenaza{
    objetivo = ["Destruir a las Chicas Superpoderosas"],
    nivelDePoder =70,
    debilidades =["Velocidad","Superfuerza"]
}
princesa = Amenaza{
    objetivo = ["Quiere ser la unica Chica Superpoderosa"],
    nivelDePoder =95,
    debilidades =["Burbujas","Golpes fuertes"]
}
bandaGangrena = Amenaza{
    objetivo = ["Esparcir el caos","Hacer que todos sean flojos y peleen entre ellos"],
    nivelDePoder =49,
    debilidades =["Escuchar canciones de Luciano Pereyra","Kryptonita"]
}

--------------------Ciudades-------------------
saltadilla = Ciudad {
    nombreCiudad = "Saltadilla",
    poblacion = 31
} 

springfield = Ciudad {
    nombreCiudad = "Springfield",
    poblacion = 43
} 

------------------Integrante 1------------------
{- Calcular el daño potencial de una amenaza, el cual se calcula como el nivel de poder, menos el triple de su cantidad de debilidades. -}
dañoPotencial :: Amenaza -> Number 
dañoPotencial amenaza =((+nivelDePoder amenaza).(*(-3)).length.debilidades)amenaza


------------------Integrante 2------------------
{- Modelar todo lo necesario para saber si una amenaza puede atacar una ciudad. 
Si una amenaza tiene un daño potencial mayor al doble del número de habitantes de la ciudad, entonces puede atacar a la ciudad. -}

peligraCiudad :: Ciudad -> Amenaza -> Bool
peligraCiudad ciudad amenaza = (poblacion ciudad*2) < dañoPotencial amenaza

------------------Integrante 3------------------
{-
    Saber si una chica puede vencer a una amenaza. 
    Si tiene longitud de propósito par, ocurre si la resistencia es mayor a la mitad del daño potencial de una amenaza. 
    Si el propósito tiene longitud impar entonces, es suficiente que la resistencia sea mayor al daño potencial.

    
    
-}
longitudProposito :: Amenaza -> Number
longitudProposito amenaza = sum (map length (objetivo amenaza))


propositoPar :: Amenaza -> Bool
propositoPar = even.longitudProposito

puedeVencer chicaSuperpoderosa amenaza
    | propositoPar amenaza && nivelResistencia chicaSuperpoderosa > dañoPotencial amenaza/2 = True
    | not(propositoPar amenaza) && nivelResistencia chicaSuperpoderosa > dañoPotencial amenaza = True
    | otherwise = False

------------------Integrante 4------------------
{- Determinar si una amenaza es de nivel alto, lo que ocurre si tiene una cantidad par de debilidades, 
las mismas no incluyen kryptonita y su daño potencial es mayor a 50.-}