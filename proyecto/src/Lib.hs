module Parcial where
import Text.Show.Functions()

sumar :: Num a => a -> a -> a
sumar = (+)

{--Modelar Personaje, estos tienen: nombre, una cantidad de poder y una lista de victorias 
(léase, las veces que le ganó a alguien). Cada victoria tiene el nombre de su oponente y
el año en el que ocurrió. --}
type Oponente = Nombre
type Ny = Int
type Nombre = String
type Poder = Int
type Victoria = (Oponente,Ny)
data Personaje = Unpersonaje {
nombre :: String,
poder :: Poder,
victorias :: [Victoria],
equipamientos :: [Equipamiento]
} deriving (Show)

iron :: Personaje
iron = Unpersonaje {nombre = "Iron Man", poder = 8000, victorias = [("Mandarin",2013),("Loki",2012),("Hulk",2016)], equipamientos = []}

mandarin :: Personaje
mandarin = Unpersonaje {nombre = "Mandarin",poder = 2000, victorias = [], equipamientos = []}

loki :: Personaje 
loki = Unpersonaje {nombre = "Loki",poder = 1000, victorias = [], equipamientos = []}

thanos :: Personaje
thanos = Unpersonaje {nombre = "Thanos",poder = 10000, victorias = [("Iron man",2019),("Hulk",2019),("Spider man",2019),
("Star lord",2019),("Loki",2019),("Thor",2019)], equipamientos = []}

thor :: Personaje
thor = Unpersonaje {nombre = "Thor",poder = 1000, victorias = [("Hijo de Thanos",2015),("Malekit",2014),("Loki",2012)], equipamientos = []}

hulk :: Personaje
hulk = Unpersonaje {nombre = "Hulk",poder = 5000, victorias = [("Hijo de Thanos",2015),("Loki",2012)], equipamientos = []} 

prueba :: [Personaje]
prueba = [loki,iron,mandarin]

prueba2 :: [Personaje]
prueba2 = [thanos,thor,hulk]

oponente :: Victoria -> Oponente
oponente (oponent,_) = oponent
{--
Modelar entrenamiento, el cual lo realizan un grupo de personajes y 
multiplica el poder de cada uno de ellos por la cantidad de personajes 
que están entrenando al mismo tiempo.--}

entrenamiento :: [Personaje] -> [Personaje]
entrenamiento personajes = map (nuevopoder personajes) personajes

nuevopoder :: [Personaje] -> Personaje -> Personaje
nuevopoder personajes personaje = personaje {poder = poder personaje * length personajes}



{--
Modelar rivalesDignos, que dado un grupo de personajes nos dice quienes 
son rivales para Thanos. Son dignos aquellos personajes que, luego de haber 
entrenado,  tienen un poder mayor a 500 y además alguna de sus victorias se llame 
"Hijo de Thanos".
--}
rivalesDignos :: [Personaje] -> [Personaje]
rivalesDignos personajes = filter esdigno personajes

esdigno :: Personaje -> Bool
esdigno personaje = poder personaje >500 && victoriacrucial personaje

victoriacrucial :: Personaje -> Bool
victoriacrucial personaje = any vencioAhijothanos (victorias personaje)

vencioAhijothanos :: Victoria -> Bool
vencioAhijothanos victoria = oponente victoria == "Hijo de Thanos"

{--la cual dado un año y dos conjuntos de personajes hace 
que cada personaje pelee con su contraparte de la otra 
lista y nos dice quienes son los ganadores. Cuando dos personajes 
pelean, gana el que posee mayor poder y se le agregará la victoria 
del perdedor a su lista de victorias con el año en el que ocurrió--}

guerraCivil :: Ny -> [Personaje] -> [Personaje] -> [Personaje]
guerraCivil ny grupo1 grupo2 = zipWith (nemesisde ny) grupo1 grupo2

nemesisde :: Ny -> Personaje -> Personaje -> Personaje
nemesisde ny personaje personaje2 
 | poder personaje > poder personaje2 = personaje {victorias = victorias personaje ++ [(nombre personaje2, ny)] }
 | otherwise = personaje2 {victorias = victorias personaje2 ++ [(nombre personaje, ny)] }

{--parteB 
Ahora aparecen los equipamientos: estos son objetos de poder que dado un personaje 
modifican sus habilidades de manera extraordinaria. Asimismo, sabemos que los personajes
 tienen varios equipamientos.--}
type Equipamiento = Personaje -> Personaje


{--: si tiene menos de 5 victorias le suma 50 de poder, 
pero si tiene 5 o más le resta 100 de poder. --}
escudo :: Equipamiento
escudo personaje 
 | menosde5 . victorias $ personaje = personaje {poder = poder personaje + 50}
 | otherwise = personaje {poder = poder personaje - 100}

menosde5 :: [Victoria] -> Bool
menosde5 victorias = length victorias <5 

{--trajeMecanizado: devuelve el personaje anteponiendo "Iron" 
al nombre del personaje y le agrega una versión dada al final del mismo. 
  Por ejemplo:
Si el personaje se llama "Groot" y la versión del traje es 2 , su nombre quedaría "Iron Groot V2"
--}

trajeMecanizado :: Int -> Equipamiento
trajeMecanizado version personaje = personaje {nombre = "Iron " ++ nombre personaje ++ "V" ++ show version}

{--stormBreaker: Le agrega "dios del trueno" al final del nombre y 
limpia su historial de victorias ya que un dios es bondadoso.--}

stormBreaker :: Equipamiento
stormBreaker personaje
 | nombre personaje == "Thor" = personaje {nombre = nombre personaje ++ " Dios del Trueno", victorias = []}
 | otherwise = personaje

--Añade a la lista de victorias a todos los extras, y cada uno
-- on un año diferente comenzando con el actual. Considerar que hay incontables extra
-- [("extra numero 1", 2018), ("extra numero 2", 2019), …]
gemadelalma :: Equipamiento
gemadelalma personaje 
 | nombre personaje == "Thanos" = personaje {victorias = victorias personaje ++ victoriasinfinitas}
 | otherwise = personaje

victoriasinfinitas :: [Victoria]
victoriasinfinitas = zip extrasinfinitos [2030..]

extrasinfinitos :: [String]
extrasinfinitos = map (("extra numero " ++). show) [1..]

{--guanteleteInfinito
Aplica todos los equipamientos que sean gemas del infinito al personaje. 
Usar la función sin definirla esGemaDelInfinito la cual recibe un equipamiento
 y nos dice si la misma es o no una gema del infinito. 
--}


esGemaDelInfinito::Equipamiento -> Bool
esGemaDelInfinito = undefined

guanteleteInfinito::Equipamiento
guanteleteInfinito personaje
 |nombre personaje == "Thanos" = aplicarGemasDelInfinito personaje
 | otherwise = personaje

aplicarGemasDelInfinito::Personaje -> Personaje
aplicarGemasDelInfinito personaje = foldl (flip ($)) personaje.gemasDelInfinito $ personaje

gemasDelInfinito::Personaje -> [Equipamiento]
gemasDelInfinito = filter esGemaDelInfinito.equipamientos


--guanteleteInfinito :: Equipamiento
--guanteleteInfinito personaje = personaje {equipamientos = equipamientos personaje ++ }
