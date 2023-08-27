-- # 2 NUMEROS ENTEROS
--1
--a
sucesor :: Int -> Int
sucesor n = n+1

--b
sumar :: Int -> Int -> Int
sumar n m = n+m

--c
divisionYResto :: Int -> Int -> (Int, Int)
--Precondicion: m no es 0
divisionYResto n m = (div n m , mod n m)

--d
maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) = if(n>m)
                    then n
                    else m 

--2
--EJEMPLO DE EXPRESIONES QUE DENOTAN EL NUMERO 10
-- sumar (maxDelPar (divisionYResto 18 2)) (sucesor 0)
-- sucesor (sumar (maxDelPar (divisionYResto 8 2)) (maxDelPar( 2,5 )) ) 
-- maxDelPar (divisionYResto (sucesor (sumar 18 2)) 2)
-- sucesor (maxDelPar( (divisionYResto 30 (sumar 2 1) )) - 1)


-- # 3 TIPOS ENUMERATIVOS
--1
data Dir = Norte | Sur | Este | Oeste
    deriving Show

--a
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur   = Norte
opuesto Este  = Oeste
opuesto Oeste = Este

--b
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Este  Este  = True
iguales Sur   Sur   = True
iguales Oeste Oeste = True
iguales _     _     = False

--c
--PREC: Oeste no tiene siguiente
siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este  = Sur
siguiente Sur   = Oeste
siguiente _     = error "No tiene siguiente Dir"
-- Es parcial ya que no siempre retorna Dir

--2
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving Show

--a 
primerYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primerYUltimoDia = (Lunes, Domingo)

--b
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes    = True
empiezaConM Miercoles = True
empiezaConM _         = False

--c
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues a b = numeroDeDia a > numeroDeDia b

numeroDeDia :: DiaDeSemana -> Int
numeroDeDia Lunes      = 1
numeroDeDia Martes     = 2
numeroDeDia Miercoles  = 3
numeroDeDia Jueves     = 4
numeroDeDia Viernes    = 5
numeroDeDia Sabado     = 6
numeroDeDia Domingo    = 7

--d
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

--3
--a 
negar :: Bool -> Bool
negar True  = False
negar False = True

--b
implica :: Bool -> Bool -> Bool
implica True b     = b
implica _    _     = True
   
--c
yTambien :: Bool -> Bool -> Bool
yTambien False _ = False
yTambien _     a = a

--d
oBien :: Bool -> Bool -> Bool
oBien False False = False
oBien _     _     = True

-- # 4 REGISTROS
--1
data Persona = P String  Int
                --Nombre Edad
    deriving Show

-- Defino personas para probar funciones
matias = P "Matias" 28
juan   = P "Juan"   50

nombre :: Persona -> String
nombre (P n _) = n 

edad :: Persona -> Int
edad (P _ e) = e

crecer :: Persona -> Persona
crecer (P n e) = P n (e+1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nn (P _ e) = P nn e

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P _ e) (P _ e2) = e>e2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if (esMayorQueLaOtra p1 p2)
                                    then p1
                                    else p2

--2 
data Pokemon = Pk TipoDePokemon  Int
                --TipoDePokemon  Energia 
    deriving Show

data TipoDePokemon = Agua | Fuego | Planta
    deriving Show

data Entrenador = E String Pokemon Pokemon
                  --Nombre Pk      Pk
    deriving Show

-- Definimos algunos pokemones
bulbasaur  = Pk Agua   39
charmander = Pk Fuego  67
squirtle   = Pk Planta 43
pikachu    = Pk Fuego  45

-- y Entrenadores
entrenadorPablo  = E "Pablo"  bulbasaur  charmander
entrenadorAndrea = E "Andrea" squirtle   pikachu
entrenadorJulian = E "Julian" charmander pikachu

--a
superaA :: Pokemon -> Pokemon -> Bool
superaA (Pk t _) (Pk t2 _)   = esTipoSuperior t t2


esTipoSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
esTipoSuperior Agua Fuego   = True
esTipoSuperior Fuego Planta = True
esTipoSuperior Planta Agua  = True
esTipoSuperior _      _     = False

--b
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t (E _ pk1 pk2) = cantidadDelMismoTipo pk1 pk2 t

cantidadDelMismoTipo :: Pokemon -> Pokemon -> TipoDePokemon -> Int
cantidadDelMismoTipo (Pk t1 _) (Pk t2 _) tp =    unoSiCeroSino (esPkMismoTipo t1 tp)
                                               + unoSiCeroSino (esPkMismoTipo t2 tp)

esPkMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esPkMismoTipo Agua   Agua   = True
esPkMismoTipo Fuego  Fuego  = True
esPkMismoTipo Planta Planta = True
esPkMismoTipo _      _      = False    

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino _    = 0

--c
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon ((E _ pk1 pk2), (E _ pk3 pk4)) = pk1:pk2:pk3:pk4:[]

-- # 5 FUNCIONES POLIMORFICAS
--1
--a
loMismo :: a -> a
loMismo x = x

--b
siempreSiete :: a -> Int
siempreSiete _ = 7

--c
swap :: (a, b) -> (b, a)
swap(x, y) = (y, x)

--2
-- Las funciones son polimorficas porque en su definicion no tiene ninguna restriccion de tipo
-- y no tiene alguna operacion de algun tipo especifico.
-- Es decir pueden trabajar con cualquier tipo de dato


-- # 6 PATTERN MATCHING SOBRE LISTAS
--2
estaVacia :: [a] -> Bool
estaVacia [] = True 
estaVacia _  = False

--3
elPrimero :: [a] -> a
elPrimero (x : _) = x
elPrimero _ = error "La lista esta vacia"

--4
sinElPrimero :: [a] -> [a]
sinElPrimero (_ : xs) = xs
sinElPrimero _        = error "La lista no tiene mas de un elemento"

--5
--PREC: La lista no esta vacia y tiene mas de un elemento 
splitHead :: [a] -> (a, [a])
splitHead l = (elPrimero l, sinElPrimero l)

splitHead' :: [a] -> (a, [a])
splitHead' (x:xs) = (x, xs)


