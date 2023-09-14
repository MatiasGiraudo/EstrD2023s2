-- # 1 RECURSION SOBRE LISTAS
--1
sumatoria :: [Int] -> Int
    -- sumatoria [1,2,3,4,5]
sumatoria []     = 0
sumatoria (n:ns) = n + sumatoria ns

--2
longitud :: [a] -> Int
    --longitud ["Hola","como","estas","?"]
longitud []     = 0
longitud (_:xs) = 1 + longitud xs    

--3
sucesores :: [Int] -> [Int]
    --sucesores [4,6,2,90]
sucesores []     = []
sucesores (n:ns) = n+1 : sucesores ns    

--4
conjuncion :: [Bool] -> Bool
conjuncion []     = True
conjuncion (x:xs) = x && conjuncion xs    

-- --5
disyuncion :: [Bool] -> Bool
disyuncion []     = False
disyuncion (x:xs) = x || disyuncion xs

--6
aplanar :: [[a]] -> [a]
    --aplanar [[1,2],[2],[]]
aplanar []     = []
aplanar (x:xs) = x ++ aplanar xs

--7
pertenece :: Eq a => a -> [a] -> Bool
    --pertenece 4 [1,2,3,4,5,6]
pertenece _ []     = False
pertenece e (x:xs) = esMismoElemento e x || pertenece e xs

esMismoElemento :: Eq a => a -> a -> Bool 
esMismoElemento e1 e2 = e1 == e2                       

--8
apariciones :: Eq a => a -> [a] -> Int     
    --apariciones 'A' ['A', 'J', 'B', 'A']              
apariciones _ []     = 0
apariciones e (x:xs) = unoSi (e == x) + apariciones e xs

unoSi :: Bool -> Int 
unoSi True  = 1
unoSi False = 0                        

--9
losMenoresA :: Int -> [Int] -> [Int]
    --losMenoresA 5 [1,2,2,3,5,6,7,8]
losMenoresA _ []     = []
losMenoresA n (x:xs) = if(n>x) 
                        then x : losMenoresA n xs
                        else losMenoresA n xs

--10
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
    --lasDeLongitudMayorA 2 [[1],[2,5],[6,6,8], [1,2,3,4]]
lasDeLongitudMayorA _ []     = []
lasDeLongitudMayorA n (x:xs) = if(n < (longitud x))
                                then x : lasDeLongitudMayorA n xs
                                else lasDeLongitudMayorA n xs

--11
agregarAlFinal :: [a] -> a -> [a]
    --agregarAlFinal [1,2,3,4,5] 10
agregarAlFinal [] e     = e:[]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

--12
agregar :: [a] -> [a] -> [a]
    --agregar [1,2] [6,7,9]
agregar []     ys = ys 
agregar (x:xs) ys = x : agregar xs ys

--13
reversa :: [a] -> [a]
    --reversa [1,2,3,4,5]
reversa []     = []
reversa (x:xs) = agregarAlFinal (reversa xs) x 

--14
zipMaximos :: [Int] -> [Int] -> [Int]
    --zipMaximos [3,8,5] [9,1,1,8]
zipMaximos []  ms = ms
zipMaximos  ns [] = ns
zipMaximos (n:ns) (m:ms) = max n m : zipMaximos ns ms  

--15
elMinimo :: Ord a => [a] -> a
--PREC: La lista no esta vacía
    --elMinimo [2,6,3,4,78]
elMinimo []     = error "La lista no puede estar vacía"
elMinimo (x:[]) = x
elMinimo (x:xs) = if(x < elMinimo xs)
                    then x
                    else elMinimo xs    

-- # 2 RECURSION SOBRE NUMEROS
--1
factorial :: Int -> Int
    --factorial 3 = 6
factorial 0 = 1
factorial n = if(n < 0)
                then error "El factorial no esta definido para numeros negativos"
                else n * factorial (n-1)

--2
cuentaRegresiva :: Int -> [Int]
    --cuentaRegresiva 5 = [5,4,3,2,1,0]
cuentaRegresiva 0 = 0:[]
cuentaRegresiva n = n : cuentaRegresiva (n-1)

--3
repetir :: Int -> a -> [a]
    --repetir 3 "Hola mundo"
repetir 0 _ = []
repetir n e = e : repetir (n-1) e

--4
losPrimeros :: Int -> [a] -> [a]
    --losPrimeros 3 [4,6,7,2,3]
losPrimeros 0 _      = []
losPrimeros _ []     = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs    
                    
--5
sinLosPrimeros :: Int -> [a] -> [a]
     --sinLosPrimeros 2 [6,4,8,2,9,7]
sinLosPrimeros 0 xs     = xs
sinLosPrimeros _ []     = []
sinLosPrimeros n (x:xs) = sinLosPrimeros(n-1) xs

-- # 3 REGISTROS
--1
data Persona = P String Int
               --Nombre Edad
    deriving Show

marcos  = P "Marcos"  20
matilda = P "Matilda" 30
pedro   = P "Pedro"   58
nahuel  = P "Nahuel"  45

edad :: Persona -> Int
edad (P _ e) = e

--a
mayoresA :: Int -> [Persona] -> [Persona]
    --mayoresA 40 [marcos, matilda, pedro, nahuel]
mayoresA _ []     = []
mayoresA n (x:xs) = if(edad x > n)
                        then x : mayoresA n xs
                        else mayoresA n xs

--b
promedioEdad :: [Persona] -> Int
-- PRECOND: la lista no es vacía
    --promedioEdad [marcos, matilda, pedro, nahuel]
promedioEdad [] = error "No se puede sacar el promedio de una lista de Persona vacia"
promedioEdad ps = div (sumarEdadTodos ps) (longitud ps)

sumarEdadTodos :: [Persona] -> Int
sumarEdadTodos [] = 0
sumarEdadTodos (p:ps) = edad p + sumarEdadTodos ps    

--c
elMasViejo :: [Persona] -> Persona
--PREC: La lista no esta vacía
    --elMasViejo [marcos, matilda, pedro, nahuel]
elMasViejo [x]    = x
elMasViejo (x:xs) = if(edad x > edad (elMasViejo xs))
                        then x 
                        else elMasViejo xs

--2                        
data TipoDePokemon = Agua | Fuego | Planta
data Pokemon    = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

--Generamos datos para pruebas
bulbasaur  = ConsPokemon Agua   39
charmander = ConsPokemon Fuego  67
squirtle   = ConsPokemon Planta 43
pikachu    = ConsPokemon Fuego  45

-- y Entrenadores
entrenadorPablo     = ConsEntrenador "Pablo"  [bulbasaur, charmander]
entrenadorAndrea    = ConsEntrenador "Andrea" [squirtle, pikachu, charmander]
entrenadorJulian    = ConsEntrenador "Julian" [bulbasaur, charmander, pikachu, squirtle]
entrenadorSoloAgua  = ConsEntrenador "SoloAgua" [bulbasaur]
entrenadorSoloFuego = ConsEntrenador "SoloFuego" [charmander, pikachu]

--a
cantPokemon :: Entrenador -> Int
    --cantPokemon entrenadorJulian
cantPokemon e = longitud (pokemonesDe e)

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (ConsEntrenador _ pks) = pks

--b
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
    --cantPokemonDe Agua entrenadorJulian
cantPokemonDe t e = cantidadPokemonDeTipoEn t (pokemonesDe e)

cantidadPokemonDeTipoEn :: TipoDePokemon -> [Pokemon] -> Int 
cantidadPokemonDeTipoEn _ []      = 0
cantidadPokemonDeTipoEn t (p:pks) = unoSi(esMismoTipoPokemon t (tipoPokemon p)) 
                                  + cantidadPokemonDeTipoEn t pks

esMismoTipoPokemon :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipoPokemon Agua   Agua   = True
esMismoTipoPokemon Fuego  Fuego  = True
esMismoTipoPokemon Planta Planta = True
esMismoTipoPokemon _      _      = False 

tipoPokemon :: Pokemon -> TipoDePokemon 
tipoPokemon (ConsPokemon t _) = t

--c
cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
    --cuantosDeTipo_De_LeGananATodosLosDe_ Agua entrenadorSoloAgua entrenadorSoloFuego
cuantosDeTipo_De_LeGananATodosLosDe_ t e1 e2 = cantidadDePkGanan t (pokemonesDe e2) (pokemonesDe e1)

cantidadDePkGanan :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
cantidadDePkGanan _ _           []      = 0
cantidadDePkGanan t pksEnemigos (p:pks) =  if puedeVencerATodos p pksEnemigos
                                            then unoSi (esMismoTipoPokemon t (tipoPokemon p)) + cantidadDePkGanan t pksEnemigos pks
                                            else cantidadDePkGanan t pksEnemigos pks

puedeVencerATodos :: Pokemon -> [Pokemon] -> Bool
puedeVencerATodos _ [] = True
puedeVencerATodos a (p:pks) =  puedeVencer (tipoPokemon a) (tipoPokemon p) && puedeVencerATodos a pks

puedeVencer :: TipoDePokemon -> TipoDePokemon -> Bool
puedeVencer Agua   Fuego  = True
puedeVencer Fuego  Planta = True
puedeVencer Planta Agua   = True
puedeVencer _      _      = False

--d
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon e = poseeAlMenosPkCadaTipo (pokemonesDe e)

poseeAlMenosPkCadaTipo :: [Pokemon] -> Bool
poseeAlMenosPkCadaTipo pks = (hayAlmenosUnPkDe pks Agua) &&
                             (hayAlmenosUnPkDe pks Fuego) &&
                             (hayAlmenosUnPkDe pks Planta) 

hayAlmenosUnPkDe :: [Pokemon] -> TipoDePokemon -> Bool
hayAlmenosUnPkDe [] _      = False
hayAlmenosUnPkDe (p:pks) t = esMismoTipoPokemon (tipoPokemon p) t 
                             || hayAlmenosUnPkDe pks t                                 


--3

data Seniority = Junior | SemiSenior | Senior
    deriving Show
data Proyecto  = ConsProyecto String
    deriving Show
data Rol       = Developer Seniority Proyecto | Management Seniority Proyecto
    deriving Show
data Empresa   = ConsEmpresa [Rol]
    deriving Show

--Generamos datos para pruebas
proyecto1 = ConsProyecto "Proyecto1"
proyecto2 = ConsProyecto "Proyecto2"
proyecto3 = ConsProyecto "Proyecto3"

rolJuniorDev       = Developer Junior proyecto1
rolSemiSeniorDev   = Developer SemiSenior proyecto1
rolSeniorDev       = Developer Senior proyecto2
rolSeniorDev2      = Developer Senior proyecto1

rolJuniorManag     = Management Junior proyecto2
rolSemiSeniorManag = Management SemiSenior proyecto2
rolSeniorManag     = Management Senior proyecto1

--[rolJuniorDev, rolSemiSeniorDev, rolSeniorDev, rolSeniorDev2, rolJuniorManag, rolSemiSeniorManag, rolSeniorManag]

empresa = ConsEmpresa (rolJuniorDev : rolJuniorManag 
                 : rolSemiSeniorDev : rolSemiSeniorManag 
                 : rolSeniorDev     : rolSeniorManag 
                 : rolSeniorDev2    : [])

--a 
proyectos :: Empresa -> [Proyecto]
    --proyectos empresa 
proyectos (ConsEmpresa rs) = proyectosDeRoles rs

proyectosDeRoles :: [Rol] -> [Proyecto]
proyectosDeRoles []     = []
proyectosDeRoles (r:rs) = agregarSinRepetir (proyecto r) (proyectosDeRoles rs)

agregarSinRepetir :: Proyecto -> [Proyecto] -> [Proyecto]
agregarSinRepetir p []     = p:[]
agregarSinRepetir p (x:xs) = if proyectosIguales x p 
                                then agregarSinRepetir p xs
                                else x : agregarSinRepetir p xs

proyectosIguales :: Proyecto -> Proyecto -> Bool
proyectosIguales  p1 p2  = (nombreProyecto p1) == (nombreProyecto p2)                                  

nombreProyecto :: Proyecto -> String
nombreProyecto (ConsProyecto n) = n

proyecto :: Rol -> Proyecto
proyecto (Developer  _ p) = p 
proyecto (Management _ p) = p 


--b
losDevSenior :: Empresa -> [Proyecto] -> Int
    --losDevSenior empresa [proyecto1, proyecto2]
losDevSenior (ConsEmpresa rs) ps = longitud(trabajanEn(losRolesSenior(losDevs rs)) ps)

losDevs :: [Rol] -> [Rol]
losDevs []     = []
losDevs (r:rs) = if esRolDev r 
                    then r : losDevs rs
                    else losDevs rs

esRolDev :: Rol -> Bool
esRolDev (Developer _ _) = True
esRolDev  _              = False

losRolesSenior :: [Rol] -> [Rol]
losRolesSenior []     = []
losRolesSenior (r:rs) = if esRolSenior r 
                         then r : losRolesSenior rs
                         else losRolesSenior rs

esRolSenior :: Rol -> Bool
esRolSenior (Developer  s _) = esSenior s
esRolSenior (Management s _) = esSenior s

esSenior :: Seniority -> Bool
esSenior Senior = True
esSenior _      = False


trabajanEn :: [Rol] -> [Proyecto] -> [Rol]
trabajanEn [] ps     = [] 
trabajanEn (r:rs) ps = if rolTrabajaEn r ps
                        then r : trabajanEn rs ps
                        else trabajanEn rs ps 

rolTrabajaEn :: Rol -> [Proyecto] -> Bool
rolTrabajaEn r []     = False
rolTrabajaEn r (p:ps) = perteneceA r p || rolTrabajaEn r ps                            

perteneceA :: Rol -> Proyecto -> Bool
perteneceA r p = proyectosIguales (proyecto r) p

--c
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps (ConsEmpresa rs) = longitud(trabajanEn rs ps)

--d

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
    --asignadosPorProyecto empresa 
asignadosPorProyecto (ConsEmpresa rs) = contarPersonasPorProyecto rs

contarPersonasPorProyecto :: [Rol] -> [(Proyecto, Int)]
contarPersonasPorProyecto rs = contarPorProyecto rs (proyectosDeRoles rs) 

contarPorProyecto :: [Rol] -> [Proyecto] -> [(Proyecto, Int)]
contarPorProyecto _  []     = []
contarPorProyecto rs (p:ps) = (p, contarPersonasEnProyecto rs p ) : contarPorProyecto rs ps

contarPersonasEnProyecto ::  [Rol] -> Proyecto -> Int
contarPersonasEnProyecto []     _ = 0
contarPersonasEnProyecto (r:rs) p = unoSi (proyectosIguales (proyecto r) p) + contarPersonasEnProyecto rs p

