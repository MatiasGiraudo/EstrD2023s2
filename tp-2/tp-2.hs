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
pertenece e (x:xs) = if(e == x)
                        then True
                        else  pertenece e xs

--8
apariciones :: Eq a => a -> [a] -> Int     
    --apariciones 'A' ['A', 'J', 'B', 'A']              
apariciones _ []     = 0
apariciones e (x:xs) = if(e == x)
                        then 1 + apariciones e xs
                        else apariciones e xs

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
zipMaximos []  ms = ms
zipMaximos  ns [] = ns
zipMaximos (n:ns) (m:ms) = if(n > m) 
                            then n : zipMaximos ns ms    
                            else m : zipMaximos ns ms  

--15
elMinimo :: Ord a => [a] -> a
--PREC: La lista no esta vacía
elMinimo []     = error "La lista no puede estar vacía"
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
repetir 0 _ = []
repetir n e = e : repetir (n-1) e

--4
losPrimeros :: Int -> [a] -> [a]
    --losPrimeros 3 [4,6,7,2,3] --> [4,6,7]
losPrimeros 0 _      = []
losPrimeros _ []     = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs    
                    
--5
-- sinLosPrimeros :: Int -> [a] -> [a]

-- # 3 REGISTROS
--1
data Persona = P String Int
               --Nombre Edad
    deriving Show

marcos = P "Marcos" 20
matilda = P "Matilda" 30
pedro = P "Pedro" 58
nahuel = P "Nahuel" 45

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
elMasViejo [x]    = x
elMasViejo (x:xs) = if(edad x > edad (elMasViejo xs))
                        then x 
                        else elMasViejo xs

--2                        

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
-- asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
-- asignadosPorProyecto e = asignadosA (roles e) (proyectos e)

-- asignadosA :: [Rol] -> [Proyecto] -> [(Proyecto, Int)]4e