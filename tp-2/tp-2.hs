-- # 1 RECURSION SOBRE LISTAS
--1
sumatoria :: [Int] -> Int
    -- sumatoria [1,2,3,4,5]
sumatoria []     = 0
sumatoria (n:ns) = n + sumatoria ns

--2
longitud :: [a] -> Int
longitud []     = 0
longitud (_:xs) = 1 + longitud xs    

--3
sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (n:ns) = n+1 : sucesores ns    

--4
-- conjuncion :: [Bool] -> Bool
-- conjuncion []     = 
-- conjuncion (x:xs) = x .. conjuncion xs    

-- --5
-- disyuncion :: [Bool] -> Bool
-- disyuncion [] = 
-- disyuncion (x:xs) = x .. disyuncion xs

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
-- agregarAlFinal :: [a] -> a -> [a]
--     --agregarAlFinal [1,2,3,4,5] 10
-- agregarAlFinal [] e = e:[]
-- agregarAlFinal (x:xs) e = e : agregarAlFinal xs e

--12
-- agregar :: [a] -> [a] -> [a]
--     --agregar [1,2] [2,3,4]
-- agregar [] [] = 
-- agregar (x:xs) (y:ys) = x y .. agregar xs ys

-- --13
-- reversa :: [a] -> [a]

-- --14
-- zipMaximos :: [Int] -> [Int] -> [Int]

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
    --v 3*2*1
factorial 0 = 1
factorial n = if(n < 0)
                then error "El factorial no esta definido para numeros negativos"
                else n * factorial (n-1)

--2
cuentaRegresiva :: Int -> [Int]
    --cuentaRegresiva 5 [5,4,3,2,1,0]
cuentaRegresiva 0 = 0:[]
cuentaRegresiva n = n : cuentaRegresiva (n-1)

--3
repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n e = e : repetir (n-1) e

--4
-- losPrimeros :: Int -> [a] -> [a]
--     --losPrimeros 2 [4,6,7,2,3] --> [4,6]
-- losPrimeros 0 []     =
-- losPrimeros n (x:xs) = n x ... losPrimeros (n-1) xs    

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
-- promedioEdad :: [Persona] -> Int
-- promedioEdad [] = error "La lista debe tener al menos una Persona"
-- promedioEdad (x:xs) = x ... promedioEdad xs                    

--c
elMasViejo :: [Persona] -> Persona
--PREC: La lista no esta vacía
elMasViejo [x]    = x
elMasViejo (x:xs) = if(edad x > edad (elMasViejo xs))
                        then x 
                        else elMasViejo xs