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
agregar :: [a] -> [a] -> [a]
    --agregar [1,2] [2,3,4]
agregar [] [] = 
agregar (x:xs) (y:ys) = x y .. agregar xs ys    

