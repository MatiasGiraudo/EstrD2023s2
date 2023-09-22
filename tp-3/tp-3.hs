-- # 1 TIPOS RECURSIVOS SIMPLES
--1.1
data Color = Azul | Rojo
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

celda1 = Bolita Rojo 
            (Bolita Azul 
                (Bolita Rojo 
                    (Bolita Azul CeldaVacia)
                )
            )

celda2 = Bolita Azul 
            (Bolita Azul 
                (Bolita Azul 
                    (Bolita Azul CeldaVacia)
                )
            )          
--a
nroBolitas :: Color -> Celda -> Int 
nroBolitas c CeldaVacia       = 0
nroBolitas c (Bolita col cel) = unoSi (esMismoColor c col) + nroBolitas c cel                

esMismoColor :: Color -> Color -> Bool
esMismoColor Azul Azul = True
esMismoColor Rojo Rojo = True
esMismoColor _    _    = False

unoSi :: Bool -> Int
unoSi True   = 1
unoSi False  = 0

--b
poner :: Color -> Celda -> Celda
poner col cel = Bolita col cel

--c
sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia     = CeldaVacia
sacar c (Bolita col cel) = if esMismoColor c col 
                            then cel 
                            else Bolita col (sacar c cel)

--d
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _   cel = cel
ponerN n col cel = poner col (ponerN (n-1) col cel)

--1.2
data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

caminoBase = Fin
caminoConTesoroAlInicio =  Cofre [Tesoro,Cacharro] ( Nada Fin ) 
caminoConTesoro = Nada(
                      Nada (
                         Nada (
                             Cofre [Tesoro,Cacharro] (
                                Nada Fin
                                ) 
                             )
                         )
                     )

caminoConVariosTesoros = Nada(
                             Nada (
                                Cofre [Tesoro,Cacharro, Tesoro] (
                                    Nada (
                                        Cofre [Tesoro] (
                                           Nada Fin
                                           ) 
                                        )
                                    )       
                                ) 
                            )                     

caminoSinTesoro = Nada (
                    Nada (
                        Cofre [Cacharro] Fin 
                        )
                    )           

--a
hayTesoro :: Camino -> Bool
hayTesoro Fin            = False
hayTesoro (Nada c)       = hayTesoro c
hayTesoro (Cofre objs c) = tieneTesoro objs || hayTesoro c

tieneTesoro :: [Objeto] -> Bool
tieneTesoro []     = False
tieneTesoro (x:xs) = esTesoro x  || tieneTesoro xs     

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False


--b
pasosHastaTesoro :: Camino -> Int
--PRECOND: Tiene que haber al menos un tesoro
pasosHastaTesoro Fin            = error "Llegaste al fin, debiste haber encontrado un tesoro en el camino"
pasosHastaTesoro (Nada c)       = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre objs c) = if (tieneTesoro objs) 
                                    then 0 
                                    else 1 + pasosHastaTesoro c    

--c
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 _              = False
hayTesoroEn _ Fin            = False
hayTesoroEn n (Nada c)       = hayTesoroEn (n-1) c
hayTesoroEn n (Cofre objs c) = tieneTesoro objs || hayTesoroEn (n-1) c   


--d
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n Fin            = n <= 0
alMenosNTesoros n (Nada c)       = alMenosNTesoros n c
alMenosNTesoros n (Cofre objs c) = alMenosNTesoros (n-(tesorosEncontrados objs)) c

tesorosEncontrados :: [Objeto] -> Int
tesorosEncontrados []     = 0
tesorosEncontrados (x:xs) = unoSi (esTesoro x) + tesorosEncontrados xs    

-- # 2 TIPOS ARBOREOS
--2.1
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

arbol0 :: Tree Int 
arbol0 = NodeT 12 EmptyT EmptyT 

arbol1 :: Tree Int 
arbol1 = NodeT 12 
            (NodeT 10  EmptyT (NodeT 20 EmptyT EmptyT) ) 
            (NodeT 13 EmptyT EmptyT)
        
arbol2 :: Tree String
arbol2 = NodeT "Escudo" 
            (NodeT "Escudo"  EmptyT (NodeT "VaraMagica" EmptyT EmptyT) ) 
            (NodeT "Maza" EmptyT EmptyT)        
--1
sumarT :: Tree Int -> Int
sumarT EmptyT          = 0
sumarT (NodeT n t1 t2) = n + (sumarT t1) + (sumarT t2)     

--2
sizeT :: Tree a -> Int
sizeT EmptyT          =  0
sizeT (NodeT _ t1 t2) = 1 + sizeT t1 + sizeT t2

--3
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT n t1 t2) = (NodeT (n*2) (mapDobleT t1) (mapDobleT t2))  

--4
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT         = False
perteneceT e (NodeT y t1 t2) = (elementosIguales e y) || (perteneceT e t1) || (perteneceT e t2)

elementosIguales :: Eq a => a -> a -> Bool
elementosIguales x y = x == y

--5
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT          = 0
aparicionesT e (NodeT x t1 t2) = unoSi (elementosIguales e x) + (aparicionesT e t1) + (aparicionesT e t2)

--6
leaves :: Tree a -> [a]
leaves EmptyT          = []
leaves (NodeT x t1 t2) = if (esVacia t1 && esVacia t2) 
                          then [x] 
                          else leaves t1 ++ leaves t2 

esVacia :: Tree a -> Bool
esVacia EmptyT = True
esVacia _      = False

--7
heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT x t1 t2) = 1 + max (heightT t1) (heightT t2) 

--8 
mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x t1 t2) = NodeT x (mirrorT t2) (mirrorT t1)

--9
toList :: Tree a -> [a]
toList EmptyT          = []
toList (NodeT x t1 t2) = toList t1 ++ [x] ++ toList t2 

--10
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT          = []
levelN 0 (NodeT x t1 t2) = [x]
levelN n (NodeT x t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2                            

--11
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [x] : nivelar (listPerLevel t1) (listPerLevel t2)

nivelar :: [[a]] -> [[a]] -> [[a]]
nivelar []       yss      = yss
nivelar xss      []       = xss
nivelar (xs:xss) (ys:yss) = (xs ++ ys) : nivelar xss yss

--12
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x t1 t2) = x : elegirLaMasLarga (ramaMasLarga t1) (ramaMasLarga t2)

elegirLaMasLarga :: [a] -> [a] -> [a]
elegirLaMasLarga xs ys = if length xs > length ys
                            then xs
                            else ys

--13
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x t1 t2) = [x] : agregarACada x (todosLosCaminos t1)
                                     ++ agregarACada x (todosLosCaminos t2)

agregarACada :: a -> [[a]] -> [[a]]
agregarACada _ []       = []
agregarACada x (ys:yss) = (x : ys) : agregarACada x yss      

-- # 2 EXPRESIONES ARITMETICAS 
data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA
    deriving Show

numero1 = Valor 5
numero2 = Valor 2

operacionSuma1 = Sum numero1 numero2
operacionSuma2 = Sum operacionSuma1 numero1

operacionProd = Prod numero1 numero2

operacionNeg = Neg numero2

--1
eval :: ExpA -> Int
eval (Valor n)    = n
eval (Sum n m)  = (eval n) + (eval m)
eval (Prod n m) = (eval n) * (eval m)
eval (Neg n)    = -(eval n)

--2
-- a) 0 + x = x + 0 = x
-- b) 0 * x = x * 0 = 0
-- c) 1 * x = x * 1 = x
-- d) - (- x) = x


-- simplificar :: ExpA -> ExpA
-- simplificar (Sum 0 n)     = simplificar n
-- simplificar (Sum n 0)     = simplificar n
-- simplificar (Prod 0 _)    = Valor 0
-- simplificar (Prod _ 0)    = Valor 0
-- simplificar (Prod 1 n)    = simplificar n
-- simplificar (Prod n 1)    = simplificar n
-- simplificar (Neg (Neg n)) = simplificar n
-- simplificar n             = n






