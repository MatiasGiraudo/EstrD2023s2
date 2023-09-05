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
-- sacar :: Color -> Celda -> Celda 

--d
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _   cel = cel
ponerN n col cel = poner col (ponerN (n-1) col cel)

--1.2
data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

caminoBase = Fin
caminoConTesoro = Nada(
                      Nada (
                         Nada (
                             Cofre [Tesoro,Cacharro] (
                                Nada Fin
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
hayTesoro (Cofre objs c) = tieneTesoro objs || hayTesoro c
hayTesoro (Nada c)       = hayTesoro c

tieneTesoro :: [Objeto] -> Bool
tieneTesoro []     = False
tieneTesoro (x:xs) = esTesoro x  || tieneTesoro xs     

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

--b
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin            = 0
pasosHastaTesoro (Nada c)       = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre objs c) = if (tieneTesoro objs) then pasosHastaTesoro c else 1 + pasosHastaTesoro c     
--CORREGIR

--c
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 _              = False
hayTesoroEn _ Fin            = False
hayTesoroEn n (Nada c)       = hayTesoroEn (n-1) c
hayTesoroEn n (Cofre objs c) = tieneTesoro objs || hayTesoroEn (n-1) c   
--Consultar si hay que evaluar el caso donde hay cero pasos-----

--d
-- alMenosNTesoros :: Int -> Camino -> Bool
-- alMenosNTesoros 0 c              =
-- alMenosNTesoros n Fin            = 
-- alMenosNTesoros n (Nada c)       = 
-- alMenosNTesoros n (Cofre objs c) = ... n ... objs .... alMenosNTesoros (n-1) c   

--e
-- cantTesorosEntre :: Int -> Int -> Camino -> Int

-- # 2 TIPOS ARBOREOS
--2.1
data Tree a = EmptyT | NodeT Int (Tree a) (Tree a)
    deriving Show


data TreeGen a = EmptyGen | NodeGen a (TreeGen a) (TreeGen a)
    deriving Show


arbol0 = NodeT 12 EmptyT EmptyT 

arbol1 = NodeT 12 
            (NodeT 10  EmptyT (NodeT 20 EmptyT EmptyT) ) 
            (NodeT 13 EmptyT EmptyT)
        
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
-- perteneceT :: Eq a => a -> TreeGen a -> Bool
-- perteneceT _ EmptyGen          = False
-- perteneceT x (NodeGen y t1 t2) = (elementosIguales x y) || (perteneceT t1) || (perteneceT t2)

elementosIguales :: Eq a => a -> a -> Bool
elementosIguales x y = if(x == y) then True else False

--5
aparicionesT :: Eq a => a -> TreeGen a -> Int
aparicionesT _ EmptyGen          = 0
aparicionesT e (NodeGen x t1 t2) = unoSi (elementosIguales e x) + (aparicionesT t1) + (aparicionesT t2)

--6
leaves :: TreeGen a -> [a]
leaves EmptyGen          = []
leaves (NodeGen x t1 t2) = x : leaves t1 ++ leaves t2  


