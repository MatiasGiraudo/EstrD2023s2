-- #1 PIZZAS

data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
data Ingrediente = Salsa  | Queso  | Jamon | Aceitunas Int deriving Show

-- pizza :: Pizza
-- pizza = Capa (Aceitunas 8)(Capa Jamon(Capa Queso(Capa Salsa (Prepizza))))
-- pizza2 = Capa (Aceitunas 8)(Capa Jamon(Capa Queso(Capa Salsa (Prepizza))))
-- pizza3 = Capa Jamon(Capa Queso(Capa Salsa (Prepizza)))
-- pizza4 = Capa Queso(Capa Salsa (Prepizza))

-- listaPizzas = [pizza, pizza2,pizza3,pizza4]

--a
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza     = 0
cantidadDeCapas (Capa ing p) = 1 +  cantidadDeCapas p   

--b
armarPizza :: [Ingrediente] -> Pizza
armarPizza []     = Prepizza
armarPizza (i:is) = Capa i (armarPizza is)

--c
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza     = Prepizza
sacarJamon (Capa ing p) = 
    if esJamon ing 
        then (sacarJamon p)
        else Capa ing (sacarJamon p)

esJamon :: Ingrediente -> Bool 
esJamon Jamon = True
esJamon _     = False

--d
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True 
tieneSoloSalsaYQueso (Capa ing p) = esSalsaOQueso ing && tieneSoloSalsaYQueso p 

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso _     = False

--e
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza     = Prepizza
duplicarAceitunas (Capa ing p) =  
    if esAceituna ing 
        then Capa (duplicarAceitunasDe ing) (duplicarAceitunas p)
        else Capa ing (duplicarAceitunas p)

duplicarAceitunasDe :: Ingrediente -> Ingrediente 
--PRECOND: EL infgrediente dado es aceituna
duplicarAceitunasDe (Aceitunas n) = Aceitunas (2*n)

esAceituna :: Ingrediente -> Bool 
esAceituna (Aceitunas _) = True
esAceituna _             = False


--f
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza []     = []
cantCapasPorPizza (p:ps) =  (cantidadDeCapas p, p) : (cantCapasPorPizza ps)

-- # 2 MAPA DE TESOROS

data Dir = Izq | Der deriving Show
data Objeto = Tesoro | Chatarra deriving Show
data Cofre = Cofre [Objeto] deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa  deriving Show

--2.1
hayTesoro :: Mapa -> Bool
hayTesoro (Fin cf)               = hayTesoroEnCofre cf
hayTesoro (Bifurcacion cf mi md) = hayTesoroEnCofre cf || hayTesoro mi || hayTesoro md

hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre (Cofre os) = hayTesoroEnObjetos os

hayTesoroEnObjetos :: [Objeto] -> Bool
hayTesoroEnObjetos []     = False
hayTesoroEnObjetos (o:os) = esTesoro o || hayTesoroEnObjetos os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

--2.2
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] m                          = hayTesoroAca m 
hayTesoroEn (d:ds) (Fin cf)               = False
hayTesoroEn (d:ds) (Bifurcacion cf mi md) =
    if esIzq d 
        then hayTesoroEn ds mi
        else hayTesoroEn ds md

esIzq :: Dir -> Bool 
esIzq Izq = True
esIzq _   = False

hayTesoroAca :: Mapa -> Bool
hayTesoroAca (Fin cf)             = hayTesoroEnCofre cf
hayTesoroAca (Bifurcacion cf _ _) = hayTesoroEnCofre cf

--2.3
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin cf)               = []
caminoAlTesoro (Bifurcacion cf mi md) = 
    if hayTesoroEnCofre cf
        then []
        else if hayTesoro mi 
            then Izq : caminoAlTesoro mi 
            else Der : caminoAlTesoro md

--2.4
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin _)               = []
caminoDeLaRamaMasLarga (Bifurcacion _ mi md) =
    if length (caminoDeLaRamaMasLarga mi ) > length (caminoDeLaRamaMasLarga md) 
        then Izq : (caminoDeLaRamaMasLarga mi)
        else Der : (caminoDeLaRamaMasLarga md)
    
--2.5
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin cf)               = tesorosEnCofre cf : []
tesorosPorNivel (Bifurcacion cf mi md) = 
    tesorosEnCofre cf : juntarNiveles (tesorosPorNivel mi) (tesorosPorNivel md)

tesorosEnCofre  :: Cofre -> [Objeto]
tesorosEnCofre (Cofre os) = tesorosEnObjetos os

tesorosEnObjetos :: [Objeto] -> [Objeto]
tesorosEnObjetos []     = []
tesorosEnObjetos (o:os) = if esTesoro o 
                            then o : tesorosEnObjetos os
                            else tesorosEnObjetos os


juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles [] yss            = yss 
juntarNiveles xss []            = xss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : (juntarNiveles xss yss)


--2.6
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _)               = []
todosLosCaminos (Bifurcacion _ mi md) = 
    consACada Izq (todosLosCaminos mi) ++ consACada Der (todosLosCaminos md)

consACada :: a -> [[a]] -> [[a]] 
consACada x []       = [[x]]
consACada x (xs:xss) = (x : xs) : (consACada x xss)

-- # 3 NAVE ESPACIAL
data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show
type SectorId = String 
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
data Nave = N (Tree Sector) deriving Show
data Sector = S SectorId [Componente] [Tripulante] deriving Show

--3.1
sectores :: Nave -> [SectorId]
sectores (N t) = sectoresT t 

sectoresT :: Tree Sector -> [SectorId]
sectoresT EmptyT          = []
sectoresT (NodeT s ti td) = sectorId s : (sectoresT ti) ++ (sectoresT td)

sectorId :: Sector -> SectorId
sectorId (S id _ _ ) = id

--3.2
poderDePropulsion :: Nave -> Int
poderDePropulsion (N t) = poderDePropulsionT t 

poderDePropulsionT :: Tree Sector -> Int 
poderDePropulsionT EmptyT          = 0
poderDePropulsionT (NodeT s ti td) = 
    poderDePropulsionSector s + poderDePropulsionT ti + poderDePropulsionT td

poderDePropulsionSector :: Sector -> Int
poderDePropulsionSector (S _ cs _) = poderDePropulsionCs cs 

poderDePropulsionCs :: [Componente] -> Int 
poderDePropulsionCs []     = 0
poderDePropulsionCs (c:cs) = poderPropulsionC c + poderDePropulsionCs cs 

poderPropulsionC :: Componente -> Int 
poderPropulsionC (Motor n) = n 
poderPropulsionC _         = 0

--3.3
barriles :: Nave -> [Barril]
barriles (N t) = barrilesT t 

barrilesT :: Tree Sector -> [Barril]
barrilesT EmptyT          = []
barrilesT (NodeT s ti td) = barrilesS s ++ (barrilesT ti) ++ (barrilesT td)

barrilesS :: Sector -> [Barril] 
barrilesS (S _ cs _) = barrilesCs cs 

barrilesCs :: [Componente] -> [Barril]
barrilesCs []     = []
barrilesCs (c:cs) = barrilesC c ++ (barrilesCs cs)

barrilesC :: Componente -> [Barril]
barrilesC (Almacen bs) = bs
barrilesC _            = []



--3.4
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs id (N t) = N (agregarASectorT cs id t)

agregarASectorT :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASectorT cs id EmptyT          = EmptyT
agregarASectorT cs id (NodeT s ti td) = 
    if sectorId s == id 
        then NodeT (agregarComponentes s cs) ti td 
        else NodeT s (agregarASectorT cs id ti) (agregarASectorT cs id td)


agregarComponentes :: Sector -> [Componente] -> Sector 
agregarComponentes (S id cs ts) csn = S id (cs ++ csn) ts

-- # 4 MANADA DE LOBOS

type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
data Manada = M Lobo

--4.5
-- exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
-- exploradoresPorTerritorio (M l) = exploradoresPorTerritorioL l 

-- exploradoresPorTerritorioL :: Lobo -> [(Territorio, [Nombre])]
-- exploradoresPorTerritorioL (Cazador _ _ l1 l2 l3) = 
--     juntarTerritorio (exploradoresPorTerritorioL l1) 
--     (juntarTerritorio (exploradoresPorTerritorioL l2) (exploradoresPorTerritorioL l3)) 
-- exploradoresPorTerritorioL (Explorador n ts l1 l2) = 
--     agregarExplorador n ts (juntarTerritorio (exploradoresPorTerritorioL l1)(exploradoresPorTerritorioL l2)) 
-- exploradoresPorTerritorioL (Cria n) = []

-- agregarExplorador :: Nombre -> [Territorio] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
-- agregarExplorador n []     lss = lss
-- agregarExplorador n (t:ts) lss = 
--     agregarATerritorio n t (agregarExplorador n ts lss)

-- agregarATerritorio :: Nombre -> Territorio -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
-- agregarATerritorio n tn []             = (tn, [n]) : []
-- agregarATerritorio n tn ((t, ns): tns) = 
--     if tn == t 
--         then (tn,n:ns) : tns
--         else (t, ns) : (agregarATerritorio n tn tns)