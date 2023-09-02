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
ponerN 0 col cel = cel
ponerN n col cel = poner col (ponerN (n-1) col cel)

--1.2
data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

--a
-- hayTesoro :: Camino -> Bool
-- hayTesoro Fin            = False
-- hayTesoro (Cofre objs c) = 
-- hayTesoro (Nada c)       =


