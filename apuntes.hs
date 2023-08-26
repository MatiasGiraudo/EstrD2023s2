-- Constuccion de if-else
-- Siempre debe haber un else

max :: Int -> Int -> Int
max n m = if (n>m)
            then n
            else m

-- Para varias alternativas podemos anidar
signo :: Int -> Int 
signo n = if (n==0)
            then 0
            else if (n>0)
                    then 1
                    else -1    

-- Errores 
divisionEntera :: Int -> Int -> Int
-- Precond: y es distinto de 0
divisionEntera m n = if (n==0)
                        then error "No se puede dividir por cero"
                        else div m n

-- Definir tipo algebraicos  (variant en Intro)
-- Tipo Enum
data Dir = Norte | Este | Sur | Oeste
    deriving Show -- Tipos enumerados con show para mostrarlos



-- Pattern Machine (SOL OSIRVEN PARA CONSTRUCTORES DE TIPO ALGEBRAICO)
-- Constructores de tipo algebraico
-- Son las palabras en mayuscula que estan en la definicion del data
siguienteDir :: Dir -> Dir 
siguienteDir Norte = Este  --Cuando recibo Norte devuelve Este
siguienteDir Este = Sur
siguienteDir Sur = Oeste
siguienteDir Oeste = Norte

--Similar pero con alternativa indexada
siguienteDir' :: Dir -> Dir 
siguienteDir' d = 
    case d of
        Norte -> Este
        Este -> Sur
        Sur -> Oeste
        Oeste -> Norte


--Tipo Registro
--   nombre clase   nombre constructor  campos 
data Persona       =    P              String Int String
                                    -- Nombre Edad DNI
    deriving Show

nombre :: Persona -> String
nombre (P n e d) = n

edad :: Persona -> Int
edad (P n e d) = e

matias = P "Matias" 28 "123456578"
pepito = P "Pepito" 8 "123456578"



--Orden de las ecuaciones 
esEste :: Dir -> Bool
esEste Este = True
esEste d     = False --Va evaluando en orden

--Es equivalente
--Orden de las ecuaciones 
esEste'' :: Dir -> Bool
esEste'' Este = True
esEste'' _     = False

--Es distinto 
esEsteMal :: Dir -> Bool
esEsteMal d     = False 
esEsteMal Este = True --Nunca llega a esta evaluacion porque d toma cualquier cosa

--Es equivalente a 
esEste' :: Dir -> Bool
esEste' Este = True
esEste' Sur = False
esEste' Oeste = False
esEste' Norte = False

-- POSIBLE USO DE ORDEN A NUESTRO FAVOR
disyuncionPM :: Bool -> Bool -> Bool
disyuncionPM True True = True
disyuncionPM True False = True
disyuncionPM False True = True
disyuncionPM False False = False

disyuncionPM' :: Bool -> Bool -> Bool
disyuncionPM' False False = False
disyuncionPM' _       _   = True

--PM vs Funcion observadora
esMayorDeEdad' :: Persona -> Bool
esMayorDeEdad' (P _ e _) = e >= 18

--con funcion observadora
esMayorDeEdad :: Persona -> Bool
esMayorDeEdad p = edad p >= 18 -- aca uso la funcion edad que definimos mas arriba

--TUPLAS
fst :: (Int, Bool) -> Int
fst (n,b) = n --Esto siempre debe devolver un Int

snd :: (Int. Bool) -> Bool
snd (n,b) = b -- Al igual que esto
 

--Tipos polimorficos 
--POLIMORFISMO PARAMETRICO
fst :: (a, b) -> a 
fst(x , y) = x --Esto es polimorfico porque usa "VARIABLES DE TIPOS"

--Doble PM
implica :: Bool -> Bool -> Bool
implica True True = True
implica True False = False
implica False True = True
implica False False = True

--Equivalente
implica' :: Bool -> Bool -> Bool
implica' True b = b
implica' _    _ = True

