-- Ejercicio 1

-- a)

data Carrera = Matematica | Fisica | Computacion | Astronomia deriving (Eq,Show)

-- b)

titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Computacion"
titulo Astronomia = "Licenciatura en Astronomia"

-- c y 2a)

data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Eq, Ord, Bounded, Show)

-- d)

cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'


-- Ejercicio 3

-- a)

minimoElemento :: Ord a => [a] -> a
minimoElemento [] = undefined
minimoElemento [x] = x
minimoElemento (x:xs) = min x (minimoElemento xs)

-- b)

minimoElemento1 :: (Bounded a, Ord a) => [a] -> a
minimoElemento1 [] = minBound
minimoElemento1 [x] = x
minimoElemento1 (x:xs) = min x (minimoElemento1 xs)

-- c)

notaGrave :: [NotaBasica] -> NotaBasica
notaGrave [x] = x
notaGrave xs = minimoElemento xs

-- Ejercicio 4

-- a)


type Ingreso = Int

data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving Show
data Area = Administrativa | Ensenanza | Economica | Postgrado deriving Show

data Persona = Decano
    |Docente Cargo
    |NoDocente Area
    |Estudiante Carrera Ingreso deriving Show

-- b)


-- c)

cuantos_doc :: [Persona] -> Cargo -> Int
cuantos_doc [] c = 0
cuantos_doc ((Docente x):xs) c
    | mismocargo x c = 1 + cuantos_doc xs c
    | otherwise = cuantos_doc xs c
cuantos_doc (_:xs) c = cuantos_doc xs c

mismocargo:: Cargo -> Cargo -> Bool
mismocargo Titular Titular = True
mismocargo Asociado Asociado = True
mismocargo Adjunto Adjunto = True
mismocargo Asistente Asistente = True
mismocargo Auxiliar Auxiliar = True
mismocargo _ _ = False

-- d)

cuantos_doc' :: [Persona] -> Cargo -> Int
cuantos_doc' [] c = 0
cuantos_doc' xs c = length (filter (filterCargo c) xs)

filterCargo :: Cargo -> Persona -> Bool
filterCargo c (Docente a)
    | mismocargo c a = True
    | otherwise = False
filterCargo c _ = False


-- Ejercicio 5

-- 5 a)

data Alteracion = Bemol | Sostenido | Natural deriving (Eq,Show,Ord)
data NotaMusical = Nota NotaBasica Alteracion deriving (Eq,Show,Ord)

sonido :: NotaBasica -> Int
sonido Do = 1
sonido Re = 3
sonido Mi = 5
sonido Fa = 6
sonido Sol = 8
sonido La = 10
sonido Si = 12

-- 5 b)

sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota n Sostenido) = (sonido n) + 1
sonidoCromatico (Nota n Bemol) = (sonido n) - 1
sonidoCromatico (Nota n Natural) = sonido n

--5 c)

comparaSonido :: NotaMusical -> NotaMusical -> Bool
comparaSonido (Nota n a) (Nota m b)
    | sonidoCromatico (Nota n a) == sonidoCromatico (Nota m b) = True
    | otherwise = False

--5 d)

comparaSonido' :: NotaMusical -> NotaMusical -> Bool
comparaSonido' (Nota n a) (Nota m b)
    | sonidoCromatico (Nota n a) <= sonidoCromatico (Nota m b) = True
    | otherwise = False

-- Ejercicio 6

--6 a)

primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento (x:xs) = Just x

-- Ejercicio 7

data Cola = VaciaC | Encolada Persona Cola deriving Show

-- 7 a)

--1

atender:: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada x xs ) = Just xs

--2

encolar :: Persona -> Cola -> Cola
encolar p VaciaC = Encolada p VaciaC
encolar p (Encolada x xs) = (Encolada x (Encolada p xs))

encolarr :: Persona -> Cola -> Cola
encolarr = Encolada





--8
data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b) deriving (Show, Eq) 
type Diccionario = ListaAsoc String String
type Padron = ListaAsoc Int String
--a)
type ListaTelefonica = ListaAsoc Int String
--b)
--1
la_long :: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo a b xs) = 1 + la_long xs 
--2
la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Vacia Vacia = Vacia
la_concat Vacia (Nodo y s xs) = Nodo y s xs
la_concat (Nodo a b zs) Vacia = Nodo a b zs
la_concat (Nodo a b zs) xs = Nodo a b (la_concat xs zs) 
-- lo probamos con esto la_concat (Nodo "alexis" "Ortiz" Vacia) (Nodo "Estudiante" "Famaf" Vacia)
--3
la_agregar :: ListaAsoc a b -> a -> b -> ListaAsoc a b
la_agregar Vacia a b = (Nodo a b Vacia)
la_agregar nod a b = (Nodo a b nod)
--4
la_pares :: ListaAsoc a b -> [(a,b)]
la_pares Vacia = []
la_pares (Nodo a b ys) = (a,b) : la_pares ys
--5
la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia _ = Nothing
la_busca (Nodo a b xs) c
        | a == c = Just b
        | otherwise = la_busca xs c 
--6
la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b 
la_borrar _ Vacia = Vacia
la_borrar c (Nodo a b xs)
        | a == c = xs
        | otherwise = Nodo a b (la_borrar c xs)