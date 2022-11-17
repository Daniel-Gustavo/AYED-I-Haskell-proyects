-------------------------------------------------------------------------------
-- Daniel Gustavo Martín, DNI 44439038, Primer proyecto de Laboratorio.
-------------------------------------------------------------------------------
-- Ejercicio 1-a)
esCero:: Int -> Bool
esCero x | x==0 = True
         | otherwise = False

-- Ejercicio 1-b)
esPositivo:: Int -> Bool
esPositivo x | x>0 = True 
             |otherwise = False 

-- Ejercicio 1-c)
esVocal :: Char -> Bool
esVocal x | x == 'a' = True
          | x == 'e' = True
          | x == 'i' = True
          | x == 'o' = True
          | x == 'u' = True
          |otherwise = False

-- Ejercicio 2-a)
paratodo :: [Bool]-> Bool 
paratodo [] = True 
paratodo (x:xs)
            | x==True = paratodo xs
            |otherwise = False 

--Ejercicio 2-b)
sumatoria :: [Int]-> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs 
                
--Ejercicio 2-c)
productoria :: [Int]-> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

--Ejercicio 2-d)
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

-- Ejercicio 2-e)
promedio :: [Int] -> Int
promedio [] = 0
promedio xs = div (sumatoria xs) (length xs)

-- Ejercicio 3)
pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece y(x:xs) | y==x = True
                  | otherwise = pertenece y xs

-- Ejercicio 4-a)
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] f = True
paratodo' (x:xs)f| f x == True = paratodo' xs f
                 | otherwise = False

-- Ejercicio 4-b)
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] f = False
existe' (x:xs) f | f x == True = True
                 |otherwise = existe' xs f 

--Ejercicio 4-c)
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] f = 0
sumatoria' (x:xs) f = f x + sumatoria' xs f 

-- Ejercicio 4-d)
productoria' :: [a]-> (a -> Int) -> Int
productoria'[] f = 0
productoria'(x:xs) f = f x * productoria' xs f 

-- Ejercicio 5)
paratodo'' :: [Bool] ->Bool
paratodo'' xs = paratodo' xs (== True)


-- Ejercicio 6- a)
todosPares :: [Int] -> Bool
todosPares xs = paratodo' xs even

-- Ejercicio 6- b)
esMultiplo :: Int -> Int -> Bool
esMultiplo x y = mod y x == 0

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo x ys = existe' ys (esMultiplo x)

-- Ejercicio 6- c)
sumaCuadrados :: Int -> Int
sumaCuadrados x = sumatoria' [0..x] (^2)

-- Ejercicio 6- d)
factorial' :: Int -> Int
factorial' x = productoria' [1..x] (*1)

-- Ejercicio 6- e)
filtroPares :: Int -> Int
filtroPares x
        |mod x 2 == 0 = x
        |otherwise = 1

multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria' xs filtroPares

-- Ejercicio 7)

--a) La funcion map toma una funcion y se lo aplica a cada elemento de la lista.
--b) La funcion Filter toma una funcion(que devuelve un bool) y se la aplica a cada elemento de la lista y te devuelve una lista con los elementos que la cumplan.
--c) Equivale a [2, -3, 6, 3, -7]
-- [1, 6, 2]

--Ejercicio 8- a) 
duplica' :: [Int] -> [Int]
duplica' [] = []
duplica' (x:xs) = x*2 : duplica' xs 

--Ejercicio 8- b) 
--map :: (a -> b) -> [a] -> [b]
duplica'' :: [Int] -> (Int-> Int) -> [Int]
duplica'' xs b = map b xs

--Ejercicio 9- a)
listaPares :: [Int] -> [Int]
listaPares [] = []
listaPares (x:xs)
        | mod x 2 == 0 = x:listaPares xs  
        | otherwise = listaPares xs

-- Ejercicio 9- b)
listaPares' :: [Int]-> (Int -> Bool) -> [Int]
listaPares' [] _ = []
listaPares' xs b = filter b xs

-- Ejercicio 9- c)
multiplicaPares' :: [Int] -> (Int -> Bool) -> Int
multiplicaPares' xs b = productoria (filter b xs)

-- Ejercicio 10- a)
primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA _ [] = []
primIgualesA x (y:ys) 
        | x == y =  x : primIgualesA x ys 
        | otherwise = [] 
        
--Ejercicio 10- b)
primIgualesA' ::  Eq a => a -> [a] -> [a]
primIgualesA' x ys = takeWhile (== x) ys

--Ejercicio 11- a)
primIguales :: Eq a => [a] -> [a]
primIguales [] = []
primIguales [x]= [x]
primIguales (x:(y:ys))
        | x == y  =  x: primIguales (y:ys)
        | otherwise = [x]

-- Ejercicio 11- b)
primIguales'' :: Eq a => [a] -> [a]
primIguales'' [] = []
primIguales'' [x]= [x]
primIguales'' (y:ys) = primIgualesA' y (y:ys)