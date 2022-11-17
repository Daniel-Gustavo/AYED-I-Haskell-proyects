type Talle = Int

data Ropa = Camisa | Pantalon | Pollera | Short deriving Show
data Prenda = ConTalle Talle Ropa
             | TalleUnico Ropa
             deriving Show

mismo_ropa :: Ropa -> Ropa -> Bool
mismo_ropa Camisa Camisa = True
mismo_ropa Pantalon Pantalon = True
mismo_ropa Pollera Pollera = True
mismo_ropa Short Short = True
mismo_ropa _ _ = False

valor_talle :: Prenda -> Int
valor_talle (ConTalle n f) = n
valor_talle (TalleUnico f) = 0

instance Eq Prenda where
    n1 == n2 = valor_talle n1 == valor_talle n2

instance Ord Prenda where
    n1 <= n2 = valor_talle n1 <= valor_talle n2



--ejercicio 2
solo_con_talle :: [Prenda] -> Ropa -> [Talle]
solo_con_talle [] _ = []
solo_con_talle ((ConTalle n p') : xs) p | mismo_ropa p p' = n : solo_con_talle xs p
                                        | otherwise = solo_con_talle xs p
solo_con_talle (_:xs) p = solo_con_talle xs p

--ejercicio 3
data ListaAsoc a b = Vacia | Nodo a b ( ListaAsoc a b ) deriving Show

la_duplica_pares :: (Integral a, Num b) => ListaAsoc a b -> ListaAsoc a b
la_duplica_pares Vacia = Vacia
la_duplica_pares (Nodo k v la) | even k = Nodo k (2*v) (la_duplica_pares la)
                               | otherwise = Nodo k v (la_duplica_pares la)


-- ejercicio 4
data Arbol a = Hoja | Rama (Arbol a) a (Arbol a) deriving Show

a_esCota_inf :: Ord a => a -> Arbol a -> Bool
a_esCota_inf e Hoja = True
a_esCota_inf e (Rama l v r) = (e <= v) && (a_esCota_inf e l) && (a_esCota_inf e r)