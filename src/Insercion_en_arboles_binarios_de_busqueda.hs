-- Insercion_en_arboles_binarios_de_busqueda.hs
-- Inserción en árboles binarios de búsqueda
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 12 de Julio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un árbol binario de búsqueda (ABB) es un árbol binario tal que el de
-- cada nodo es mayor que los valores de su subárbol izquierdo y es
-- menor que los valores de su subárbol derecho y, además, ambos
-- subárboles son árboles binarios de búsqueda. Por ejemplo, al
-- almacenar los valores de [8,4,2,6,3] en un ABB se puede obtener el
-- siguiente ABB:  
--    
--       5                
--      / \               
--     /   \              
--    2     6             
--         / \            
--        4   8           
-- 
-- Los ABB se pueden representar como tipo de dato algebraico:
--    data ABB = V
--             | N Int ABB ABB
--             deriving (Eq, Show)
-- Por ejemplo, la definición del ABB anteriore es
--    ej :: ABB
--    ej = N 3 (N 2 V V) (N 6 (N 4 V V) (N 8 V V))
--
-- Definir la función 
--    inserta :: Int -> ABB -> ABB
-- tal que (inserta v a) es el árbol obtenido añadiendo el valor v al
-- ABB a, si no es uno de sus valores. Por ejemplo, 
--    ghci>  inserta 5 ej
--    N 3 (N 2 V V) (N 6 (N 4 V (N 5 V V)) (N 8 V V))
--    ghci>  inserta 1 ej
--    N 3 (N 2 (N 1 V V) V) (N 6 (N 4 V V) (N 8 V V))
--    ghci>  inserta 2 ej
--    N 3 (N 2 V V) (N 6 (N 4 V V) (N 8 V V))
-- ---------------------------------------------------------------------

data ABB = V
         | N Int ABB ABB
         deriving (Eq, Show)

ej :: ABB 
ej = N 3 (N 2 V V) (N 6 (N 4 V V) (N 8 V V))

inserta :: Int -> ABB -> ABB
inserta v' V = N v' V V
inserta v' (N v i d) 
    | v' == v   = N v i d
    | v' < v    = N v (inserta v' i) d
    | otherwise = N v i (inserta v' d)

-- ---------------------------------------------------------------------
-- § Soluciones de alumnos                                            --
-- ---------------------------------------------------------------------

-- Eduardo
-- =======

insertaA1 n (N x i d) | pertenece n (N x i d) = (N x i d)
                      | otherwise             = aux n (N x i d)
    where
      aux n (N x V V) | n < x = (N x (N n V V) V)
                      | n > x = (N x V (N n V V))
      aux n (N x i d) | n > x = (N x i (aux n d))
                      | n < x = (N x (aux n i) d) 

pertenece n (N x V V) = n == x
pertenece n (N x i d) | n == x    = True
                      | otherwise = pertenece n i || pertenece n d

-- Eduardo
-- =======

insertaA2 n (N x i d) | perteneceA2 n (N x i d) = (N x i d)
                      | otherwise = aux n (N x i d)
    where
      aux n V = N n V V
      aux n (N x i d) | n > x = N x i (aux n d)
                      | n < x = N x (aux n i) d

perteneceA2 n (N x V V) = n == x
perteneceA2 n (N x i d) | n == x = True
                        | otherwise = perteneceA2 n i || perteneceA2 n d

-- David
-- =====

insertaA3 :: Int -> ABB -> ABB
insertaA3 v V = N v V V
insertaA3 v (N x i d)| v==x  = N x i d 
                     | v < x = N x (insertaA3 v i) d
                     | otherwise = N x i (insertaA3 v d)

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

verifica f =
    inserta 5 ej ==  N 3 (N 2 V V) (N 6 (N 4 V (N 5 V V)) (N 8 V V)) &&
    inserta 1 ej ==  N 3 (N 2 (N 1 V V) V) (N 6 (N 4 V V) (N 8 V V)) &&
    inserta 2 ej ==  N 3 (N 2 V V) (N 6 (N 4 V V) (N 8 V V))
    where inserta = f
