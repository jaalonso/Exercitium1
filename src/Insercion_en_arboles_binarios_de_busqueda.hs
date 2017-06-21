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
