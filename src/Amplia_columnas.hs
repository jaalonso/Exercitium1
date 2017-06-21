import Data.Array

-- ----------------------------------------------------------------------
-- Las matrices enteras se pueden representar mediante tablas con
-- índices enteros: 
--    type Matriz = Array (Int,Int) Int
-- 
-- Definir la función
--    ampliaColumnas :: Matriz -> Matriz -> Matriz
-- tal que (ampliaColumnas p q) es la matriz construida añadiendo las
-- columnas de la matriz q a continuación de las de p (se supone que
-- tienen el mismo número de filas). Por ejemplo, si p y q representa
-- las dos primeras matrices, entonces (ampliaColumnas p q) es la
-- tercera  
--    |0 1|    |4 5 6|    |0 1 4 5 6| 
--    |2 3|    |7 8 9|    |2 3 7 8 9|
-- En Haskell,
--    ghci> :{
--    *Main| ampliaColumnas (listArray ((1,1),(2,2)) [0..3]) 
--    *Main|                (listArray ((1,1),(2,3)) [4..9])
--    *Main| :}
--    array ((1,1),(2,5)) 
--          [((1,1),0),((1,2),1),((1,3),4),((1,4),5),((1,5),6),
--           ((2,1),2),((2,2),3),((2,3),7),((2,4),8),((2,5),9)]
-- --------------------------------------------------------------------- 

type Matriz = Array (Int,Int) Int

ampliaColumnas :: Matriz -> Matriz -> Matriz
ampliaColumnas p1 p2 =
  array ((1,1),(m,n1+n2)) [((i,j), f i j) | i <- [1..m], j <- [1..n1+n2]]
    where ((_,_),(m,n1)) = bounds p1
          ((_,_),(_,n2)) = bounds p2
          f i j | j <= n1   = p1!(i,j)
                | otherwise = p2!(i,j-n1) 

-- ---------------------------------------------------------------------
-- § Soluciones de los alumnos                                        --
-- ---------------------------------------------------------------------

-- Ana (incorrecta)
ampliaColumnasA1 :: Matriz -> Matriz -> Matriz
ampliaColumnasA1 p q = 
    array ((1,1),(m,n1+n2)) [((i,j),f i j) | i<-[1..m], j <- [1..n1+n2]]
        where (_,(m,n1)) = bounds p
              (_,(_,n2)) = bounds q
              f i j | j <= n1   = p!(i,j)
                    | otherwise = q!(i,j-n1)

-- Luis (incorrecta)
ampliaColumnasA2 :: Matriz -> Matriz -> Matriz
ampliaColumnasA2 p1 p2 =
    array ((1,1),(m,n1+n2)) [((i,j),f i j) | i <- [1..m], j <- [1..n1+n2]]
        where (_,(m,n1)) = bounds p1
              (_,(_,n2)) = bounds p2
              f i j | j <= n1 = p1!(i,j)
                    | otherwise = p2!(i,j-n1)

-- Eduardo
ampliaColumnasA3 :: Matriz -> Matriz -> Matriz
ampliaColumnasA3 p1 p2 = 
    array ((1,1),(m,n1+n2))
          (assocs p1 ++ [((a,b+n1),c) | ((a,b),c) <- assocs p2])
    where (_,(m,n1)) = bounds p1
          (_,(_,n2)) = bounds p2

verifica :: (Matriz -> Matriz -> Matriz) -> Bool
verifica f =
   f (listArray ((1,1),(2,2)) [0..3]) 
     (listArray ((1,1),(2,3)) [4..9])
   ==
   array ((1,1),(2,5)) 
         [((1,1),0),((1,2),1),((1,3),4),((1,4),5),((1,5),6),
         ((2,1),2),((2,2),3),((2,3),7),((2,4),8),((2,5),9)]
