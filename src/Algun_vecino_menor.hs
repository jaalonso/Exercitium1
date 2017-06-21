import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 4. Las matrices puede representarse mediante tablas cuyos
-- índices son pares de números naturales:    
--    type Matriz = Array (Int,Int) Int
-- Definir la función 
--    algunMenor :: Matriz -> [Int]
-- tal que (algunMenor p) es la lista de los elementos de p que tienen
-- algún vecino menor que él. Por ejemplo,  
--    algunMenor (listArray ((1,1),(3,4)) [9,4,6,5,8,1,7,3,4,2,5,4])
--    [9,4,6,5,8,7,4,2,5,4]          
-- pues sólo el 1 y el 3 no tienen ningún vecino menor en la matriz
--    |9 4 6 5|
--    |8 1 7 3|
--    |4 2 5 4|
-- ---------------------------------------------------------------------        

type Matriz = Array (Int,Int) Int

algunMenor :: Matriz -> [Int]
algunMenor p = 
    [p!(i,j) | (i,j) <- indices p,
               or [p!(a,b) < p!(i,j) | (a,b) <- vecinos (i,j)]] 
    where (_,(m,n)) = bounds p
          vecinos (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
                                   b <- [max 1 (j-1)..min n (j+1)],
                                   (a,b) /= (i,j)]

-- ---------------------------------------------------------------------
-- § Soluciones de los alumnos                                        --
-- ---------------------------------------------------------------------

-- Luis
-- ====

-- type Matriz = Array (Int,Int) Int 
algunMenorA1 :: Matriz -> [Int]
algunMenorA1 p = 
    [x | ((i,j),x) <- assocs p, 
         x > minimum [y | ((a,b),y) <- assocs p, 
                          a `elem` [(max 1 (i-1))..(min m (i+1))], 
                          b `elem` [(max 1 (j-1))..(min n (j+1))]]]
    where (m,n) = snd (bounds p)

-- David
-- =====

-- import Data.Array
--  
-- type Matriz = Array (Int,Int) Int
 
algunMenorA2 :: Matriz -> [Int]
algunMenorA2 p = [p!(i,j) | i<- [1..m], j <- [1..n], tieneMenor i j]
    where (m,n)          = snd (bounds p)
          tieneMenor i j = or [p!(i,j) > p!(i',j') | i' <- [i-1..i+1],
                                                     j' <- [j-1..j+1],
                                                     pertenece i' j']
          pertenece i' j' = inRange ((1,1),(m,n)) (i',j')

