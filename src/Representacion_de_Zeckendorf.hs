-- Representacion_de_Zeckendorf.hs
-- Representacion_de_Zeckendorf.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 7 de Julio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Introducción                                                     --
-- ---------------------------------------------------------------------

-- Los primeros números de Fibonacci son
--    1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, ...
-- tales que los dos primeros son iguales a 1 y los siguientes se
-- obtienen sumando los dos anteriores. 
-- 
-- El [teorema de Zeckendorf](http://bit.ly/VB2pU3) establece que todo
-- entero positivo n se puede representar, de manera única, como la suma
-- de números de Fibonacci no consecutivos decrecientes. Dicha suma se
-- llama la representación de Zeckendorf de n. Por ejemplo, la
-- representación de Zeckendorf de 100 es 
--    100 = 89 + 8 + 3
-- Hay otras formas de representar 100 como sumas de números de
-- Fibonacci; por ejemplo,
--    100 = 89 +  8 + 2 + 1
--    100 = 55 + 34 + 8 + 3
-- pero no son representaciones de Zeckendorf porque 1 y 2 son números
-- de Fibonacci consecutivos, al igual que 34 y 55.

-- ---------------------------------------------------------------------
-- § Ejercicio                                                        --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    zeckendorf :: Integer -> [Integer]
-- tal que (zeckendorf n) es la representación de Zeckendorf de n. Por
-- ejemplo, 
--    zeckendorf 100       == [89,8,3]
--    zeckendorf 2014      == [1597,377,34,5,1]
--    zeckendorf 28656     == [17711,6765,2584,987,377,144,55,21,8,3,1]
--    zeckendorf 14930396  == [14930352,34,8,2]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========
zeckendorf1 :: Integer -> [Integer]
zeckendorf1 n = reverse (head (aux n (tail fibs)))
    where aux 0 _ = [[]]
          aux n (x:y:zs) 
              | x <= n     = [x:xs | xs <- aux (n-x) zs] ++ aux n (y:zs)
              | otherwise  = []

-- fibs es la la sucesión de los números de Fibonacci. Por ejemplo,
--    take 14 fibs  == [1,1,2,3,5,8,13,21,34,55,89,144,233,377]
fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

-- 2ª solución
-- ===========
zeckendorf2 :: Integer -> [Integer]
zeckendorf2 n = aux n (reverse (takeWhile (<= n) fibs))
    where aux 0 _ = []
          aux n (x:xs) = x : aux (n-x) (dropWhile (>n-x) xs)

-- 3ª solución
-- ===========
zeckendorf3 :: Integer -> [Integer]
zeckendorf3 0 = []
zeckendorf3 n = x : zeckendorf3 (n - x) 
    where x = last (takeWhile (<= n) fibs)

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    ghci> zeckendorf1 300000
--    [196418,75025,17711,6765,2584,987,377,89,34,8,2]
--    (0.72 secs, 58478576 bytes)
--    ghci> zeckendorf2 300000
--    [196418,75025,17711,6765,2584,987,377,89,34,8,2]
--    (0.00 secs, 517852 bytes)
--    ghci> zeckendorf3 300000
--    [196418,75025,17711,6765,2584,987,377,89,34,8,2]
--    (0.00 secs, 515360 bytes)
-- Se observa que las definiciones más eficientes son la 2ª y la 3ª.

-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- Este ejercicio se basa en el problema 
-- [Zeckendorf representation](http://bit.ly/VB4yz6) 
-- de [Programming Praxis](http://programmingpraxis.com).
-- 
-- La representación de Zeckendorf se describe en el artículo de la
-- Wikipedia [Zeckendorf's theorem](http://bit.ly/VB2pU3). 
