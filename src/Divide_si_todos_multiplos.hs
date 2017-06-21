-- Divide_si_todos_multiplos.hs
-- Divide si todos son múltiplos.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 17 de Abril de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio. Definir la función
--    divideSiTodosMultiplos :: Integral a => a -> [a] -> Maybe [a]
-- tal que (divideSiTodosMultiplos x ys) es justo la lista de los
-- cocientes de los elementos de ys entre x si todos son múltiplos de x
-- y Nothing en caso contrario. Por ejemplo,
--    divideSiTodosMultiplos 2 [6,10,4]  ==  Just [3,5,2]
--    divideSiTodosMultiplos 2 [6,10,5]  ==  Nothing
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión)
divideSiTodosMultiplos1 :: Integral a => a -> [a] -> Maybe [a]
divideSiTodosMultiplos1 x ys
    | todosMultiplos x ys = Just [y `div` x | y <- ys]
    | otherwise           = Nothing

-- (todosMultiplos x ys) se verifica si todos los elementos de ys son
-- múltiplos de x. Por ejemplo,
--    todosMultiplos 2 [6,10,4]  ==  True
--    todosMultiplos 2 [6,10,5]  ==  False
todosMultiplos :: Integral a => a -> [a] -> Bool
todosMultiplos x ys = and [y `mod` x == 0 | y <- ys]

-- 2ª definición (por recursión)
divideSiTodosMultiplos2 :: Integral a => a -> [a] -> Maybe [a]
divideSiTodosMultiplos2 _ [] = Just []
divideSiTodosMultiplos2 x (y:ys)
    | y `mod` x /= 0 = Nothing
    | aux == Nothing  = Nothing
    | otherwise       = Just ((y `div` x) : zs)
    where aux = divideSiTodosMultiplos2 x ys
          Just zs = aux

-- 3ª definición (con sequence y map)
divideSiTodosMultiplos3 :: Integral a => a -> [a] -> Maybe [a]
divideSiTodosMultiplos3 x ys =
    sequence (map (x `divide`) ys)
    where divide x y | y `mod` x == 0 = Just (y `div` x)
                     | otherwise      = Nothing

-- Nota. En la definición anterior se usa la función  
--    sequence :: Monad m => [m a] -> m [a]
-- tal que (sequence xs) es la mónada obtenida evaluando cada una de las
-- de xs de izquierda a derecha. Por ejemplo,
--    sequence [Just 2, Just 5]   ==  Just [2,5]
--    sequence [Just 2, Nothing]  ==  Nothing
--    sequence [[2,4],[5,7]]      ==  [[2,5],[2,7],[4,5],[4,7]]
--    sequence [[2,4],[5,7],[6]]  ==  [[2,5,6],[2,7,6],[4,5,6],[4,7,6]]
--    sequence [[2,4],[5,7],[]]   ==  []

-- 4ª definición (con sequence, map y (.))
divideSiTodosMultiplos4 :: Integral a => a -> [a] -> Maybe [a]
divideSiTodosMultiplos4 x = sequence . map (x `divide`)
    where divide x y | y `mod` x == 0 = Just (y `div` x)
                     | otherwise      = Nothing

-- 5ª definición (con sequence y map)
divideSiTodosMultiplos5 :: Integral a => a -> [a] -> Maybe [a]
divideSiTodosMultiplos5 x = mapM (x `divide`)
    where divide x y | y `mod` x == 0 = Just (y `div` x)
                     | otherwise      = Nothing

-- Nota. En la definición anterior se usa la función mapM ya que
--    mapM f 
-- es equivalente a
--    sequence . map f
-- Por ejemplo,
--    ghci> mapM (\n -> if even n then Just (2*n) else Nothing) [4,6,10]
--    Just [8,12,20]
--    ghci> mapM (\n -> if even n then Just (2*n) else Nothing) [4,6,11]
--    Nothing
