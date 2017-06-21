-- Suma_si_todos_justos.hs
-- Suma si todos los valores son justos.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 17 de Abril de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sumaSiTodosJustos :: (Num a, Eq a) => [Maybe a] -> Maybe a
-- tal que (sumaSiTodosJustos xs) es justo la suma de todos los
-- elementos de xs si todos son justos (es decir, si Nothing no
-- pertenece a xs) y Nothing en caso contrario. Por ejemplo,
--    sumaSiTodosJustos [Just 2, Just 5]           == Just 7
--    sumaSiTodosJustos [Just 2, Just 5, Nothing]  == Nothing
-- ---------------------------------------------------------------------

import Data.Maybe

-- 1ª definición:
sumaSiTodosJustos :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos xs 
    | todosJustos xs = Just (sum [x | (Just x) <- xs])
    | otherwise      = Nothing
                       
-- (todosJustos xs) se verifica si todos los elementos de xs si todos
-- son justos (es decir, si Nothing no pertenece a xs) y Nothing en caso
-- contrario. Por ejemplo, 
--    todosJustos [Just 2, Just 5]           == True
--    todosJustos [Just 2, Just 5, Nothing]  == False

-- 1ª definición de todosJustos:
todosJustos :: Eq a => [Maybe a] -> Bool
todosJustos = notElem Nothing 

-- 2ª definición de todosJustos (con isJust):
todosJustos2 :: Eq a => [Maybe a] -> Bool
todosJustos2 = all isJust

-- Nota. La expresión (isJust x) se verifica si x es un valor justo. Por
-- ejemplo, 
--    isJust (Just 3)  == True
--    isJust Nothing  == False

-- 2ª definición de sumaSiTodosJustos (con fromJust):
sumaSiTodosJustos2 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos2 xs 
    | todosJustos xs = Just (sum [fromJust x | x <- xs])
    | otherwise      = Nothing

-- 3ª definición de sumaSiTodosJustos (con catMaybes):
sumaSiTodosJustos3 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos3 xs 
    | todosJustos xs = Just (sum (catMaybes xs))
    | otherwise      = Nothing

-- 4ª definición de sumaSiTodosJustos (con sequence):
sumaSiTodosJustos4 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos4 xs = suma (sequence xs)
    where suma Nothing   = Nothing
          suma (Just ys) = Just (sum ys)

-- Nota. En la definición anterior se usa la función  
--    sequence :: Monad m => [m a] -> m [a]
-- tal que (sequence xs) es la mónada obtenida evaluando cada una de las
-- de xs de izquierda a derecha. Por ejemplo,
--    sequence [Just 2, Just 5]   ==  Just [2,5]
--    sequence [Just 2, Nothing]  ==  Nothing
--    sequence [[2,4],[5,7]]      ==  [[2,5],[2,7],[4,5],[4,7]]
--    sequence [[2,4],[5,7],[6]]  ==  [[2,5,6],[2,7,6],[4,5,6],[4,7,6]]
--    sequence [[2,4],[5,7],[]]   ==  []

-- 5ª definición de sumaSiTodosJustos (con sequence y fmap):
sumaSiTodosJustos5 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos5 xs = fmap sum (sequence xs)

-- Nota. En la definición anterior se usa la función
--    fmap :: (a -> b) -> f a -> f b 
-- tal que (fmap f x) aplica la función f al functor x. Por ejemplo, 
--    fmap (+2) (Just 3)  ==  Just 5
--    fmap (+2) Nothing   ==  Nothing
--    fmap (+2) [3,7]     ==  [5,9]
--    fmap (+2) []        ==  []

-- 6ª definición de sumaSiTodosJustos (con sequence, fmap y (.)):
sumaSiTodosJustos6 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos6 = fmap sum . sequence 

