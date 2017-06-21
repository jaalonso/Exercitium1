-- Sustitucion_en_una_expresion.hs
-- Sustitución en una expresión.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 21 de Julio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La expresiones aritméticas se pueden representar mediante el
-- siguiente tipo  
--    data Expr = V Char 
--              | N Int 
--              | S Expr Expr
--              | P Expr Expr
--              deriving Show
-- por ejemplo, representa la expresión "z*(3+x)" se representa por
-- (P (V 'z') (S (N 3) (V 'x'))). 
--
-- Definir la función
--    sustitucion :: Expr -> [(Char, Int)] -> Expr
-- tal que (sustitucion e s) es la expresión obtenida sustituyendo las
-- variables de la expresión e según se indica en la sustitución s. Por
-- ejemplo, 
--    ghci> sustitucion (P (V 'z') (S (N 3) (V 'x'))) [('x',7),('z',9)]
--    P (N 9) (S (N 3) (N 7))
--    ghci> sustitucion (P (V 'z') (S (N 3) (V 'y'))) [('x',7),('z',9)]
--    P (N 9) (S (N 3) (V 'y'))
-- ---------------------------------------------------------------------
                   
data Expr = V Char 
          | N Int 
          | S Expr Expr
          | P Expr Expr
          deriving Show

sustitucion :: Expr -> [(Char, Int)] -> Expr
sustitucion e [] = e
sustitucion (V c) ((d,n):ps) | c == d = N n
                             | otherwise = sustitucion (V c) ps
sustitucion (N n) _ = N n                                 
sustitucion (S e1 e2) ps = S (sustitucion e1 ps) (sustitucion e2 ps)
sustitucion (P e1 e2) ps = P (sustitucion e1 ps) (sustitucion e2 ps)

