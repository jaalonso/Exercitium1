-- Cadenas0y1.hs
-- Cadenas de ceros y unos.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 21 de Julio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la constante
--    cadenasDe0y1 :: [String]
-- tal que cadenasDe0y1 es la cadena de ceros y unos. Por ejemplo,
--    ghci> take 10 cadenasDe0y1
--    ["","0","1","00","10","01","11","000","100","010"]
-- ---------------------------------------------------------------------

cadenasDe0y1 :: [String]
cadenasDe0y1 = "" : concat [['0':cs, '1':cs] | cs <- cadenasDe0y1]

