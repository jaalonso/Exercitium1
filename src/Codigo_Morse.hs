-- Codigo_Morse.hs
-- Código Morse.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 9 de Julio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Introducción                                                     --
-- ---------------------------------------------------------------------

-- El [código Morse](http://bit.ly/1neNnh9) es un sistema de
-- representación de letras y números mediante señales emitidas de forma
-- intermitente.
--
-- A los signos (letras mayúsculas o dígitos) se le asigna un código
-- como se muestra a continuación 
--    |---+-------|---+-------|---+-------|---+-------|
--    | A | .-    | J | .---  | S | ...   | 1 | ..--- |
--    | B | -...  | K | -.-   | T | -     | 2 | ...-- |
--    | C | -.-.  | L | .-..  | U | ..-   | 3 | ....- |
--    | D | -..   | M | --    | V | ...-  | 4 | ..... |
--    | E | .     | N | -.    | W | .--   | 5 | -.... |
--    | F | ..-.  | O | ---   | X | -..-  | 6 | --... |
--    | G | --.   | P | .--.  | Y | -.--  | 7 | ---.. |
--    | H | ....  | Q | --.-  | Z | --..  | 8 | ----. |
--    | I | ..    | R | .-.   | 0 | .---- | 9 | ----- |
--    |---+-------|---+-------|---+-------|---+-------|
--
-- El código Morse de las palabras se obtiene a partir del de sus
-- caracteres insertando un espacio entre cada uno. Por ejemplo, el
-- código de "todo" es "- --- -.. ---"
--
-- El código Morse de las frase se obtiene a partir del de sus
-- palabras insertando un espacio entre cada uno. Por ejemplo, el
-- código de "todo o nada" es "- --- -.. ---  ---  -. .- -.. .-"

-- ---------------------------------------------------------------------
-- § Enunciado                                                        --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir las funciones
--    fraseAmorse :: String -> String
--    morseAfrase :: String -> String
-- tales que
-- * (fraseAmorse cs) es la traducción de la frase cs a Morse. Por
--   ejemplo, 
--      ghci> fraseAmorse "En todo la medida"
--      ". -.  - --- -.. ---  .-.. .-  -- . -.. .. -.. .-"
-- * (morseAfrase cs) es la frase cuya traducción a Morse es cs. Por 
--   ejemplo, 
--      ghci> morseAfrase ". -.  - --- -.. ---  .-.. .-  -- . -.. .. -.. .-"
--      "EN TODO LA MEDIDA"
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Solución                                                         --
-- ---------------------------------------------------------------------

import Data.Char (toUpper)
import Data.List (intercalate)
import Data.List.Split (splitOn)

-- caracteres es la lista ordenada de las caracteres (letras mayúsculas
-- y dígitos) que se usan en los mensajes Morse.
caracteres :: [Char]
caracteres = ['A'..'Z'] ++ ['0'..'9']

-- morse es la lista de los códigos Morse correspondientes a la lista
-- de caracteres.
morse :: [String]
morse = [".-","-...","-.-.","-..",".","..-.","--.","....","..",".---",
         "-.-",".-..","--","-.","---",".--.","--.-",".-.","...","-",
         "..-","...-",".--","-..-","-.--","--..",".----","..---","...--",
         "....-",".....","-....","--...","---..","----.","-----"]

-- (correspondiente xs ys x) es el elemento de ys en la misma posición
-- que x en xs. Por ejemplo,
--    correspondiente [1..10] [2,4..20] 3  ==  6
correspondiente :: Ord a => [a] -> [b] -> a -> b
correspondiente xs ys x = head [y | (z,y) <- zip xs ys, z == x]

-- (caracterAmorse x) es el código Morse correspondiente al carácter
-- x. Por ejemplo, 
--    caracterAmorse 'A'  ==  ".-"
--    caracterAmorse 'B'  ==  "-..."
--    caracterAmorse '1'  ==  "..---"
--    caracterAmorse 'a'  ==  ".-"
caracterAmorse :: Char -> String
caracterAmorse = correspondiente caracteres morse . toUpper

-- (morseAcaracter x) es el carácter cuyo código Morse es x. Por
-- ejemplo,  
--    morseAcaracter ".-"     ==  'A'
--    morseAcaracter "-..."   ==  'B'
--    morseAcaracter "..---"  ==  '1'
morseAcaracter :: String -> Char
morseAcaracter = correspondiente morse caracteres

-- (palabraAmorse cs) es el código Morse correspondiente a la palabra
-- cs. Por ejemplo,
--    palabraAmorse "En"  ==  ". -."
palabraAmorse :: [Char] -> String
palabraAmorse = unwords . map caracterAmorse

-- (morseApalabra cs) es la palabra cuyo traducción a Morse es cs. Por
-- ejemplo, 
--    morseApalabra ". -."  ==  "EN"
morseApalabra :: String -> [Char]
morseApalabra = map morseAcaracter . words

-- (fraseAmorse cs) es la traducción de la frase cs a Morse. Por ejemplo,
--    ghci> fraseAmorse "En todo la medida"
--    ". -.  - --- -.. ---  .-.. .-  -- . -.. .. -.. .-"
fraseAmorse :: String -> String
fraseAmorse = intercalate "  " . map palabraAmorse . words

-- Ejemplo de cálculo
--    fraseAmorse "En todo la medida"
--    = (intercalate "  " . map palabraAmorse . words)
--      "En todo la medida"
--    = (intercalate "  " . map palabraAmorse)
--      ["En","todo","la","medida"]
--    = intercalate "  " [". -.","- --- -.. ---",".-.. .-","-- . -.. .. -.. .-"]
--    = ". -.  - --- -.. ---  .-.. .-  -- . -.. .. -.. .-"
  
-- (morseAfrase cs) es la frase cuya traducción a Morse es cs. Por
-- ejemplo, 
--    ghci> morseAfrase ". -.  - --- -.. ---  .-.. .-  -- . -.. .. -.. .-"
--    "EN TODO LA MEDIDA"
morseAfrase :: String -> String
morseAfrase = unwords . map morseApalabra . splitOn "  "

-- Ejemplo de cálculo
--    morseAfrase ". -.  - --- -.. ---  .-.. .-  -- . -.. .. -.. .-"
--    = (unwords . map morseApalabra)
--      ". -.  - --- -.. ---  .-.. .-  -- . -.. .. -.. .-"
--    = (unwords . map morseApalabra)
--      [". -.","- --- -.. ---",".-.. .-","-- . -.. .. -.. .-"]
--    = unwords ["EN","TODO","LA","MEDIDA"]
--    = "EN TODO LA MEDIDA"

-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Soluciones en los comentarios                                    --
-- ---------------------------------------------------------------------

-- Eduardo
-- =======

-- import Data.List
-- import Data.List.Split

fraseAmorseC1 xs = aux xs
    where
      aux [] = []
      aux (x:xs) 
          | x == ' '  = " " ++ aux xs
          | otherwise = concat [b ++ " "| (a,b) <- mezcla, a == x] ++ aux xs

morseAfraseC1 [] = []
morseAfraseC1  xs = separa (map aux (splitOn "  " xs))
    where
      aux [] = []
      aux xs = [b | (a,b) <- mezcla1, a == takeWhile (/=' ') xs] ++
               (aux (ultimo (dropWhile (/= ' ') xs)))
      separa [] = []
      separa (x:xs) = x ++ " " ++ separa xs
 
letrasYnumeros =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz"

ultimo [] = []
ultimo (x:xs) = xs

enMorse = concat (replicate 2
    [".-", "-...", "-.-.", "-..", ".", "..-.", "--.", "....", "..",
     ".---", "-.-", ".-..", "--", "-.", "---", ".--.", "--.-", ".-.", 
     "...", "-", "..-", "...-", ".--", "-..-", "-.--", "--..", ".----", 
     "..---", "...--", "....-", ".....", "-....", "--...", "---..",
     "----.", "-----"])

enMorse1 =  [".-", "-...", "-.-.", "-..", ".", "..-.", "--.", "....", 
             "..", ".---", "-.-", ".-..", "--", "-.", "---", ".--.", 
             "--.-", ".-.", "...", "-", "..-", "...-", ".--", "-..-", 
             "-.--", "--..", ".----", "..---", "...--", "....-", 
             ".....", "-....", "--...", "---..", "----.", "-----"]

mezcla = zip letrasYnumeros enMorse
mezcla1 = zip enMorse1 letrasYnumeros

-- David
-- =====

-- import Data.List.Split
-- import Data.Char
 
fraseAmorseC2 :: String -> String
fraseAmorseC2 cs = concat $ init $ frase cs 
 
frase :: [Char] -> [[Char]]
frase "" = []
frase (c:cs)|c == ' ' = " ":frase cs
            |otherwise = cdfca c:" ":frase cs
 
cdfca :: Char -> [Char]
cdfca c = head $ [m |(v,m)<-cdgMrs,v== toUpper c]  
 
morseAfraseC2 :: String -> String
morseAfraseC2 cs =  morseC2 (splitOn " " cs)
 
morseC2 :: [[Char]] -> [Char]
morseC2 [] =[]
morseC2 (c:cs) |c=="" = ' ': morseC2 cs 
               |otherwise = dcdfca c: morseC2 cs 
 
dcdfca :: [Char] -> Char
dcdfca c = head $ [v |(v,m)<-cdgMrs,m == c]
 
cdgMrs :: [(Char, [Char])]
cdgMrs = zip (['A'..'Z']++['0'..'9'])
              [".-","-...","-.-.","-..",".","..-.","--.","....",".."
              ,".---","-.-",".-..","--","-.","---",".--.","--.-"
              ,".-.","...","-","..-","...-",".--","-..-","-.--","--.."
              ,".----","..---","...--","....-",".....","-....","--..."
              ,"---..","----.","-----"]
