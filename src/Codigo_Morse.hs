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
