import Data.Char (toLower)

-- Función que convierte un número entero en letras
numToWords :: Int -> String
numToWords n
    | n < 20           = units !! n
    | n `mod` 10 == 0  = tens !! (n `div` 10 - 2)
    | otherwise        = tens !! (n `div` 10 - 2) ++ "-" ++ units !! (n `mod` 10)
  where
    units = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
             "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
    tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

-- Función principal que devuelve "Fizz!", "Buzz!", "FizzBuzz!" o el número en letras según el caso
fizzBuzz :: Int -> String
fizzBuzz n
    | n `mod` 15 == 0 = "FizzBuzz!"
    | n `mod` 5 == 0  = "Fizz!"
    | n `mod` 3 == 0  = "Buzz!"
    | otherwise       = numToWords n

-- Función principal que lee un entero desde la entrada estándar y muestra el resultado
main :: IO ()
main = do
    putStrLn "Ingrese un número entre 0 y 100:"
    input <- getLine
    let number = read input :: Int
    if number >= 0 && number <= 100
        then putStrLn $ fizzBuzz number
        else putStrLn "Número fuera del rango válido (0-100)."
