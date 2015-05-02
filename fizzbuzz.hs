fizzbuzz = map
fizzer a
  | mod a 15 == 0 = "FizzBuzz!"
  | mod a  3 == 0 = "Fizz!"
  | mod a  5 == 0 = "Buzz!"
  | otherwise     = show a

main = putStrLn (show (fizzbuzz fizzer [1..100]))
