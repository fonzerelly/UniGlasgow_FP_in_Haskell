import System.Random
import System.Time

main = pick

pick = do
    let randomWords = ["monade", "monoid", "functor", "category", "endofunctor"]
    index <- rand (length randomWords)
    let result = (randomWords !! index)
    mkguess result (take (length result) $ repeat '_') 3

rand :: (Num a, Random a) => a -> IO a
rand max = do {
   randomRIO (0, max)
}

-- pick' = do pick $ getClockTime

mkguess :: String -> String -> Int -> IO()
mkguess word display n = do putStrLn(display ++ "  " ++ take n (repeat '*'));
                            putStr "  Enter your guess: "
                            guess <- getLine
                            let (correct, display') = check word display (guess!!0)
                            let n' = if correct then n else n-1
                            turn word display' n'

check :: String -> String -> Char -> (Bool, String)
check word display c = (c `elem` word, map match (zip word display))
   where
      match (x, y)
         | x == c = c
         | x /= c = y

turn :: String -> String -> Int -> IO ()
turn word display n
   | n == 0 = do putStrLn "You lose"
   | word==display = do putStrLn "You win!"
   | word/=display = do mkguess word display n
