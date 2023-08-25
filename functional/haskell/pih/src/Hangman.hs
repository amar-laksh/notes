module Hangman (
    hangman,
)
where

hangman :: IO ()
hangman = do
    putStrLn "Think of a word: "
    word <- getLine
    putStrLn "Try to guess the word:"
    play word

play :: String -> IO ()
play word = do
    putStr "? "
    guess <- getLine
    if guess == word
        then putStrLn "You guessed it!!"
        else do
            putStrLn (match' word guess)
            play word

match' :: String -> String -> String
match' word guess = [if c `elem` guess then c else '-' | c <- word]
