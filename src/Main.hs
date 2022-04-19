import Text.Printf
import Data.Char
import Control.Monad.Fix

data ScoreResult = SWin | SResults String

evaluateGuess :: String -> String -> ScoreResult
evaluateGuess word guess =
  if word == guess then
    SWin
  else
    SResults $ map evalPair pairs
    where pairs = zip word guess
          nonmatches = map (\(first, _) -> first) $ filter (\(left, right) -> left /= right) pairs
          evalPair (left, right)
            | left == right = 'G'
            | elem right nonmatches = 'Y'
            | otherwise = ' '

-- TODO: Maybe verify that the word is in a dictionary?
validateGuess :: String -> Bool
validateGuess guess = length guess == 5 && all isAlpha guess

main :: IO ()
main = flip fix (5 :: Int) $ \loop lives -> do
  let word = "murky"
  case lives > 0 of
    False -> do
      putStrLn $ printf "You ran out of lives! The word was '%s'." word
      return ()
    True -> do
      putStrLn $ printf "You have %d live(s) remaining." lives
      putStrLn "Please input a guess:"
      guess <- getLine
      case validateGuess guess of
        False -> do
          putStrLn "\nPlease input a valid guess.\n"
          loop lives
        True -> do
          case evaluateGuess word guess of
            SWin -> do
              putStrLn $ printf "\nYou got it! The word was '%s'.\n" word
            SResults results -> do
              putStrLn $ printf "\nGuess: %s" guess
              putStrLn $ printf "       %s\n" results
              loop $ lives - 1
