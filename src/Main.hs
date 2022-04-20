import Text.Printf
import Data.Char
import Control.Monad.Fix

data Color = Red | Green | Yellow | Gray

colorEscape :: Color -> String
colorEscape color =
  case color of
    Red -> "\x1b[31m"
    Green -> "\x1b[32m"
    Yellow -> "\x1b[33m"
    Gray -> "\x1b[38;5;242m"

colorString :: Color -> String -> String
colorString color string = colorEscape color ++ string ++ "\x1b[0m"

printColor :: Color -> String -> IO ()
printColor color = putStrLn . colorString color

data GameState = GameState { stateLives :: Int, stateResults :: [String] }

startingState :: GameState
startingState = GameState { stateLives = 6, stateResults = [] }

printState :: GameState -> IO ()
printState state = do
  if (length $ stateResults state) /= 0 then do
    putStrLn ""
    mapM_ putStrLn $ reverse $ stateResults state
  else return ()
  putStrLn $ printf "\nYou have %s live(s) left." (colorString Green $ show $ stateLives state)

data ScoreResult = SWin | SResults String

evaluateGuess :: String -> String -> ScoreResult
evaluateGuess word guess
  | word == guess = SWin
  | otherwise = SResults $ concat $ map (\pair@(_, right) -> colorString (evalPair pair) [right]) pairs
    where pairs = zip word guess
          nonmatches = map (\(first, _) -> first) $ filter (\(left, right) -> left /= right) pairs
          evalPair (left, right)
            | left == right = Green
            | elem right nonmatches = Yellow
            | otherwise = Gray

-- TODO: Maybe verify that the word is in a dictionary?
validateGuess :: String -> Bool
validateGuess guess = length guess == 5 && all isAlpha guess

main :: IO ()
main = flip fix startingState $ \loop state -> do
  let word = "murky"
  case stateLives state > 0 of
    False -> do
      printColor Red $ printf "You ran out of lives! The word was %s." (colorString Green word)
      return ()
    True -> do
      printState state
      putStrLn "Please input a guess:"
      _guess <- getLine
      let guess = map toLower _guess
      case validateGuess guess of
        False -> do
          printColor Red "\nPlease input a valid guess.\n"
          loop state
        True -> do
          case evaluateGuess word guess of
            SWin -> do
              putStrLn $ printf "\nYou got it! The word was %s." (colorString Green word)
            SResults results -> do
              loop state{ stateLives = stateLives state - 1, stateResults = results:stateResults state }
