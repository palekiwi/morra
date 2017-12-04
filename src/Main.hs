module Main where

import Data.Char (toUpper, isDigit, digitToInt)
import System.Random
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

type Morra = StateT Score (ReaderT Config IO)
type Score = (Int, Int)
data Gameplay = P2P | P2CPU deriving Show
newtype Config = Config {
                       gameplay :: Gameplay
                     }

scoreRound :: Int -> Score -> Score
scoreRound total (a,b) =
  if odd total
     then (a + 1, b)
     else (a, b + 1)

printRoundResult :: Int -> IO ()
printRoundResult n = if odd n
                        then putStrLn "Player Scores"
                        else putStrLn "Computer Scores"

p2p :: Morra ()
p2p = undefined

p2cpu :: Morra ()
p2cpu = do
  liftIO $ putStr "P: "
  p <- liftIO getChar
  _ <- liftIO getChar
  when (isDigit p) $ do
    let p' = digitToInt p
    c <- liftIO randomCPU
    liftIO . putStrLn $ "C: " ++ show c
    let total = p' + c
    modify (scoreRound total)
    liftIO $ printRoundResult total
    p2cpu

morra :: Morra ()
morra = do
  gp <- lift (asks gameplay)
  case gp of
    P2P -> do
      liftIO $ putStrLn "Player vs Player Mode"
      p2p
    P2CPU -> do
      liftIO $ putStrLn "Player vs CPU Mode"
      p2cpu

randomCPU :: IO Int
randomCPU = getStdRandom (randomR (1,2))

printScore :: Gameplay -> Score -> IO ()
printScore gp (a,b) = do
  let (p1, p2) = case gp of
                  P2P -> ("Player 1", "Player 2")
                  P2CPU -> ("Player", "Computer")

  putStrLn "#####################"
  putStrLn "#### GAME RESULT ####"
  putStrLn $ "# " ++ p1 ++ " score: " ++ show a
  putStrLn $ "# " ++ p2 ++ " score: " ++ show b
  case compare a b of
    GT -> putStrLn $ toUpper <$> "# " ++ p1 ++ " wins"
    LT -> putStrLn $ toUpper <$> "# " ++ p2 ++ " wins"
    EQ -> putStrLn $ toUpper <$> "# It's a draw."
  putStrLn "#####################"

getGameplay :: IO Gameplay
getGameplay = do
  putStrLn "Select gameplay:"
  putStrLn "(1) Player vs Player"
  putStrLn "(2) Player vs CPU"
  putStrLn "(q) Quit"
  c <- getChar
  _ <- getChar
  case c of
    '1' -> return P2P
    '2' -> return P2CPU
    _   -> do
      putStrLn "Invalid input. Please enter 1 2 or q"
      getGameplay

main :: IO ()
main = do
  putStrLn "Welcome to Morra game"
  gp <- getGameplay
  (_, score) <- flip runReaderT (Config gp)
           $ runStateT morra (0,0)
  printScore gp score
