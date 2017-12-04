module Main where

import Data.Char (toUpper, isDigit, digitToInt)
import System.Random
import System.Exit (exitSuccess)
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import System.Console.ANSI (clearScreen)

type Morra = StateT Score (ReaderT Config IO)
type Score = (Int, Int)
data Gameplay = P2P | P2CPU deriving Show
newtype Config = Config {
                       gameplay :: Gameplay
                     }

playerInput :: String -> IO (Maybe Int)
playerInput i = do
  putStr i
  c <- getChar
  _ <- getChar
  case c of
    '1' -> return (Just 1)
    '2' -> return (Just 2)
    'q' -> return Nothing
    _   -> do
      putStrLn "Invalid input."
      playerInput i

scoreRound :: Int -> Morra ()
scoreRound total = modify f
  where f (a,b) = if odd total
                    then (a + 1, b)
                    else (a, b + 1)

printRoundResult :: Int -> IO ()
printRoundResult n = if odd n
                        then putStrLn "Odd Scores"
                        else putStrLn "Even Scores"

p2p :: Morra ()
p2p = do
  total <- liftIO $ do
    p1 <- playerInput "P1: "
    clearScreen
    p2 <- playerInput "P2: "
    return $ (+) <$> p1 <*> p2

  case total of
    Just t -> do
      scoreRound t
      liftIO $ printRoundResult t
      p2cpu
    Nothing -> return ()

p2cpu :: Morra ()
p2cpu = do
  total <- liftIO $ do
    p <- playerInput "P1: "
    c <- randomCPU
    putStrLn $ "C: " ++ show c
    return $ (+c) <$> p

  case total of
    Just t -> do
      scoreRound t
      liftIO $ printRoundResult t
      p2cpu
    Nothing -> return ()

morra :: Morra ()
morra = do
  liftIO clearScreen
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

  liftIO clearScreen
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
    'q' -> exitSuccess
    '1' -> return P2P
    '2' -> return P2CPU
    _   -> do
      putStrLn "Invalid input. Please enter: 1 2 or q"
      getGameplay

runMorra :: IO ()
runMorra = do
  putStrLn "Welcome to Morra game"
  gp <- getGameplay
  (_, score) <- flip runReaderT (Config gp)
           $ runStateT morra (0,0)
  printScore gp score

main :: IO ()
main = runMorra
