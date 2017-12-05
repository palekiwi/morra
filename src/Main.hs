module Main where

import Data.Char (toUpper)
import System.Random
import System.Exit (exitSuccess)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import System.Console.ANSI (clearScreen)

data Game = Game {
                   score :: Score
                 , plays :: [Int]
                 , trigrams :: Trigrams
                 }

type Trigrams = Map (Int, Int) Int

type Morra = StateT Game (ReaderT Config IO)
type Score = (Int, Int)
data Gameplay = P2P | P2CPU deriving Show
newtype Config = Config { gameplay :: Gameplay }

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
  where f s@Game{score = (a,b)} = if odd total
                                    then s{score = (a + 1, b)}
                                    else s{score = (a, b + 1)}


printRoundResult :: Int -> IO ()
printRoundResult n = if odd n
                        then putStrLn "Odd Scores"
                        else putStrLn "Even Scores"

playersInput :: MaybeT IO Int
playersInput = do
  p1 <- MaybeT $ playerInput "P1: "
  liftIO clearScreen
  p2 <- MaybeT $ playerInput "P2: "
  return $ p1 + p2

p2p :: Morra ()
p2p = do
  total <- liftIO (runMaybeT playersInput)
  case total of
    Just t -> do
      scoreRound t
      liftIO $ printRoundResult t
      p2p
    Nothing -> return ()

p2cpu :: Morra ()
p2cpu = do
  p <- liftIO $ playerInput "P1: "

  case p of
    Just p' -> do
      g <- get
      c <- liftIO $ smartCPU g
      recordPlay p'
      liftIO $ putStrLn $ "C: " ++ show c
      let t = p' + c
      scoreRound t
      liftIO $ printRoundResult t
      p2cpu
    Nothing -> return ()

smartCPU :: Game -> IO Int
smartCPU (Game _ ps t) =
  if length ps > 3
     then do
       let [a,b] = take 2 ps
           p' = M.lookup (a,b) t
       case p' of
         Just p -> return p
         Nothing -> randomCPU
     else randomCPU

recordPlay :: Int -> Morra ()
recordPlay p = do
  g@(Game _ ps t) <- get
  if length ps >= 2
     then do
       let [a,b] = take 2 ps
           t' = M.insert (a,b) p t
       modify (\s -> g{plays = p:ps, trigrams = t'})
      else modify (\s -> g{plays=p:ps})

morra :: Morra ()
morra = do
  liftIO clearScreen
  gp <- lift (asks gameplay)
  case gp of
    P2P -> do
      liftIO $ putStrLn "Player vs Player Mode"
      liftIO $ putStrLn "Player1 (P1) is even, P2 (P2) is odds"
      p2p
    P2CPU -> do
      liftIO $ putStrLn "Player vs CPU Mode"
      liftIO $ putStrLn "Player (P) is even, CPU (C) is odds"
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
  putStrLn $ "# " ++ p1 ++ " score: " ++ show a ++ " #"
  putStrLn $ "# " ++ p2 ++ " score: " ++ show b ++ " #"
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
  (_, s) <- flip runReaderT (Config gp)
           $ runStateT morra (Game (0,0) [] M.empty)
  printScore gp (score s)

main :: IO ()
main = runMorra
