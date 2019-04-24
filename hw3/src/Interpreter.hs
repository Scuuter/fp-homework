{-# LANGUAGE InstanceSigs #-}

module Interpreter
  (
    run
  ) where

import Parser (
    Command(..)
  , Value(..)
  , Ref(..)
  , parseCommand
  )

import Control.Monad (when, unless)
import Control.Monad.Reader (ReaderT(..), ask, local, liftIO)

import qualified Data.HashMap.Strict as Map (HashMap, empty, insert, lookupDefault)
import Data.Maybe (isNothing, fromJust)

import System.Environment (getArgs)
import System.Directory (getCurrentDirectory, setCurrentDirectory)


import Text.Megaparsec (
    State(..)
  , PosState(..)
  , errorBundlePretty
  , stateInput
  , initialPos
  , defaultTabWidth
  , runParser'
  )

data Ctx = Ctx
  {
    input :: String
  , args :: [String]
  , vars :: Map.HashMap String String
  } deriving (Show)

run :: IO ()
run = do
  args' <- getArgs
  when (null args') $ ioError $ userError "You need to pass path to script as first argument"
  inp <- readFile $ head args'
  let ctx = Ctx inp args' Map.empty
  runReaderT shell ctx

shell :: ReaderT Ctx IO ()
shell = do
  ctx <- ask
  case runParser' parseCommand (initialState (input ctx)) of
    (state, Right cmd) -> exec (stateInput state) cmd
    (state, Left err) -> liftIO $ putStrLn (errorBundlePretty err)
      >> runReaderT shell (Ctx (skipLine $ stateInput state) (args ctx) (vars ctx))

skipLine :: String -> String
skipLine = dropWhile (/='\n')

exec :: String -> Command -> ReaderT Ctx IO ()
exec inp (Assign var quote) = local (
  \ (Ctx _ args' vars') -> Ctx inp args' (Map.insert var (concatMap (valToStr args' vars') quote) vars')
  ) shell
exec inp (Read v) = do
  str <- liftIO getLine
  local (\ (Ctx _ args' vars') -> Ctx inp args' (updateMap vars' v (words str))) shell
    where
      updateMap :: Map.HashMap String String -> [String] -> [String] -> Map.HashMap String String
      updateMap m _ [] = m
      updateMap m [] _ = m
      updateMap m [x] list = Map.insert x (unwords list) m
      updateMap m (x:xs) (y:ys) = updateMap (Map.insert x y m) xs ys
exec inp (Echo n quote) = do
  ctx <- ask
  let str = unwords $ fmap (valToStr (args ctx) (vars ctx)) quote
  if isNothing n
    then liftIO $ putStrLn str
    else liftIO $ putStr str
  local (\ (Ctx _ a b) -> Ctx inp a b) shell
exec inp Pwd = do
  dir <- liftIO getCurrentDirectory
  liftIO $ putStrLn dir
  local (\ (Ctx _ a b) -> Ctx inp a b) shell
exec inp (Cd path) = do
  liftIO $ setCurrentDirectory path
  local (\ (Ctx _ a b) -> Ctx inp a b) shell
exec _ (Exit code) = unless (isNothing code || code == Just 0) $
  liftIO $ putStrLn ("Exit code: " <> show (fromJust code))

valToStr :: [String] -> Map.HashMap String String -> Value -> String
valToStr _ _ (Str s) = s
valToStr arguments _ (Ref (Arg a)) = if a >= length arguments then "" else arguments !! a
valToStr _ variables (Ref (Var v)) = Map.lookupDefault "" v variables

initialState :: s -> State s
initialState s = State
  { stateInput  = s
  , stateOffset = 0
  , statePosState = PosState
    { pstateInput = s
    , pstateOffset = 0
    , pstateSourcePos = initialPos ""
    , pstateTabWidth = defaultTabWidth
    , pstateLinePrefix = ""
    }
  }
