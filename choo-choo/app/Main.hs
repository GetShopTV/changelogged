module Main where

import qualified Data.Text as T

import Turtle

import System.Console.ANSI (Color(..))

import Bump
import Types
import Settings

main :: IO ()
main = do
  (packages, packageLev, ignoreChecks, fromStart, force) <- options welcome parser

  paths <- loadPaths

  cd ".."

  processChecks ignoreChecks fromStart paths force

  newVersion <- case packageLev of
    Nothing -> generateVersionByChangelog ignoreChecks
    Just lev -> generateVersion (levelFromText lev)
  
  case packages of
    Just project -> bumpPackages newVersion (T.split (==' ') project)
    Nothing -> case defaultPackages paths of
        Just defaults -> do
          coloredPrint Green "Bump packages found in ./paths.\n"
          bumpPackages newVersion defaults
        Nothing -> coloredPrint Yellow "WARNING: no packages specified.\n"

  case apiPathsWithVars paths of
    Nothing -> do
      coloredPrint Green "If you want to bump API version, specify paths in ./paths\n"
      return ()
    Just apiPathList -> return () -- Not implemented yet.
    
