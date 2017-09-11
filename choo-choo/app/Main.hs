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
    Nothing -> case ignoreChecks of
      True  -> coloredPrint Yellow "WARNING: no packages specified.\n"
      False -> coloredPrint Yellow "WARNING: no packages specified, so only check changelogs.\n"
