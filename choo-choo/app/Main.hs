module Main where

import qualified Data.Text as T

import Turtle
import Data.Maybe (fromMaybe)

import System.Console.ANSI (Color(..))

import Bump
import Types
import Settings

main :: IO ()
main = do
  (packages, packageLev, apiLev, ignoreChecks, fromStart, force) <- options welcome parser

  paths <- loadPaths

  cd ".."
  
  processChecks ignoreChecks fromStart force (fst <$> swaggerFileName paths)
                (fromMaybe "CHANGELOG.md" (changeLog paths))
                (fromMaybe "API_CHANGELOG.md" (apiChangeLog paths))

  newVersion <- case packageLev of
    Nothing -> generateVersionByChangelog ignoreChecks Project Nothing (fromMaybe "CHANGELOG.md" (changeLog paths))
    Just lev -> generateVersion (levelFromText lev) Project Nothing
  
  newApiVersion <- case apiLev of
    Nothing -> do
      newV <- generateVersionByChangelog ignoreChecks API (swaggerFileName paths) (fromMaybe "API_CHANGELOG.md" (apiChangeLog paths))
      return $ case newV of
        "" -> Nothing
        ver -> Just ver
    Just lev -> do
      newV <- generateVersion (levelFromText lev) API (swaggerFileName paths)
      return $ case newV of
        "" -> Nothing
        ver -> Just ver
  
  case newVersion of
    "!" -> return ()
    version -> case packages of
      Just project -> bumpPackages version (T.split (==' ') project)
      Nothing -> case defaultPackages paths of
        Just defaults -> do
          coloredPrint Green "Bump packages found in ./paths.\n"
          bumpPackages version defaults
          case packagesPathsWithVars paths of
            Nothing -> return ()
            Just anotherFiles -> mapM_ (bumpPart API version) anotherFiles
        Nothing -> coloredPrint Yellow "WARNING: no packages specified.\n"

  case apiPathsWithVars paths of
    Nothing -> do
      coloredPrint Green "If you want to bump API version, specify paths in ./paths\n"
      return ()
    Just apiPathList -> case newApiVersion of
      Just "!" -> return ()
      Just ver -> do
        curVersion <- currentVersion API (swaggerFileName paths)
        printf ("Version: "%s%" -> ") curVersion
        coloredPrint Yellow (ver <> "\n")
        printf ("Updating API version to "%s%"\n") ver
        mapM_ (bumpPart API ver) apiPathList
      Nothing -> coloredPrint Red "Cannot determine current API version, add swagger file name to ./paths.\n"
    
