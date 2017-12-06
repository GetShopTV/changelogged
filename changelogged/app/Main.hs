{-# LANGUAGE RecordWildCards #-}
module Main where

import Turtle
import Data.Maybe (fromMaybe, fromJust)

import System.Console.ANSI (Color(..))

import CheckLog.Check
import Bump.API
import Bump.Project
import Types
import Settings

main :: IO ()
main = do
  opts@Options{..} <- options welcome parser

  paths <- loadPaths

  processChecks opts optNoCheck
    (fst <$> swaggerFileName paths)
    (fromMaybe "CHANGELOG.md" (changeLog paths))
    (fromMaybe "API_CHANGELOG.md" (apiChangeLog paths))

  when optNoBump $ exit ExitSuccess

  newVersion <- case optPackagesLevel of
    Nothing -> generateVersionByChangelog optNoCheck (fromMaybe "CHANGELOG.md" (changeLog paths))
    Just lev -> Just <$> generateVersion lev

  newApiVersion <- case optApiLevel of
    Nothing -> case swaggerFileName paths of
      Just swagger -> generateAPIVersionByChangelog optNoCheck swagger (fromMaybe "API_CHANGELOG.md" (apiChangeLog paths))
      Nothing -> do
        coloredPrint Yellow "Cannot generate API version - no file with previous version specified in .paths (swaggerFileName).\n"
        return Nothing
    Just lev -> case swaggerFileName paths of
        Just swagger -> Just <$> generateAPIVersion lev swagger
        Nothing -> do
          coloredPrint Yellow "Cannot generate API version - no file with previous version specified in .paths (swaggerFileName).\n"
          return Nothing

  case newVersion of
    Nothing -> return ()
    Just version -> case optPackages of
      Just packages -> bumpPackages version packages
      Nothing -> case defaultPackages paths of
        Just defaults -> do
          coloredPrint Green "Bump packages found in ./paths.\n"
          bumpPackages version defaults
          case packagesPathsWithVars paths of
            Just files -> mapM_ (bumpPart version) files
            Nothing -> return ()
        Nothing -> coloredPrint Yellow "WARNING: no packages specified.\n"

  case apiPathsWithVars paths of
    Nothing -> do
      coloredPrint Green "If you want to bump API version, specify paths in ./paths\n"
      return ()
    Just apiPathList -> case newApiVersion of
      Nothing -> return ()
      Just ver -> do
        curVersion <- currentAPIVersion (fromJust $ swaggerFileName paths) -- guaranted, must be gone with refactoring.
        printf ("Version: "%s%" -> ") curVersion
        coloredPrint Yellow (ver <> "\n")
        printf ("Updating API version to "%s%"\n") ver
        mapM_ (bumpAPIPart ver) apiPathList

