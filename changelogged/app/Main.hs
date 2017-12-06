{-# LANGUAGE RecordWildCards #-}
module Main where

import Turtle
import Data.Maybe (fromMaybe, fromJust)

import System.Console.ANSI (Color(..))

import CheckLog.Check
import Bump.API
import Bump.Project
import Types
import Git
import Options
import Utils
import Settings

projectMain :: Paths -> Options -> Git -> IO ()
projectMain paths opts@Options{..} git = do
  coloredPrint Green "Checking changelog file and creating it if missing.\n"
  touch (chLog paths)

  bump <- checkChangelogWrap opts git optNoCheck (chLog paths)

  when (bump && not optNoBump) $ do
    newVersion <- case optPackagesLevel of
      Nothing -> generateVersionByChangelog optNoCheck (chLog paths)
      Just lev -> Just <$> generateVersion lev
  
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
  where
    chLog cfg = fromMaybe "CHANGELOG.md" (changeLog cfg)

apiMain :: Paths -> Options -> Git -> IO ()
apiMain paths opts@Options{..} git = do
  coloredPrint Green "Checking API changelog file and creating it if missing.\n"
  touch (apiChLog paths)

  bumpA <- checkAPIChangelogWrap opts git optNoCheck (fst <$> swaggerFileName paths) (apiChLog paths)

  when (bumpA && not optNoBump) $ do
    newApiVersion <- case optApiLevel of
      Nothing -> case swaggerFileName paths of
        Just swagger -> generateAPIVersionByChangelog optNoCheck swagger (apiChLog paths)
        Nothing -> do
          coloredPrint Yellow "Cannot generate API version - no file with previous version specified in .paths (swaggerFileName).\n"
          return Nothing
      Just lev -> case swaggerFileName paths of
          Just swagger -> Just <$> generateAPIVersion lev swagger
          Nothing -> do
            coloredPrint Yellow "Cannot generate API version - no file with previous version specified in .paths (swaggerFileName).\n"
            return Nothing

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
  where
    apiChLog cfg = fromMaybe "API_CHANGELOG.md" (apiChangeLog cfg)

main :: IO ()
main = do
  opts@Options{..} <- options welcome parser

  paths <- loadPaths

  git <- gitData optFromBC

  projectMain paths opts git

  when optApiExists $ apiMain paths opts git
  
  sh $ rm $ gitHistory git
