{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.Main where

import Prelude hiding (FilePath)
import Turtle

import Control.Exception
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)

import System.Console.ANSI (Color(..))

import Changelogged.CheckLog.Check
import Changelogged.Bump.Local
import Changelogged.Bump.Common
import Changelogged.Bump.General
import Changelogged.Types
import Changelogged.Git
import Changelogged.Options
import Changelogged.Utils
import Changelogged.Pure (showPath, fromJustCustom, defaultedEmpty, showText)
import Changelogged.Settings

commonMain :: Paths -> Options -> Git -> IO ()
commonMain paths opts@Options{..} git = do
  coloredPrint Green ("Checking " <> showPath (taggedLogPath $ chLog paths) <> " and creating it if missing.\n")
  touch $ taggedLogPath (chLog paths)

  bump <- checkChangelogWrap opts git optNoCheck (chLog paths)

  (when (bump && optBumpVersions) $ do
    newVersion <- case optPackagesLevel of
      Nothing -> generateVersionByChangelog optNoCheck (taggedLogPath $ chLog paths) (gitRevision git)
      Just lev -> Just <$> generateVersion lev (gitRevision git)
  
    case newVersion of
      Nothing -> return ()
      Just version -> case HM.lookup "main" (defaultedEmpty (versioned paths)) of
        Just files -> do
          printf ("Version: "%s%" -> ") (gitRevision git)
          coloredPrint Yellow (version <> "\n")
          mapM_ (bumpPart version) files
          headChangelog version (taggedLogPath $ chLog paths)
        Nothing -> coloredPrint Yellow "WARNING: no files to bump project version in specified.\n"
    ) `catch` (\(ex :: PatternMatchFail) -> coloredPrint Red (showText ex))
  where
    chLog cfg = HM.lookupDefault (TaggedLog "ChangeLog.md" Nothing) "main"
      (fromMaybe (HM.singleton "main" (TaggedLog "ChangeLog.md" Nothing)) (changelogs cfg))

apiMain :: Paths -> Options -> Git -> IO ()
apiMain paths opts@Options{..} git = do
  case chLog paths of
    Nothing -> return ()
    Just cl -> do
      let tlp = taggedLogPath cl
      coloredPrint Green ("Checking " <> showPath tlp
                                      <> " and creating it if missing.\nIf no indicator exists it will be checked as global changelog.\n")
      touch $ tlp

      bump <- checkChangelogWrap opts git optNoCheck cl

      (when (bump && optBumpVersions) $ do
        newVersion <- case optApiLevel of
          Nothing -> generateLocalVersionByChangelog optNoCheck cl
          Just lev -> Just <$> generateLocalVersion lev (fromJustCustom "No file with current API version specified." (taggedLogIndicator cl))

        case newVersion of
          Nothing -> return ()
          Just version -> case HM.lookup "api" (defaultedEmpty (versioned paths)) of
            Just files -> do
              mapM_ (bumpPart version) files
              headChangelog version tlp
            Nothing -> coloredPrint Yellow "WARNING: no files to bump API version in specified.\n"
        ) `catch` (\(ex :: PatternMatchFail) -> coloredPrint Red (showText ex))
  where
    chLog cfg = do
      hm <- changelogs cfg
      HM.lookup "api" hm

otherMain :: Paths -> Options -> Git -> IO ()
otherMain paths opts@Options{..} git = do
  mapM_ act (entries (changelogs paths))
  where
    entries :: Maybe (HM.HashMap Text TaggedLog) -> [(Text, TaggedLog)]
    entries (Just a) = HM.toList $ HM.delete "main" $ HM.delete "api" a
    entries Nothing = []
    
    act (key, changelog) = do
      coloredPrint Green ("Checking " <> showPath (taggedLogPath changelog) <> " and creating it if missing.\n")
      touch (taggedLogPath changelog)
    
      bump <- checkChangelogWrap opts git optNoCheck changelog
    
      (when (bump && optBumpVersions) $ do
        newVersion <- generateLocalVersionByChangelog optNoCheck changelog
      
        case newVersion of
          Nothing -> return ()
          Just version -> case HM.lookup key (defaultedEmpty (versioned paths)) of
            Just files -> do
              mapM_ (bumpPart version) files
              headChangelog version (taggedLogPath changelog)
            Nothing -> coloredPrint Yellow "WARNING: no files to bump version in specified.\n"
        ) `catch` (\(ex :: PatternMatchFail) -> coloredPrint Red (showText ex))

defaultMain :: IO ()
defaultMain = do
  opts@Options{..} <- options welcome parser

  defaultPaths <- makeDefaultPaths

  paths <- fromMaybe defaultPaths <$> loadPaths

  git <- gitData optFromBC

  commonMain paths opts git

  apiMain paths opts git

  otherMain paths opts git
  
  sh $ rm $ gitHistory git