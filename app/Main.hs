{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding (FilePath)
import Turtle

import Control.Exception
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)

import System.Console.ANSI (Color(..))

import CheckLog.Check
import Bump.Local
import Bump.Common
import Bump.General
import Types
import Git
import Options
import Utils
import Pure (showPath, fromJustCustom, defaultedEmpty, showText)
import Settings

commonMain :: Paths -> Options -> Git -> IO ()
commonMain paths opts@Options{..} git = do
  coloredPrint Green ("Checking " <> showPath (taggedLogPath $ chLog paths) <> " and creating it if missing.\n")
  touch $ taggedLogPath (chLog paths)

  bump <- checkChangelogWrap opts git optNoCheck (chLog paths)

  (when (bump && not optNoBump) $ do
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
  coloredPrint Green ("Checking " <> showPath (taggedLogPath $ chLog paths)
                                  <> " and creating it if missing.\nIf no indicator exists it will be checked as global changelog.\n")
  touch $ taggedLogPath (chLog paths)

  bump <- checkChangelogWrap opts git optNoCheck (chLog paths)

  (when (bump && not optNoBump) $ do
    newVersion <- case optApiLevel of
      Nothing -> generateLocalVersionByChangelog optNoCheck (chLog paths)
      Just lev -> Just <$> generateLocalVersion lev (fromJustCustom "No file with current API version specified." (taggedLogIndicator (chLog paths)))
  
    case newVersion of
      Nothing -> return ()
      Just version -> case HM.lookup "api" (defaultedEmpty (versioned paths)) of
        Just files -> do
          mapM_ (bumpPart version) files
          headChangelog version (taggedLogPath $ chLog paths)
        Nothing -> coloredPrint Yellow "WARNING: no files to bump API version in specified.\n"
    ) `catch` (\(ex :: PatternMatchFail) -> coloredPrint Red (showText ex))
  where
    chLog cfg = HM.lookupDefault (TaggedLog "ApiChangeLog.md" Nothing) "api"
      (fromMaybe (HM.singleton "api" (TaggedLog "ApiChangeLog.md" Nothing)) (changelogs cfg))

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
    
      (when (bump && not optNoBump) $ do
        newVersion <- generateLocalVersionByChangelog optNoCheck changelog
      
        case newVersion of
          Nothing -> return ()
          Just version -> case HM.lookup key (defaultedEmpty (versioned paths)) of
            Just files -> do
              mapM_ (bumpPart version) files
              headChangelog version (taggedLogPath changelog)
            Nothing -> coloredPrint Yellow "WARNING: no files to bump version in specified.\n"
        ) `catch` (\(ex :: PatternMatchFail) -> coloredPrint Red (showText ex))

main :: IO ()
main = do
  opts@Options{..} <- options welcome parser

  defaultPaths <- makeDefaultPaths

  paths <- fromMaybe defaultPaths <$> loadPaths

  git <- gitData optFromBC

  commonMain paths opts git

  when optWithAPI $ apiMain paths opts git

  when optDifferentChlogs $ otherMain paths opts git
  
  sh $ rm $ gitHistory git
