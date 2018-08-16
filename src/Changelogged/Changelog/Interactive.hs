{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Changelogged.Changelog.Interactive where

import           Prelude              hiding (FilePath)
import           Turtle               hiding (stdout)

import qualified Control.Foldl        as Fold

import           Data.Maybe           (fromMaybe, isJust)
import           Data.Text            (Text)
import qualified Data.Text            as Text

import           System.Console.ANSI  (Color (..))

import           Changelogged.Common
import           Changelogged.Config (addCommitToIgnored)
import           Changelogged.Changelog.Common
import           Changelogged.Git     (listPRCommits, showDiff, getCommitTag)
import           Changelogged.Pattern (isMerge)

-- $setup
-- >>> :set -XOverloadedStrings

-- FIXME: names and modules. interactive session with constant prompt is not interactive.
interactiveSession :: Appl Interaction -> Text -> Link -> FilePath -> [Commit] -> Appl ()
interactiveSession _ _ _ _ [] = return ()
interactiveSession prompt entryPrefix repoUrl changelog (current@Commit{..}:rest) = do
  suggestMissing entryPrefix repoUrl current
  action <- prompt
  Options{..} <- gets envOptions
  case action of
    Write -> do
      addMissing entryPrefix repoUrl current changelog
      interactiveSession prompt entryPrefix repoUrl changelog rest
    Expand -> do
      addMissing entryPrefix repoUrl current changelog
      if (isMerge commitMessage || isJust commitIsPR) 
        then do
          subChanges <- listPRCommits commitSHA
          interactiveSession prompt ("  " <> entryPrefix) repoUrl changelog subChanges
        else return ()
      interactiveSession prompt entryPrefix repoUrl changelog rest
    Skip -> interactiveSession prompt entryPrefix repoUrl changelog rest
    Remind -> showDiff commitSHA >> interactiveSession prompt "" repoUrl changelog (current:rest)
    IgnoreAlways -> do
      debug (showText changelog)
      addCommitToIgnored commitSHA changelog
      interactiveSession prompt entryPrefix repoUrl changelog rest
    Quit -> interactiveSession promptSkip "" repoUrl changelog rest
    WriteRest -> interactiveSession promptSimple "" repoUrl changelog (current:rest)

interactiveWalk :: Link -> FilePath -> [Commit] -> Appl ()
interactiveWalk = interactiveSession promptInteractive ""

simpleWalk :: Link -> FilePath -> [Commit] -> Appl ()
simpleWalk = interactiveSession promptSimple ""

-- |
suggestMissing :: Text -> Link -> Commit -> Appl ()
suggestMissing entryPrefix gitUrl Commit{..} = do
  (ChangeloggedEnv Options{..} Config{..}) <- get
  case commitIsPR of
    Just num ->
      printEntry (EntryFormat entryPrefix <> fromMaybe defaultEntryFormat configEntryFormat) commitMessage (prLink gitUrl num) (getPR num)
    Nothing ->
      printEntry (EntryFormat entryPrefix <> fromMaybe defaultEntryFormat configEntryFormat) commitMessage (commitLink gitUrl commitSHA) (getSHA1 commitSHA)

-- |Add generated suggestion directly to changelog.
addMissing :: Text -> Link -> Commit -> FilePath -> Appl ()
addMissing entryPrefix gitUrl Commit{..} changelog = do
  currentLogs <- fold (input changelog) Fold.list
  (ChangeloggedEnv Options{..} Config{..}) <- get
  unless optDryRun $ do
    output changelog (return $ unsafeTextToLine (entry configEntryFormat))
    tagm <- getCommitTag commitSHA
    case tagm of
      Nothing -> return ()
      Just tag -> append changelog (select (map unsafeTextToLine ["","#### " <> tag,""]))
    append changelog (select currentLogs)
  where
    entry formatting = case commitIsPR of
      Just num ->
        buildEntry (EntryFormat entryPrefix <> fromMaybe defaultEntryFormat formatting) commitMessage (prLink gitUrl num) (getPR num)
      Nothing ->
        buildEntry (EntryFormat entryPrefix <> fromMaybe defaultEntryFormat formatting) commitMessage (commitLink gitUrl commitSHA) (getSHA1 commitSHA)

buildEntry ::  EntryFormat -> Text -> Link -> Text -> Text
buildEntry (EntryFormat formattingString) message link identifier =
  Text.replace "%message%" message . Text.replace "%link%" (getLink link) . Text.replace "%id%" identifier $ formattingString

printEntry :: EntryFormat -> Text -> Link -> Text -> Appl ()
printEntry (EntryFormat formattingString) message link identifier = do
  let parts = Text.split (=='%') formattingString
      printParts ("message":xs) = printf (""%s%"") message >> printParts xs
      printParts ("link":xs) = coloredPrint Blue (getLink link) >> printParts xs
      printParts ("id":xs) = coloredPrint Cyan identifier >> printParts xs
      printParts (part:xs) = printf (""%s%"") part >> printParts xs
      printParts [] = printf "\n"
    in printParts parts

defaultEntryFormat :: EntryFormat
defaultEntryFormat = EntryFormat "- %message% (see [%id%](%link%));"
