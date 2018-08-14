{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Changelogged.Changelog.Interactive where

import           Prelude              hiding (FilePath)
import           Turtle

import qualified Control.Foldl        as Fold

import           Data.Maybe           (fromMaybe, isJust)
import           Data.Text            (Text)
import qualified Data.Text            as Text

import           System.Console.ANSI  (Color (..))

import           Changelogged.Common
import           Changelogged.Config (addCommitMessageToIgnored)
import           Changelogged.Changelog.Common
import           Changelogged.Git     (listPRCommits)
import           Changelogged.Pattern (isMerge)

-- $setup
-- >>> :set -XOverloadedStrings

prompt :: Appl Interaction
prompt = return IgnoreAlways

interactiveSession :: Text -> Link -> Commit -> FilePath -> Appl ()
interactiveSession entryPrefix repoUrl commit@Commit{..} changelog = do
  suggestMissing entryPrefix repoUrl commit
  action <- prompt
  Options{..} <- gets envOptions
  case action of
    Expand -> do
      addMissing entryPrefix repoUrl commit changelog
      if (isMerge commitMessage || isJust commitIsPR) 
        then do
          subChanges <- listPRCommits commitSHA
          mapM_ (\sha -> interactiveSession ("  " <> entryPrefix) repoUrl sha changelog) subChanges
        else return ()
    Skip -> return ()
    Remind -> interactiveSession "" repoUrl commit changelog
    IgnoreAlways -> debug (showText changelog) >> addCommitMessageToIgnored commitMessage changelog

interactiveDealWithEntry :: Link -> Commit -> FilePath -> Appl Bool
interactiveDealWithEntry repoUrl commit@Commit{..} changelog =
  actOnMissingCommit commit changelog (interactiveSession "" repoUrl commit changelog >> return True)

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
defaultEntryFormat = EntryFormat "- %message% (see [%link%](%id%));"
