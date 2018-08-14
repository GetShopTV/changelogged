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
import           Changelogged.Changelog.Common
import           Changelogged.Git     (listPRCommits)
import           Changelogged.Pattern (isMerge)

-- $setup
-- >>> :set -XOverloadedStrings

interactiveSession :: Link -> Commit -> FilePath -> Appl Bool
interactiveSession _repoUrl _commit@Commit{..} _changelog = return True

interactiveDealWithEntry :: Link -> Commit -> FilePath -> Appl Bool
interactiveDealWithEntry repoUrl commit@Commit{..} changelog =
  actOnMissingCommit commit changelog (interactiveSession repoUrl commit changelog)

suggestSubchanges :: Link -> SHA1 -> Appl ()
suggestSubchanges gitUrl mergeHash = do
  entryFormat <- asks (configEntryFormat . envConfig)
  subChanges <- listPRCommits mergeHash
  mapM_ (suggest entryFormat) subChanges
  where
    suggest formatting (sha1, message) = printf "  " >> printEntry (fromMaybe defaultEntryFormat formatting) message (commitLink gitUrl sha1) (getSHA1 sha1)

-- |
suggestMissing :: Link -> Commit -> Appl ()
suggestMissing gitUrl Commit{..} = do
  (ChangeloggedEnv Options{..} Config{..}) <- ask
  case commitIsPR of
    Just num -> do
      printEntry (fromMaybe defaultEntryFormat configEntryFormat) commitMessage (prLink gitUrl num) (getPR num)
      when optExpandPR $ suggestSubchanges gitUrl commitSHA
    Nothing -> do
      printEntry (fromMaybe defaultEntryFormat configEntryFormat) commitMessage (commitLink gitUrl commitSHA) (getSHA1 commitSHA)
      when (isMerge commitMessage && optExpandPR) $ do
        suggestSubchanges gitUrl commitSHA
        debug commitMessage

addSubchanges :: Link -> SHA1 -> FilePath -> Appl ()
addSubchanges gitUrl mergeHash changelog = do
  subChanges <- listPRCommits mergeHash
  add subChanges
  where
    add :: [(SHA1, Text)] -> Appl ()
    add changes = do
      entryFormat <- asks (configEntryFormat . envConfig)
      append changelog (select . (map unsafeTextToLine) $ map (buildSubEntry entryFormat) changes)
    buildSubEntry formatting (sha1, message) = "  " <> buildEntry (fromMaybe defaultEntryFormat formatting) message (commitLink gitUrl sha1) (getSHA1 sha1)

-- |Add generated suggestion directly to changelog.
addMissing :: Link -> Commit -> FilePath -> Appl ()
addMissing gitUrl Commit{..} changelog = do
  currentLogs <- fold (input changelog) Fold.list
  (ChangeloggedEnv Options{..} Config{..}) <- ask
  unless optDryRun $ do
    output changelog (return $ unsafeTextToLine (entry configEntryFormat))
    when ((isJust commitIsPR || isMerge commitMessage) && optExpandPR) $ addSubchanges gitUrl commitSHA changelog
    append changelog (select currentLogs)
  where
    entry formatting = case commitIsPR of
      Just num -> buildEntry (fromMaybe defaultEntryFormat formatting) commitMessage (prLink gitUrl num) (getPR num)
      Nothing -> buildEntry (fromMaybe defaultEntryFormat formatting) commitMessage (commitLink gitUrl commitSHA) (getSHA1 commitSHA)

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
