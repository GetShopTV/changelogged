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

promptSkip :: Appl Interaction
promptSkip = return Skip

promptSimple :: Appl Interaction
promptSimple = return Write

promptInteractive :: Appl Interaction
promptInteractive = go
  where go = do
          coloredPrint Cyan "(↵/(s)kip/(e)xpand/(r)emind/(i)gnore/(q)uit/(a)ll):  \n"
          answer <- liftIO getLine
          case answer of
            "" -> return Write
            "w" -> return Write
            "W" -> return Write
            "Write" -> return Write
            "write" -> return Write
            "s" -> return Skip
            "skip" -> return Skip
            "Skip" -> return Skip
            "S" -> return Skip
            "e" -> return Expand
            "E" -> return Expand
            "Expand" -> return Expand
            "expand" -> return Expand
            "r" -> return Remind
            "remind" -> return Remind
            "Remind" -> return Remind
            "R" -> return Remind
            "i" -> return IgnoreAlways
            "ignore" -> return IgnoreAlways
            "Ignore" -> return IgnoreAlways
            "I" -> return IgnoreAlways
            "q" -> return Quit
            "quit" -> return Quit
            "a" -> return WriteRest
            "all" -> return WriteRest
            _ -> do
              liftIO $ putStrLn "Cannot parse action. Please repeat."
              go

interactiveSession :: Appl Interaction -> Text -> Link -> Commit -> FilePath -> Appl ()
interactiveSession prompt entryPrefix repoUrl commit@Commit{..} changelog = do
  suggestMissing entryPrefix repoUrl commit
  action <- prompt
  Options{..} <- gets envOptions
  case action of
    Write -> addMissing entryPrefix repoUrl commit changelog
    Expand -> do
      addMissing entryPrefix repoUrl commit changelog
      if (isMerge commitMessage || isJust commitIsPR) 
        then do
          subChanges <- listPRCommits commitSHA
          mapM_ (\sha -> interactiveSession prompt ("  " <> entryPrefix) repoUrl sha changelog) subChanges
        else return ()
    Skip -> return ()
    Remind -> showDiff commitSHA >> interactiveSession prompt "" repoUrl commit changelog
    IgnoreAlways -> debug (showText changelog) >> addCommitToIgnored commitSHA changelog
    Quit -> interactiveSession promptSkip "" repoUrl commit changelog
    WriteRest -> interactiveSession promptSimple "" repoUrl commit changelog

interactiveDealWithEntry :: Link -> Commit -> FilePath -> Appl Bool
interactiveDealWithEntry repoUrl commit@Commit{..} changelog =
  actOnMissingCommit commit changelog (interactiveSession promptInteractive "" repoUrl commit changelog >> return True)

simpleDealWithEntry :: Link -> Commit -> FilePath -> Appl Bool
simpleDealWithEntry repoUrl commit@Commit{..} changelog =
  actOnMissingCommit commit changelog (interactiveSession promptSimple "" repoUrl commit changelog >> return True)

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
