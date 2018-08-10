module Changelogged.Changelog.Compose where

import Prelude hiding (FilePath)
import Turtle

import qualified Control.Foldl as Fold

import Data.Maybe (isJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import System.Console.ANSI (Color(..))

import Changelogged.Common
import Changelogged.Git (listPRCommits, getCommitTag)
import Changelogged.Pattern (isMerge)

-- |
warnMissing :: Commit -> Appl ()
warnMissing Commit{..} = do
  case commitIsPR of
    Nothing -> do
      printf ("- Commit ")
      coloredPrint Cyan (getSHA1 commitSHA)
    Just (PR num) -> do
      printf ("- Pull request ")
      coloredPrint Cyan (num)
  printf (" is missing: "%s%".\n") commitMessage

-- |
-- >>> prLink (Link "https://github.com/GetShopTV/changelogged") (PR "#13")
-- Link {getLink = " https://github.com/GetShopTV/changelogged/pull/13 "}
prLink :: Link -> PR -> Link
prLink (Link link) (PR num) = Link $ " " <> link <> "/pull/" <> Text.drop 1 num <> " "

-- |
-- >>> commitLink (Link "https://github.com/GetShopTV/changelogged") (SHA1 "9e14840")
-- Link {getLink = " https://github.com/GetShopTV/changelogged/commit/9e14840 "}
commitLink :: Link -> SHA1 -> Link
commitLink (Link link) (SHA1 sha) = Link $ " " <> link <> "/commit/" <> sha <> " "

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

-- |Get commit message for any entry in history.
retrieveCommitMessage :: Maybe PR -> SHA1 -> Appl Text
retrieveCommitMessage isPR (SHA1 commit) = do
  summary <- fold (inproc "git" ["show", "-s", "--format=%B", commit] empty) Fold.list
  return $ Text.stripStart $ lineToText $ case isPR of
    Just _ -> summary !! 2
    Nothing -> summary !! 0

printCommitTag :: SHA1 -> Appl ()
printCommitTag sha = getCommitTag sha >>= \tag -> case tag of
  Nothing -> return ()
  Just t -> coloredPrint Yellow (t <> "\n")

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
