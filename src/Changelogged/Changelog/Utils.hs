module Changelogged.Changelog.Utils where

import Prelude hiding (FilePath)
import Turtle

import qualified Control.Foldl as Fold

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text

import System.Console.ANSI (Color(..))

import Changelogged.Git (listPRCommits)
import Changelogged.Types
import Changelogged.Options
import Changelogged.Pattern (isMerge)
import Changelogged.Utils

-- |Check if commit/pr is present in changelog. Return '@True@' if present.
changelogIsUp :: Link -> Commit -> FilePath -> Appl Bool
changelogIsUp repoUrl commit@Commit{..} changelog = do
  Options{..} <- ask
  noEntry <- case commitIsPR of
    Nothing -> fold (grep (has (text (getSHA1 commitSHA))) (input changelog)) Fold.null
    Just (PR num) -> fold (grep (has (text num)) (input changelog)) Fold.null
  if noEntry
    then do
      case optFormat of
        WarnSimple  -> warnMissing commit
        WarnSuggest -> suggestMissing repoUrl commit
      when (optAction == Just UpdateChangelogs) $ addMissing repoUrl commit changelog
      return False
    else return True

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
-- "https://github.com/GetShopTV/changelogged/pull/13"
prLink :: Link -> PR -> Text
prLink (Link link) (PR num) = link <> "/pull/" <> Text.drop 1 num

-- |
-- >>> commitLink (Link "https://github.com/GetShopTV/changelogged") (SHA1 "9e14840")
-- "https://github.com/GetShopTV/changelogged/commit/9e14840"
commitLink :: Link -> SHA1 -> Text
commitLink (Link link) (SHA1 sha) = link <> "/commit/" <> sha

suggestSubchanges :: Link -> SHA1 -> Appl ()
suggestSubchanges gitUrl mergeHash = do
  subChanges <- listPRCommits mergeHash
  mapM_ suggest subChanges
  where
    suggest (sha1, message) = do
      printf ("  - "%s%" (see ") message
      coloredPrint Cyan ("[`" <> getSHA1 sha1 <> "`]")
      coloredPrint Blue $ "( " <> commitLink gitUrl sha1 <> " )"
      printf ");\n"

-- |
suggestMissing :: Link -> Commit -> Appl ()
suggestMissing gitUrl Commit{..} = do
  Options{..} <- ask
  printf ("- "%s%" (see ") commitMessage
  case commitIsPR of
    Just num -> do
      coloredPrint Cyan $ "[" <> getPR num <> "]"
      coloredPrint Blue $ "( " <> prLink gitUrl num <> " )"
      printf ");\n"
      when optExpandPR $ suggestSubchanges gitUrl commitSHA
    Nothing -> do
      coloredPrint Cyan ("[`" <> getSHA1 commitSHA <> "`]")
      coloredPrint Blue $ "( " <> commitLink gitUrl commitSHA <> " )"
      printf ");\n"
      when (isMerge commitMessage && optExpandPR) $ do
        suggestSubchanges gitUrl commitSHA
        debug commitMessage

addSubchanges :: Link -> SHA1 -> FilePath -> Appl ()
addSubchanges gitUrl mergeHash changelog = do
  subChanges <- listPRCommits mergeHash
  add subChanges
  where
    add :: [(SHA1, Text)] -> Appl ()
    add changes = append changelog (select . (map unsafeTextToLine) $ map buildEntry changes)
    buildEntry (sha1, message) = prolog message <> sense sha1  <> ");"
    prolog msg = "  - " <> msg <> " (see "
    sense sha = "[`" <> getSHA1 sha <> "`]" <> "( " <> commitLink gitUrl sha <> " )"

-- |Add generated suggestion directly to changelog.
addMissing :: Link -> Commit -> FilePath -> Appl ()
addMissing gitUrl Commit{..} changelog = do
  currentLogs <- fold (input changelog) Fold.list
  Options{..} <- ask
  unless optDryRun $ do
    output changelog (return $ unsafeTextToLine entry)
    when ((isJust commitIsPR || isMerge commitMessage) && optExpandPR) $ addSubchanges gitUrl commitSHA changelog
    append changelog (select currentLogs)
  where
    entry = prolog <> sense <> epilog
    prolog = "- " <> commitMessage <> " (see "
    sense = case commitIsPR of
        Just num -> "[" <> getPR num <> "]" <> "( " <> prLink gitUrl num <> " )"
        Nothing -> "[`" <> getSHA1 commitSHA <> "`]" <> "( " <> commitLink gitUrl commitSHA <> " )"
    epilog = ");"

-- |Get commit message for any entry in history.
retrieveCommitMessage :: Maybe PR -> SHA1 -> Appl Text
retrieveCommitMessage isPR (SHA1 commit) = do
  summary <- fold (inproc "git" ["show", commit] empty) Fold.list
  return $ Text.stripStart $ lineToText $ case isPR of
    Just _ -> summary !! 7
    Nothing -> summary !! 4
