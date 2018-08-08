module Changelogged.Changelog.Utils where

import Prelude hiding (FilePath)
import Turtle

import qualified Control.Foldl as Fold

import Data.Text (Text)
import qualified Data.Text as Text

import System.Console.ANSI (Color(..))

import Changelogged.Git (listPRCommits, GitInfo(..))
import Changelogged.Types
import Changelogged.Options
import Changelogged.Utils
import Changelogged.Pure

-- |Check if commit/pr is present in changelog. Return '@True@' if present.
changelogIsUp :: GitInfo -> Text -> Mode -> Text -> FilePath -> Appl Bool
changelogIsUp gitInfo item mode message changelog = do
  Options{..} <- ask
  noEntry <- fold (grep (has (text item)) (input changelog)) Fold.null
  if noEntry
    then do
      case optFormat of
        WarnSimple  -> warnMissing item mode message
        WarnSuggest -> suggestMissing gitInfo item mode message
      when (optAction == Just UpdateChangelogs) $ addMissing gitInfo item mode message changelog
      return False
    else return True

-- |
warnMissing :: Text -> Mode -> Text -> Appl ()
warnMissing item mode message = do
  printf ("- "%s%" ") (showText mode)
  coloredPrint Cyan item
  printf (" is missing: "%s%".\n") message

-- |
-- >>> prLink "https://github.com/GetShopTV/changelogged" "#13"
-- "https://github.com/GetShopTV/changelogged/pull/13"
prLink :: Text -> Text -> Text
prLink link num = link <> "/pull/" <> Text.drop 1 num

-- |
-- >>> commitLink "https://github.com/GetShopTV/changelogged" "9e14840"
-- "https://github.com/GetShopTV/changelogged/commit/9e14840"
commitLink :: Text -> Text -> Text
commitLink link sha = link <> "/commit/" <> sha

suggestSubchanges :: GitInfo -> Text -> Appl ()
suggestSubchanges GitInfo{..} item = do
  subChanges <- listPRCommits gitHistory item
  mapM_ suggest subChanges
  where
    suggest (sha1, message) = do
      printf ("  - "%s%" (see ") message
      coloredPrint Cyan ("[`" <> sha1 <> "`]")
      coloredPrint Blue $ "( " <> commitLink gitRemoteUrl sha1 <> " );\n"

-- |
suggestMissing :: GitInfo -> Text -> Mode -> Text -> Appl ()
suggestMissing gitInfo@GitInfo{..} item mode message = do
  printf ("- "%s%" (see ") message
  case mode of
    PR -> do
      coloredPrint Cyan $ "[" <> item <> "]"
      coloredPrint Blue $ "( " <> prLink gitRemoteUrl item <> " )"
      suggestSubchanges gitInfo item
    Commit -> do
      coloredPrint Cyan ("[`" <> item <> "`]")
      coloredPrint Blue $ "( " <> commitLink gitRemoteUrl item <> " )"
  printf ");\n"

addSubchanges :: GitInfo -> Text -> FilePath -> Appl ()
addSubchanges GitInfo{..} item changelog = do
  subChanges <- listPRCommits gitHistory item
  add subChanges
  where
    add :: [(Text, Text)] -> Appl ()
    add changes = append changelog (select . (map unsafeTextToLine) $ map buildEntry changes)
    buildEntry (sha1, message) = prolog message <> sense sha1  <> ");"
    prolog msg = "  - " <> msg <> " (see "
    sense sha = "[`" <> sha <> "`]" <> "( " <> commitLink gitRemoteUrl sha <> " )"

-- |Add generated suggestion directly to changelog.
addMissing :: GitInfo -> Text -> Mode -> Text -> FilePath -> Appl ()
addMissing gitInfo@GitInfo{..} item mode message changelog = do
  currentLogs <- fold (input changelog) Fold.list
  Options{..} <- ask
  unless optDryRun $ do
    output changelog (return $ unsafeTextToLine entry)
    when (mode == PR) $ unless optNoExpandPR $ addSubchanges gitInfo item changelog
    append changelog (select currentLogs)
  where
    entry = prolog <> sense <> epilog
    prolog = "- " <> message <> " (see "
    sense = case mode of
        PR -> "[" <> item <> "]" <> "( " <> prLink gitRemoteUrl item <> " )"
        Commit -> "[`" <> item <> "`]" <> "( " <> commitLink gitRemoteUrl item <> " )"
    epilog = ");"

-- |Get commit message for any entry in history.
commitMessage :: Mode -> Text -> Appl Text
commitMessage _ "" = return ""
commitMessage mode commit = do
  summary <- fold (inproc "git" ["show", commit] empty) Fold.list
  return $ Text.stripStart $ lineToText $ case mode of
    PR -> summary !! 7
    Commit -> summary !! 4
