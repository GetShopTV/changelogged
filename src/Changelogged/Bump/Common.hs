module Changelogged.Bump.Common where

import Prelude hiding (FilePath)
import Turtle

import Control.Exception
import qualified Control.Foldl as Fold

import Data.Functor (($>))
import Data.Text (Text)

import Filesystem.Path.CurrentOS (encodeString)

import Changelogged.Config
import Changelogged.Types
import Changelogged.Options
import Changelogged.Pattern

-- |Add version label to changelog.
headChangelog :: Text -> FilePath -> Appl ()
headChangelog version changelog = asks optDryRun >>= (\dry -> unless dry $ do
  currentLogs <- fold (input changelog) Fold.list
  output changelog (return $ unsafeTextToLine version)
  append changelog "---"
  append changelog ""
  append changelog (select currentLogs))

-- |Bump version in any supported file.
-- Unlike sed it reads all the file and is less memory efficient.
bumpAny :: VersionFile -> Text -> Shell ()
bumpAny VersionFile{..} version = do
  file <- fold (input versionFilePath) Fold.list
  matched <- fold (grep (has (text versionFileVersionPattern)) (select file)) Fold.list
  when (null matched) $
    throw (PatternMatchFail ("ERROR: Cannot bump. Cannot detect version in file " <> encodeString versionFilePath <> ". Check config.\n"))
  changed <- fold (sed (versionExactRegex $> version) (select matched)) Fold.list
  output versionFilePath (select $ generateVersionedFile file changed matched)

-- |Replace given lines in the file.
-- Here is used and called to write new lines with versions.
generateVersionedFile
  -- template file
  :: [Line]
  -- new lines
  -> [Line]
  -- lines to be replaced
  -> [Line]
  --result
  -> [Line]
generateVersionedFile file [] [] = file
generateVersionedFile _ [] _ = error "internal sed error"
generateVersionedFile _ _ [] = error "internal sed error"
generateVersionedFile file (new:news) (old:olds) = generateVersionedFile (replaceLine file new old) news olds
  where
    replaceLine [] _ _ = []
    replaceLine (xvar:xvars) newLine oldLine 
      | xvar == oldLine = newLine:xvars
      | otherwise = xvar : replaceLine xvars newLine oldLine

-- |Bump version in file regarding extension.
bumpPart :: Text -> VersionFile -> Appl ()
bumpPart version file@VersionFile{..} = do
  printf ("- Updating version for "%fp%"\n") versionFilePath
  dryRun <- asks optDryRun
  unless dryRun $ sh $ bumpAny file version

--TODO!
-- |Get level of changes from changelog.
getChangelogEntries :: FilePath -> LevelHeaders -> Appl (Maybe Level)
getChangelogEntries changelogFile levelHeaders = do
  liftIO $ print levelHeaders
  app <- fold (grep (prefix "* App") unreleased) countLines
  major <- fold (grep (prefix "* Major") unreleased) countLines
  minor <- fold (grep (prefix "* Minor") unreleased) countLines
  fixes <- fold (grep (prefix "* Fix") unreleased) countLines
  docs  <- fold (grep (prefix "* Doc") unreleased) countLines

  return $ case app of
    0 -> case major of
      0 -> case minor of
        0 -> case fixes of
          0 -> case docs of
            0 -> Nothing
            _ -> Just Doc
          _ -> Just Fix
        _ -> Just Minor
      _ -> Just Major
    _ -> Just App
  where
    unreleased = limitWhile (null . match (prefix versionExactRegex) . lineToText) (input changelogFile)
