module Bump.Common where

import Prelude hiding (FilePath)
import Turtle

import Control.Exception
import qualified Control.Foldl as Fold

import Data.Text (Text)

import Filesystem.Path.CurrentOS (encodeString)

import Types
import Pattern

-- |Add version label to changelog.
headChangelog :: Text -> FilePath -> IO ()
headChangelog version changelog = do
  currentLogs <- fold (input changelog) Fold.list
  output changelog (return $ unsafeTextToLine version)
  append changelog "---"
  append changelog ""
  append changelog (select currentLogs)

-- |Bump version in any supported file.
-- Unlike sed it reads all the file and is less memory efficient.
bumpAny :: (Text -> Pattern Text) -> TaggedFile -> Text -> Shell ()
bumpAny extGrep TaggedFile{..} version = do
  file <- fold (input taggedFilePath) Fold.list
  matched <- fold (grep (extGrep taggedFileVariable) (select file)) Fold.list
  when (matched == []) $
    throw (PatternMatchFail ("ERROR: Cannot bump. Cannot detect version in file " <> encodeString taggedFilePath <> ". Check config.\n"))
  changed <- fold (sed (versionExactRegex *> return version) (select matched)) Fold.list
  output taggedFilePath (select $ generateVersionedFile file changed matched)

-- |Replace given lines in the file.
-- Here is used and called to write new lines wih versions.
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
      | xvar == oldLine = (newLine:xvars)
      | otherwise = xvar : replaceLine xvars newLine oldLine

-- |Bump version in file regarding extension.
bumpPart :: Text -> TaggedFile -> IO ()
bumpPart version file@TaggedFile{..} = do
  printf ("- Updating version for "%fp%"\n") taggedFilePath
  case extension taggedFilePath of
    Just "hs" -> sh $ bumpAny hsGrep file version
    Just "json" -> sh $ bumpAny jsonGrep file version
    Just "yaml" -> sh $ bumpAny yamlGrep file version
    Just "cabal" -> sh $ bumpAny cabalGrep file version
    _ -> throw (PatternMatchFail ("ERROR: Cannot bump version. Unsupported extension in file " <> encodeString taggedFilePath <> ". Check config."))

-- |Get level of changes from changelog.
getChangelogEntries :: FilePath -> IO (Maybe Level)
getChangelogEntries changelogFile = do
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
    unreleased = limitWhile (\line -> match (prefix versionExactRegex) (lineToText line) == []) (input changelogFile)
