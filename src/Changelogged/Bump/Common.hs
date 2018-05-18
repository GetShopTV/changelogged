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

-- |Get level of changes from changelog.
getLevelOfChanges :: FilePath -> LevelHeaders -> Appl (Maybe Level)
getLevelOfChanges changelogFile levelHeaders@LevelHeaders{..} = do
  levels <- lookupLevels unreleased levelHeaders
  return $ firstPresent levels
  where
    unreleased = limitWhile (null . match (prefix versionExactRegex) . lineToText) (input changelogFile)

    lookupLevel linesList levelHeader = case levelHeader of
      Just txt -> fold (grep (prefix (text txt)) linesList) countLines >>= return . (/= 0)
      Nothing -> return False
    -- Maybe it's better to resolve it with instance Ord Level
    lookupLevels linesList (LevelHeaders h1 h2 h3 h4 h5) =
      mapM (lookupLevel linesList) [h1,h2,h3,h4,h5] >>= return . (zip [App, Major, Minor, Fix, Doc])

    firstPresent [] = Nothing 
    firstPresent ((level,isPresent):rest) = if isPresent
      then Just level
      else firstPresent rest
