{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Changelogged.Versions.Utils where

import           Prelude                   hiding (FilePath)
import           Turtle

import           Control.Exception
import qualified Control.Foldl             as Fold

import           Data.Functor              (($>))

import           Filesystem.Path.CurrentOS (encodeString)

import           Changelogged.Common
import           Changelogged.Pattern

-- |Add version label to changelog.
headChangelog :: Version -> FilePath -> Appl ()
headChangelog (Version version) changelog = gets (optDryRun . envOptions) >>= (\dry -> unless dry $ do
  currentLogs <- fold (input changelog) Fold.list
  output changelog (return $ unsafeTextToLine version)
  append changelog "---"
  append changelog ""
  append changelog (select currentLogs))

-- |Bump version in any supported file.
-- Unlike sed it reads all the file and is less memory efficient.
bumpAny :: VersionFile -> Version -> Shell ()
bumpAny VersionFile{..} (Version version) = do
  file <- fold (input versionFilePath) Fold.list
  matched <- fold (grep (has pattern) (select file)) Fold.list
  when (null matched) $
    throw (PatternMatchFail ("ERROR: Cannot bump. Cannot detect version in file " <> encodeString versionFilePath <> ". Check config.\n"))
  changed <- fold (sed (versionExactRegex $> version) (select matched)) Fold.list
  output versionFilePath (select $ generateVersionedFile file changed matched)
  where
    pattern = text (versionPatternVariable versionFileVersionPattern) <> spaces <> text (versionPatternSeparator versionFileVersionPattern)

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
-- FIXME: impossible cases.
generateVersionedFile _ [] _ = error "internal sed error"
generateVersionedFile _ _ [] = error "internal sed error"
generateVersionedFile file (new:news) (old:olds) = generateVersionedFile (replaceLine file new old) news olds
  where
    replaceLine [] _ _ = []
    replaceLine (xvar:xvars) newLine oldLine
      | xvar == oldLine = newLine:xvars
      | otherwise = xvar : replaceLine xvars newLine oldLine

-- |Bump version in file regarding extension.
bumpPart :: Version -> VersionFile -> Appl ()
bumpPart version file@VersionFile{..} = do
  printf ("- Updating version for "%fp%"\n") versionFilePath
  dryRun <- gets (optDryRun . envOptions)
  unless dryRun $ sh $ bumpAny file version

-- |Get level of changes from changelog.
-- There once could be set of heuristics to determine new version.
-- See original issue - https://github.com/GetShopTV/changelogged/issues/87
predictLevelOfChanges :: FilePath -> Appl (Maybe Level)
predictLevelOfChanges _changelogFile = return Nothing
