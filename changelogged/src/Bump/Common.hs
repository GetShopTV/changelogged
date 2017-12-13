module Bump.Common where

import Prelude hiding (FilePath)
import Turtle

import Data.Text (Text)

import Types
import Pure

-- |Bump version in '.hs' file
bumpHS :: TaggedFile -> Text -> IO ()
bumpHS TaggedFile{..} version = do
  sh $ inproc "sed" ["-i", "-r", hsExpr, showPath taggedFilePath] empty
  return ()
  where
    hsExpr = "s/(^" <> taggedFileVariable <> " = )\\\"[0-9][0-9.]*\\\"/\\1\"" <> version <> "\"/"    

-- |Bump version in '.json' file
bumpJSON :: TaggedFile -> Text -> IO ()
bumpJSON TaggedFile{..} version = do
  sh $ inproc "sed" ["-i", "-r", jsonExpr, showPath taggedFilePath] empty
  return ()
  where
    jsonExpr = "s/(^\\s*\"" <> taggedFileVariable <> "\": )\"[0-9][0-9.]*\"/\\1\"" <> version <> "\"/"

-- |Bump version in '.cabal' file
bumpCabal :: TaggedFile -> Text -> IO ()
bumpCabal TaggedFile{..} version = do
  sh $ inproc "sed" ["-i", "-r", cabalExpr, showPath taggedFilePath] empty
  return ()
  where
    cabalExpr = "s/(^" <> taggedFileVariable <> ":[^0-9]*)[0-9][0-9.]*/\\1" <> version <> "/"

-- |Get level of changes from changelog.
getChangelogEntries :: FilePath -> IO (Maybe Level)
getChangelogEntries changelogFile = do
  major <- fold (grep (prefix "* Major") unreleased) countLines
  minor <- fold (grep (prefix "* Minor") unreleased) countLines
  fixes <- fold (grep (prefix "* Fix") unreleased) countLines
  docs  <- fold (grep (prefix "* Doc") unreleased) countLines

  return $ case major of
    0 -> case minor of
      0 -> case fixes of
        0 -> case docs of
          0 -> Nothing
          _ -> Just Doc
        _ -> Just Fix
      _ -> Just Minor
    _ -> Just Major
  where
    expr =  "/^[0-9]\\.[0-9]/q"
    -- correct would be `inproc "sed" [expr] (input changelogFile)`
    -- I'm getting broken pipe possibly related to https://github.com/Gabriel439/Haskell-Turtle-Library/issues/102
    unreleased = inproc "sed" [expr, showPath changelogFile] empty
