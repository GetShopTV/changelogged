module Bump.Common where

import Prelude hiding (FilePath)
import Turtle

import Data.Text (Text)

import Types

bumpHS :: FilePath -> Text -> Text -> IO ()
bumpHS file version var = do
  sh $ inproc "sed" ["-i", "-r", hsExpr] (input file)
  return ()
  where
    hsExpr = "s/(^" <> var <> " = )\\\"[0-9][0-9.]*\\\"/\\1\"" <> version <> "\"/"
    
bumpJSON :: FilePath -> Text -> Text -> IO ()
bumpJSON file version var = do
  sh $ inproc "sed" ["-i", "-r", jsonExpr] (input file)
  return ()
  where
    jsonExpr = "s/(^\\s*\"" <> var <> "\": )\"[0-9][0-9.]*\"/\\1\"" <> version <> "\"/"

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
