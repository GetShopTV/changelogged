module Bump.Common where

import Turtle

import qualified Control.Foldl as Fold

import Data.Text (Text)

bumpHS :: Text -> Text -> Text -> IO ()
bumpHS file version var = do
  _ <- strict $ inproc "sed" ["-i", "-r", hsExpr, file] empty
  return ()
  where
    hsExpr = "s/(^" <> var <> " = )\\\"[0-9][0-9.]*\\\"/\\1\"" <> version <> "\"/"
    
bumpJSON :: Text -> Text -> Text -> IO ()
bumpJSON file version var = do
  _ <- strict $ inproc "sed" ["-i", "-r", jsonExpr, file] empty
  return ()
  where
    jsonExpr = "s/(^\\s*\"" <> var <> "\": )\"[0-9][0-9.]*\"/\\1\"" <> version <> "\"/"

getChangelogEntries :: Text -> IO (Int, Int, Int, Int)
getChangelogEntries changelogFile = do
  major <- fold (inproc "grep" ["Major changes"] unreleased) Fold.length
  minor <- fold (inproc "grep" ["Minor changes"] unreleased) Fold.length
  fixes <- fold (inproc "grep" ["Fixes"] unreleased) Fold.length
  docs  <- fold (inproc "grep" ["Docs"] unreleased) Fold.length
  return (major, minor, fixes, docs)
  where
    expr =  "/^[0-9]\\.[0-9]/q"
    unreleased = inproc "sed" [expr, changelogFile] empty
