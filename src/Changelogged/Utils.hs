module Changelogged.Utils where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid ((<>))

import System.Console.ANSI

import Turtle.Format

import Changelogged.Options

-- |Print '@text@' with ansi-terminal color.
coloredPrint :: Color -> Text -> Appl ()
coloredPrint color line = do
  noColor <- asks optNoColors
  if noColor
    then printf s line
    else do
      liftIO $ setSGR [SetColor Foreground Vivid color]
      printf s line
      liftIO $ setSGR [Reset]

warning :: Text -> Appl ()
warning msg = coloredPrint Yellow $
  "WARNING: " <> msg <> "\n"

failure :: Text -> Appl ()
failure msg = coloredPrint Red $
  "FAILURE: " <> msg <> "\n"

info :: Text -> Appl ()
info msg = coloredPrint Cyan $
  "INFO: " <> msg <> "\n"

debug :: Text -> Appl ()
debug msg = coloredPrint Magenta $
  "DEBUG: " <> msg <> "\n"

debugShow :: Show a => a -> Appl ()
debugShow = debug . Text.pack . show

versionP :: Text -> Appl ()
versionP ver = coloredPrint Green $
  "VERSION: " <> ver <> "\n"
