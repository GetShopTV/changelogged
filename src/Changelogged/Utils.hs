module Changelogged.Utils where

import Data.Text (Text)
import Data.Monoid ((<>))

import System.Console.ANSI

import Turtle.Format

-- |Print '@text@' with ansi-terminal color.
coloredPrint :: Color -> Text -> IO ()
coloredPrint color line = do
  setSGR [SetColor Foreground Vivid color]
  printf s line
  setSGR [Reset]

warning :: Text -> IO ()
warning msg = coloredPrint Yellow $
  "WARNING: " <> msg <> "\n"

failure :: Text -> IO ()
failure msg = coloredPrint Red $
  "FAILURE: " <> msg <> "\n"

info :: Text -> IO ()
info msg = coloredPrint Cyan $
  "INFO: " <> msg <> "\n"

versionP :: Text -> IO ()
versionP ver = coloredPrint Green $
  "VERSION: " <> ver <> "\n"
