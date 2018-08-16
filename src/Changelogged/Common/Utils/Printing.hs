{-# LANGUAGE OverloadedStrings #-}
module Changelogged.Common.Utils.Printing where

import Control.Monad (when)

import Data.Aeson (ToJSON)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Yaml as Yaml

import           System.Console.ANSI

import Changelogged.Common.Types

import Turtle.Format

coloredPrintIO :: Bool -> Color -> Text -> IO ()
coloredPrintIO noColor color line = if noColor
    then printf s line
    else do
      setSGR [SetColor Foreground Vivid color]
      printf s line
      setSGR [Reset]

-- |Print '@text@' with ansi-terminal color.
coloredPrint :: Color -> Text -> Appl ()
coloredPrint color line = do
  noColor <- gets (optNoColors . envOptions)
  liftIO $ coloredPrintIO noColor color line

success :: Text -> Appl ()
success msg = coloredPrint Green $
  "SUCCESS: " <> msg <> "\n"

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
debug msg = do
  verbose <- gets (optVerbose . envOptions)
  when verbose $ do
    coloredPrint Magenta $
      "DEBUG: " <> msg <> "\n"

debugShow :: Show a => Text -> a -> Appl ()
debugShow title val = debug (title <> "\n" <> Text.pack (show val))

debugYaml :: ToJSON a => Text -> a -> Appl ()
debugYaml title val = debug (title <> "\n" <> Text.decodeUtf8 (Yaml.encode val))

versionP :: Version -> Appl ()
versionP (Version ver) = coloredPrint Green $
  "VERSION: " <> ver <> "\n"
