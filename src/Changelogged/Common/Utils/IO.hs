{-# LANGUAGE OverloadedStrings #-}
module Changelogged.Common.Utils.IO where

import           Control.Monad             (when)
import           Data.Aeson                (ToJSON)
import           Data.Monoid               ((<>))
import           Data.String.Conversions
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import qualified Data.Yaml                 as Yaml

import qualified Filesystem.Path.CurrentOS as Path
import           System.Console.ANSI

import           Turtle                    (cd, pwd)
import           Turtle.Format

import           Changelogged.Common.Types

-- |Print '@text@' with ansi-terminal color.
coloredPrint :: Color -> Text -> Appl ()
coloredPrint color line = do
  noColor <- asks (optNoColors . envOptions)
  if noColor
    then printf s line
    else do
      liftIO $ setSGR [SetColor Foreground Vivid color]
      printf s line
      liftIO $ setSGR [Reset]

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
  verbose <- asks (optVerbose . envOptions)
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

splitPwdBy :: Text -> IO (Maybe Path.FilePath)
splitPwdBy gitProjectName = do
  curDirText <- cs . Path.encodeString <$> pwd
  return $ case Path.fromText . fst $ Text.breakOnEnd gitProjectName curDirText of
    ""   -> Nothing
    path -> Just path

withDir :: MonadIO m => Path.FilePath -> m a -> m a
withDir dir action = do
  prev <- pwd
  liftIO $ cd dir
  res <- action
  liftIO $ cd prev
  return res
