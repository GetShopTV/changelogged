{-# LANGUAGE OverloadedStrings #-}
module Changelogged.Common.Utils.IO where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.String.Conversions

import qualified Filesystem.Path.CurrentOS as Path

import Turtle hiding (FilePath)

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

makeWildcardPattern :: String -> Pattern Text
makeWildcardPattern [] = mempty
makeWildcardPattern ('*':'*':xs) = star dot <> suffix (makeWildcardPattern xs)
makeWildcardPattern ('*':xs) = star (noneOf "/") <> makeWildcardPattern xs
makeWildcardPattern (sym:xs) = text (Text.singleton sym) <> makeWildcardPattern xs
