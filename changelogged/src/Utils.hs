module Utils where

import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

import System.Console.ANSI

import Turtle.Format

-- |Print '@text@' with ansi-terminal color.
coloredPrint :: Color -> Text -> IO ()
coloredPrint color line = do
  setSGR [SetColor Foreground Vivid color]
  printf s line
  setSGR [Reset]

defaultedEmpty :: Maybe (HM.HashMap k v) -> HM.HashMap k v
defaultedEmpty Nothing = HM.empty
defaultedEmpty (Just hm) = hm
