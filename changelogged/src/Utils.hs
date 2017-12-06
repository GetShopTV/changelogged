module Utils where

import Data.Text (Text)

import System.Console.ANSI

import Turtle.Format

coloredPrint :: Color -> Text -> IO ()
coloredPrint color line = do
  setSGR [SetColor Foreground Vivid color]
  printf s line
  setSGR [Reset]
