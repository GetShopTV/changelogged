-- Pure functions for choo-choo.

module Utils where

import Data.Text (Text)
import qualified Data.Text as T

import Types

-- I leave tuple here cause it's all functions for internal usage only.
tuplify :: [Int] -> (Int, Int, Int, Int, Int)
tuplify [] = (0,0,0,0,0)
tuplify [a1] = (a1,0,0,0,0)
tuplify [a1,a2] = (a1,a2,0,0,0)
tuplify [a1,a2,a3] = (a1,a2,a3,0,0)
tuplify [a1,a2,a3,a4] = (a1,a2,a3,a4,0)
tuplify [a1,a2,a3,a4,a5] = (a1,a2,a3,a4,a5)
tuplify (a1:a2:a3:a4:a5:_) = (a1,a2,a3,a4,a5)

delimited :: Text -> (Int, Int, Int, Int, Int)
delimited ver = tuplify $ map (read . T.unpack) (T.split (=='.') ver)

bump :: (Int, Int, Int, Int, Int) -> Level -> Text
bump (app, major, minor, fix, doc) lev = T.intercalate "." $ map showText $ case lev of
  App -> [app + 1, 0]
  Major -> [app, major + 1, 0]
  Minor -> [app, major, minor + 1, 0]
  Fix -> [app, major, minor, fix + 1, 0]
  Doc -> [app, major, minor, fix, doc + 1]
