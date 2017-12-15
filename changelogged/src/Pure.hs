module Pure where

import Prelude hiding (FilePath)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as Text

import Filesystem.Path.CurrentOS (encodeString, FilePath)

import Types

maxByLen :: [Text] -> Text
maxByLen = foldl1 (\left right -> if Text.length left > Text.length right then left else right)

defaultedEmpty :: Maybe (HM.HashMap k v) -> HM.HashMap k v
defaultedEmpty Nothing = HM.empty
defaultedEmpty (Just hm) = hm

fromJustCustom :: Maybe a -> String -> a
fromJustCustom Nothing msg = error msg
fromJustCustom (Just a) _ = a

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
delimited ver = tuplify $ map (read . Text.unpack) (Text.split (=='.') ver)

bump :: (Int, Int, Int, Int, Int) -> Level -> Text
bump (app, major, minor, fix, doc) lev = Text.intercalate "." $ map showText $ case lev of
  App -> [app + 1, 0]
  Major -> [app, major + 1, 0]
  Minor -> [app, major, minor + 1, 0]
  Fix -> [app, major, minor, fix + 1, 0]
  Doc -> [app, major, minor, fix, doc + 1]

showPath :: FilePath -> Text
showPath = Text.pack . encodeString

showText :: Show a => a -> Text
showText = Text.pack . show
