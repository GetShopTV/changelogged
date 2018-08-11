{-# LANGUAGE OverloadedStrings #-}
-- |Common regexps in Turtle syntax.
module Changelogged.Pattern where

import           Data.Text           (Text, intercalate)
import           Turtle              ((<>), (<|>))
import           Turtle.Pattern

import           Changelogged.Common

-- >>> match versionExactRegex "version:   1.1.1"
-- []
-- >>> match versionExactRegex "1.1.1"
-- ["1.1.1"]
-- >>> match versionExactRegex "1.1.1   yy"
-- []
versionExactRegex :: Pattern Text
versionExactRegex = once digit <> "." <> (intercalate "." <$> plus digit `sepBy1` ".")

-- >>> versionMatch "\"version\":    \"1.1.1\""
-- Just "1.1.1"
-- >>> versionMatch "version =    1.1.1"
-- Just "1.1.1"
-- >>> versionMatch "version =    v1.1.1"
-- Just "1.1.1"
-- >>> versionMatch "version =    v1.1."
-- Just "1.1"
-- >>> versionMatch "version =    v.1."
-- Nothing
-- >>> versionMatch "version =    .1."
-- Nothing
-- >>> versionMatch "version =    1.1."
-- Just "1.1"
versionMatch :: Text -> Maybe Text
versionMatch str = maxByLen $ match (has versionExactRegex) str

-- >>> match hashRegex "f4875f4 Update changelog"
-- ["f4875f4","f4875f","f4875","f487","f48","f4","f"]
-- >>> match hashRegex "f4875f4 Update changelog d1123d"
-- ["f4875f4","f4875f","f4875","f487","f48","f4","f"]
-- >>> match hashRegex "f4875f423d Update changelog d1123d"
-- ["f4875f423d","f4875f423","f4875f42","f4875f4","f4875f","f4875","f487","f48","f4","f"]
-- >>> match hashRegex "fE4875f423d Update changelog"
-- []
-- >>> match hashRegex "Update changelog"
-- []
hashRegex :: Pattern Text
hashRegex = prefix $ between (within 0 chars) spaces1 (plus (digit <|> lower))

-- >>> hashMatch "f4875f4 Update changelog"
-- Just "f4875f4"
-- >>> hashMatch "Update changelog"
-- Nothing
hashMatch :: Text -> Maybe Text
hashMatch str = maxByLen $ match hashRegex str

-- >>> match githubRefRegex "d #444 f"
-- ["#444","#44","#4"]
-- >>> match githubRefRegex "d 444 f"
-- []
-- >>> match githubRefRegex "d # f"
-- []
-- >>> match githubRefRegex "d (#444) f"
-- ["#444","#44","#4"]
githubRefRegex :: Pattern Text
githubRefRegex = has $ "#" <> plus digit

-- >>> match githubRefGrep "pull request #44"
-- ["pull request #"]
-- >>> match githubRefGrep "pull request"
-- []
-- >>> match githubRefGrep "#44"
-- []
githubRefGrep :: Pattern Text
githubRefGrep = has (text "pull request #")

-- >>> githubRefMatch "text #444 text"
-- Just "#444"
githubRefMatch :: Text -> Maybe Text
githubRefMatch str = maxByLen $ match githubRefRegex str

-- >>> isMerge "Merge branch 'release-v1.0'"
-- True
-- >>> isMerge "Merge branch 'release-v1.0' into develop"
-- True
-- >>> isMerge "merge branch release into develop"
-- False
isMerge :: Text -> Bool
isMerge = not . null . match (prefix "Merge branch '")
