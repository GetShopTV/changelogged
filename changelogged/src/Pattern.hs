-- |Common regexps in Turtle syntax.
module Pattern where

import Turtle.Pattern
import Turtle ((<>), (<|>))
import Data.Text

import Pure

versionExactRegex :: Pattern Text
versionExactRegex = once digit <> "." <> (intercalate "." <$> (plus digit) `sepBy1` ".")

versionGrep :: Pattern Text
versionGrep = once (notChar '.') <> versionExactRegex <> once (notChar '.')

versionMatch :: Text -> Maybe Text
versionMatch str = maxByLen $ match (has versionExactRegex) str

hashRegex :: Pattern Text
hashRegex = prefix $ plus (digit <|> lower)

hashGrepExclude :: Pattern ()
hashGrepExclude = invert (hashRegex <> spaces <> text "Merge")

hashMatch :: Text -> Maybe Text
hashMatch str = maxByLen $ match hashRegex str

githubRefRegex :: Pattern Text
githubRefRegex = has $ "#" <> plus digit

githubRefGrep :: Pattern Text
githubRefGrep = has (text "pull request #")

githubRefMatch :: Text -> Maybe Text
githubRefMatch str = maxByLen $ match githubRefRegex str

jsonGrep :: Text -> Pattern Text
jsonGrep var = has $ "\"" <> text var <> "\"" <> spaces <> ":" <> spaces <> "\"" <> versionExactRegex <> "\""

cabalGrep :: Text -> Pattern Text
cabalGrep var = has $ text var <> spaces <> ":" <> spaces <> versionExactRegex

hsGrep :: Text -> Pattern Text
hsGrep var = has $ text var <> spaces <> "=" <> spaces <> "\"" <> versionExactRegex <> "\""
