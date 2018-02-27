-- |Common regexps in Turtle syntax.
module Pattern where

import Turtle.Pattern
import Turtle ((<>), (<|>))
import Data.Text (Text, intercalate)

import Pure

-- >>> match versionExactRegex "version:   1.1.1"
-- []
-- >>> match versionExactRegex "1.1.1"
-- ["1.1.1"]
-- >>> match versionExactRegex "1.1.1   yy"
-- []
versionExactRegex :: Pattern Text
versionExactRegex = once digit <> "." <> (intercalate "." <$> (plus digit) `sepBy1` ".")

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

-- >>> match hashGrepExclude "ee17741 Merge pull request #38 from GetShopTV/redesign-strategies"
-- []
-- >>> match hashGrepExclude "ee17741 Reference pull request #38 from GetShopTV/redesign-strategies"
-- [()]
hashGrepExclude :: Pattern ()
hashGrepExclude = invert (hashRegex <> spaces <> text "Merge")

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

-- >>> match (jsonGrep "var") "\"var\" : \"1.1.1\""
-- ["\"var\" : \"1.1.1\""]
-- >>> match (jsonGrep "v") "\"var\" : \"1.1.1\""
-- []
-- >>> match (jsonGrep "v") "\"v\" : 1.1.1\""
-- []
-- >>> match (jsonGrep "v") "\"v\":\"1.1.1\""
-- ["\"v\":\"1.1.1\""]
-- >>> match (jsonGrep "v") "\"v\"=\"1.1.1\""
-- []
jsonGrep :: Text -> Pattern Text
jsonGrep var = has $ jsonVarGrep var <> spaces <> "\"" <> versionExactRegex <> "\""

-- >>> match (cabalGrep "var") "var: 1.1.1"
-- ["var: 1.1.1","var: 1.1"]
-- >>> match (cabalGrep "var") "v: 1.1.1"
-- []
-- >>> match (cabalGrep "var") "var: (1.1.1)"
-- []
-- >>> match (cabalGrep "var") "var: 1.1.1 2.2.2.2 "
-- ["var: 1.1.1","var: 1.1"]
-- >>> match (cabalGrep "var") "var: 1.1.1"
-- ["var: 1.1.1"]
-- >>> match (cabalGrep "var") "var: 1.1.1 "
-- ["var:1.1.1"]
-- >>> match (cabalGrep "var") "var: 1.1.1h "
-- []
cabalGrep :: Text -> Pattern Text
cabalGrep var = choice [has $ cabalVarGrep var <> between spaces spaces1 versionExactRegex, suffix $ cabalVarGrep var <> spaces <> versionExactRegex]

-- >>> match (hsGrep "var") "var= \"1.1.1\" "
-- ["var= \"1.1.1\""]
-- >>> match (hsGrep "var") "v= \"1.1.1\" "
-- []
-- >>> match (hsGrep "var") "var= 1.1.1 "
-- []
hsGrep :: Text -> Pattern Text
hsGrep var = has $ hsVarGrep var <> spaces <> "\"" <> versionExactRegex <> "\""

yamlGrep :: Text -> Pattern Text
yamlGrep = cabalGrep

-- >>> match (jsonVarGrep "var") "var :" 
-- []
-- >>> match (jsonVarGrep "var") "var =" 
-- []
-- >>> match (jsonVarGrep "var") "\"var\" :" 
-- ["\"var\" :"]
jsonVarGrep :: Text -> Pattern Text
jsonVarGrep var = "\"" <> text var <> "\"" <> spaces <> ":"

-- >>> match (hsVarGrep "var") "var =" 
-- ["var ="]
-- >>> match (hsVarGrep "var") "var :" 
-- []
-- >>> match (hsVarGrep "var") "\"var\" :" 
-- []
cabalVarGrep :: Text -> Pattern Text
cabalVarGrep var = text var <> spaces <> ":"

-- >>> match (cabalVarGrep "var") "\"var\" :" 
-- []
-- >>> match (cabalVarGrep "var") "var :" 
-- ["var :"]
-- >>> match (cabalVarGrep "var") "var =" 
-- []
hsVarGrep :: Text -> Pattern Text
hsVarGrep var = text var <> spaces <> "="

yamlVarGrep :: Text -> Pattern Text
yamlVarGrep = cabalVarGrep
