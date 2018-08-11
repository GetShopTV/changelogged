module Changelogged.Utils.Naming (nameModifier) where

import           Data.Char (isDigit, isUpper, toLower)
import           Data.List (intercalate)

-- | Default name modifier.
-- Transforms to @lowerCamelCase@ and cuts longest common whole-word prefix.
--
-- >>> nameModifier "Dog" "dogName"
-- "name"
-- >>> nameModifier "PersonEntry" "personLastName"
-- "last_name"
-- >>> nameModifier "MessageStatus" "MessageSent"
-- "sent"
-- >>> nameModifier "SMSEntry" "smsExpiresAt"
-- "expires_at"
-- >>> nameModifier "Progress" "progress75"
-- "75"
nameModifier
  :: String -- ^ Datatype name.
  -> String -- ^ Field name (for records) or constructor name (for sum types).
  -> String
nameModifier prefix name = intercalate "_" (map (map toLower) suffix)
  where
    (_, _, suffix) = commonPrefixOn (map toLower) (splitCamelWords prefix) (splitCamelWords name)

-- | Strip longest common prefix of to lists.
--
-- >>> commonPrefixOn id "MessageStatus" "MessageDelivered" :: (String, String, String)
-- ("Message","Status","Delivered")
-- >>> commonPrefixOn id "MessageStatus" "MessageSent" :: (String, String, String)
-- ("MessageS","tatus","ent")
--
-- prop> commonPrefixOn id xs xs == (xs, [], [])
-- prop> commonPrefixOn id [] xs == ([], [], xs)
-- prop> commonPrefixOn id xs [] == ([], xs, [])
commonPrefixOn :: Eq b => (a -> b) -> [a] -> [a] -> ([a], [a], [a])
commonPrefixOn f (x:xs) (y:ys)
  | f x == f y = (x : prefix, xs', ys')
  | otherwise  = ([], x:xs, y:ys)
  where
    (prefix, xs', ys') = commonPrefixOn f xs ys
commonPrefixOn _ xs ys = ([], xs, ys)

-- | Split @CamelCase@ name into its constituent words.
--
-- >>> splitCamelWords "CamelCase"
-- ["Camel","Case"]
-- >>> splitCamelWords "SMSEntry"
-- ["SMS","Entry"]
-- >>> splitCamelWords "progress75"
-- ["progress","75"]
--
-- prop> concat (splitCamelWords s) == s
splitCamelWords :: String -> [String]
splitCamelWords = reverse . splitWordsReversed . reverse
  where
    splitWordsReversed :: String -> [String]
    splitWordsReversed [] = []
    splitWordsReversed rs
      | null ls   = reverse us : splitWordsReversed urs
      | otherwise = case lrs of
                      []     -> [reverse ls]
                      (c:cs) -> (c : reverse ls) : splitWordsReversed cs
      where
        (ls, lrs) = span (not . isBorder) rs
        (us, urs) = span isBorder rs
        isBorder c = isUpper c || isDigit c
