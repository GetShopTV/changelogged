#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Prelude hiding (FilePath, log)

import Data.Text (pack, Text)
import qualified Data.Text as T
import Data.Tuple.Select

data Level = App | Major | Minor | Fix | Doc
data Mode = PR | Commit

instance Show Mode where
  show PR = "Pull request"
  show Commit = "Single commit"

-- color sequences
red :: Text
red = "\\033[0;31m"
green :: Text
green = "\\033[0;32m"
yellow :: Text
yellow = "\\033[0;33m"
cyan :: Text
cyan = "\\033[0;36m"
nc :: Text
nc = "\\033[0m" -- No Color

showText :: Show a => a -> Text
showText = pack . show

changelogFile :: Text
changelogFile = "CHANGELOG.md"

checkChangelogs :: Bool
checkChangelogs = True

latestGitTag :: Text
latestGitTag = "git describe --tags origin/master"

currentVersion :: IO Text
currentVersion = do
  ver <- strict $ inshell (latestGitTag <> " | cut -c 2-") empty
  return $ T.stripEnd ver

levelFromText :: Text -> Level
levelFromText "app" = App
levelFromText "App" = App
levelFromText "APP" = App
levelFromText "major" = Major
levelFromText "Major" = Major
levelFromText "MAJOR" = Major
levelFromText "minor" = Minor
levelFromText "Minor" = Minor
levelFromText "MINOR" = Minor
levelFromText "fix" = Fix
levelFromText "Fix" = Fix
levelFromText "FIX" = Fix
levelFromText "doc" = Doc
levelFromText "Doc" = Doc
levelFromText "DOC" = Doc
levelFromText _ = error "Unsupported level of changes. See supported with -h or --help."

bumpPackage :: Text -> Text -> IO ()
bumpPackage version packageName = do
  printf ("- Updating version for "%s%"\n") packageName
  stdout (inshell processing empty)
  where
    packageFile = fromText packageName
    file = format fp $ packageFile </> packageFile <.> "cabal"
    processing = "sed -i -r \"s/(^version:[^0-9]*)[0-9][0-9.]*/\\1" <> version <> "/\" " <> file

bumpPackages :: Text -> [Text] -> IO ()
bumpPackages version packages = do
  curVersion <- currentVersion
  stdout $ inshell (report curVersion) empty

  printf ("Updating packages version to "%s%"\n") version
  mapM_ (bumpPackage version) packages
  
  where
    report v = "echo \"Version: " <> v <> " -> " <> yellow <> version <> nc <> "\""

changelogIsUp :: Text -> Mode -> IO Bool
changelogIsUp item mode = do
  line <- strict $ inshell grepPR empty
  case T.length line of
    0 -> do
      stdout $ inshell report empty
      return False
    _ -> return True
  where
    report = "echo \"- " <> showText mode <> " " <> cyan <> item <> nc <> " is missing in changelog\""
    grepPR = "grep \"" <> item <> "\" " <> changelogFile

gitLatestHistory :: Bool -> IO Text
gitLatestHistory start = do
  tmpFile <- strict $ inshell "mktemp" empty
  case start of
    False -> liftIO $ append (process tmpFile) (inshell history empty)
    True  -> liftIO $ append (process tmpFile) (inshell allHistory empty)
  return $ T.stripEnd tmpFile
  where
    process = fromText . T.stripEnd
    history = "git log --oneline --first-parent $( " <> latestGitTag <> " )..HEAD | grep -v \"Merge branch\""
    allHistory = "git log --oneline --first-parent | grep -v \"Merge branch\""

checkChangelogF :: Bool -> IO Bool
checkChangelogF start = do
  printf ("Checking "%s%"\n") (changelogFile)
  
  history <- gitLatestHistory start
  
  pulls <- strict $ inshell (pullGrep history) empty
  singles <- strict $ inshell (singleGrep history) empty
  
  flagsPR <- mapM (\i -> changelogIsUp i PR) (T.split (== '\n') pulls)
  flagsCommit <- mapM (\i -> changelogIsUp i Commit) (T.split (== '\n') singles)
  return $ foldr1 (&&) (flagsPR ++ flagsCommit)
  where
    pullGrep hist = "egrep -o 'pull request #[0-9]+' " <> hist <> " | egrep -o '#[0-9]+'"
    singleGrep hist = "egrep '^[0-9a-f]+\\s[^(Merge)]' " <> hist <> " | egrep -o '^[0-9a-f]+'"

generateVersion :: Level -> IO Text
generateVersion lev = do
  current <- currentVersion
  return $ bump current
  where
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
    
    bump :: Text -> Text
    bump v = T.intercalate "." $ map showText $ case lev of
      App -> [firstD v + 1, 0]
      Major -> [firstD v, secondD v + 1, 0]
      Minor -> [firstD v, secondD v, thirdD v + 1, 0]
      Fix -> [firstD v, secondD v, thirdD v, fourthD v + 1, 0]
      Doc -> [firstD v, secondD v, thirdD v, fourthD v, fifthD v + 1]
    
    firstD v  = sel1 $ delimited v
    secondD v = sel2 $ delimited v
    thirdD v  = sel3 $ delimited v
    fourthD v = sel4 $ delimited v
    fifthD v  = sel5 $ delimited v

parser :: Parser (Maybe Text, Maybe Text, Bool, Bool)
parser = (,,,) <$> optional (optText "packages" 'p' "List of packages to bump.")
               <*> optional (optText "level" 'l' "Level of changes")
               <*> switch  "no-check"  'c' "Do not check changelogs."
               <*> switch  "from-bc"  'e' "Check changelogs from start of project"

welcome :: Description
welcome = Description $ "---\n"
        <> "This script can check your changelogs and bump versions in project.\n"
        <> "It assumes to be run in root directory of project and that changelog is here.\n"
        <> "You can specify these levels of changes: app, major, minor, fix, doc.\n"
        <> "It can infer version from changelog.\n"
        <> "But it will refuse to do it if it's not sure changelogs are up to date."

processChecks :: Bool -> Bool -> IO ()
processChecks True _ = stdout (inshell warn empty)
  where
    warn = "echo \"" <> yellow <> "WARNING: skipping checks for changelogs." <> nc <> "\""
processChecks False start = do
  case start of
    True -> echo "Checking changelogs from start of project"
    False -> return ()
  upToDate <- checkChangelogF start
  case upToDate of
    False -> do
      stdout (inshell pfail empty)
      stdout (inshell dieText empty)
      exit ExitSuccess
    True -> stdout (inshell pwin empty)
  where
    pfail = "echo \"" <> yellow <> "WARNING: " <> changelogFile <> " is out of date." <> nc <> "\""
    pwin = "echo \"" <> green <> changelogFile <> " is up to date." <> nc <> "\""
    dieText = "echo \"" <> red <>
              "ERROR: changelog is not up-to-date. Use -c or --no-check options if you want to ignore changelog checks."
              <> nc <> "\""

generateVersionByChangelog :: Bool -> IO Text
generateVersionByChangelog True = do
  echo "You are running it with no explicit version modifiers and changelog checks. It can result in anything. Please retry"
  exit ExitSuccess
generateVersionByChangelog False = do
  major <- strict $ inshell checkMajor empty
  minor <- strict $ inshell checkMinor empty
  fixes <- strict $ inshell checkFix empty
  docs  <- strict $ inshell checkDoc empty
  curVersion <- currentVersion
  
  case T.length major of
    0 -> case T.length minor of
      0 -> case T.length fixes of
        0 -> case T.length docs of
          0 -> do
            stdout (inshell noNews empty)
            return curVersion
          _ -> generateVersion Doc
        _ -> generateVersion Fix
      _ -> generateVersion Minor
    _ -> generateVersion Major
  
  where
    noNews = "echo \"" <> yellow <> "WARNING: keep old version since " <>
              changelogFile <>" apparently does not contain any new entries." <> nc <> "\""
    checkMajor =  "sed '/^[0-9]\\.[0-9]/q' " <> changelogFile <> " | grep 'Major changes'"
    checkMinor = "sed '/^[0-9]\\.[0-9]/q' " <> changelogFile <> " | grep 'Minor changes'"
    checkFix = "sed '/^[0-9]\\.[0-9]/q' " <> changelogFile <> " | grep 'Fixes'"
    checkDoc = "sed '/^[0-9]\\.[0-9]/q' " <> changelogFile <> " | grep 'Docs'"

main :: IO ()
main = do
  (packages, packageLev, ignoreChecks, fromStart) <- options welcome parser

  cd ".."

  processChecks ignoreChecks fromStart

  newVersion <- case packageLev of
    Nothing -> generateVersionByChangelog ignoreChecks
    Just lev -> generateVersion (levelFromText lev)

  case packages of
    Just project -> bumpPackages newVersion (T.split (==' ') project)
    Nothing -> case ignoreChecks of
      True -> stdout (inshell warnNoCheck empty)
      False -> stdout (inshell warnCheck empty)

  where
    warnNoCheck = "echo \"" <> yellow <> "WARNING: No packages specified." <> nc <> "\""
    warnCheck = "echo \"" <> yellow <> "WARNING: No packages specified, so only check " <>
              changelogFile <> "." <> nc <> "\""
