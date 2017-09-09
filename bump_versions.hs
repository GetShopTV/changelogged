#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Turtle
import Prelude hiding (FilePath, log)

import qualified Data.Yaml as Yaml

import qualified Control.Foldl as Fold

import Data.Text (pack, Text)
import Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import Data.Tuple.Select

import GHC.Generics

data Part = API | Project
data Level = App | Major | Minor | Fix | Doc
data Mode = PR | Commit

instance Show Mode where
  show PR = "Pull request"
  show Commit = "Single commit"

data Paths = Paths {
  -- Path to swagger file of API
  swaggerFileName :: Maybe Text
  } deriving (Show, Generic)

instance Yaml.FromJSON Paths

loadPaths :: IO Paths
loadPaths = do
  ms <- Yaml.decodeFileEither "./paths"
  case ms of
    Left err -> error (show err)
    Right paths -> return paths

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

apiChangelogFile :: Text
apiChangelogFile = "API_CHANGELOG.md"

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
  stdout (inproc "sed" ["-i", "-r", expr, file] empty)
  where
    packageFile = fromText packageName
    file = format fp $ packageFile </> packageFile <.> "cabal"
    expr = "s/(^version:[^0-9]*)[0-9][0-9.]*/\\1" <> version <> "/"

bumpPackages :: Text -> [Text] -> IO ()
bumpPackages version packages = do
  curVersion <- currentVersion
  stdout $ inproc "echo" ["-e", (report curVersion)] empty

  printf ("Updating packages version to "%s%"\n") version
  mapM_ (bumpPackage version) packages
  
  where
    report v = "Version: " <> v <> " -> " <> yellow <> version <> nc

changelogIsUp :: Text -> Mode -> Part -> Text -> IO Bool
changelogIsUp item mode part message = do
  grepLen <- case part of
    Project -> fold (inproc "grep" [item, changelogFile] empty) Fold.length
    API -> fold (inproc "grep" [item, apiChangelogFile] empty) Fold.length
  case grepLen of
    0 -> do
      stdout $ inproc "echo" ["-e", report part] empty
      return False
    _ -> return True
  where
    report Project = "- " <> showText mode <> " " <> cyan <> item <> nc <> " is missing in changelog: " <> message
    report API = "- " <> showText mode <> " " <> cyan <> item <> nc <> " is missing in API changelog: " <> message

commitMessage :: Mode -> Text -> IO Text
commitMessage _ "" = return ""
commitMessage mode commit = do
  raw <- strict $ inproc "grep" ["^\\s"] $
                    inproc "sed" ["-n", sedString] $
                      inproc "git" ["show", commit] empty
  return $ T.stripEnd $ T.stripStart raw
  where
    sedString = case mode of
      PR -> "8p"
      Commit -> "5p"

gitLatestHistory :: Bool -> IO Text
gitLatestHistory start = do
  tmpFile <- strict $ inproc "mktemp" [] empty
  case start of
    False -> liftIO $ append (process tmpFile) $ inproc "grep" ["-v", "\"Merge branch\""] (inshell history empty)
    True  -> liftIO $ append (process tmpFile) $
      inproc "grep" ["-v", "\"Merge branch\""] (inproc "git" ["log", "--oneline", "--first-parent"] empty)
  return $ T.stripEnd tmpFile
  where
    process = fromText . T.stripEnd
    history = "git log --oneline --first-parent $( " <> latestGitTag <> " )..HEAD"

checkApiChangelogF :: Bool -> Text -> IO Bool
checkApiChangelogF start swaggerFile = do
  printf ("Checking "%s%"\n") apiChangelogFile
  
  history <- gitLatestHistory start

  commits <- strict $ inproc "egrep" ["-o", "^[0-9a-f]+", history] empty
  
  flags <- mapM (eval history) (T.split (== '\n') (T.stripEnd commits))
  return $ foldr1 (&&) flags
  where
    eval hist commit = do
      linePresent <- fold
        (inproc "grep" [swaggerFile]
          (inproc "git" ["show", "--stat", commit] empty))
        Fold.length
      case linePresent of
        0 -> return True
        _ -> do
          pull <- strict $
            inproc "egrep" ["-o", "#[0-9]+"] $
              inproc "egrep" ["-o", "pull request #[0-9]+"] $
                inproc "grep" [commit, hist] empty
          case T.length pull of
            0 -> do
              message <- commitMessage Commit commit
              changelogIsUp commit Commit API message
            _ -> do
              message <- commitMessage PR commit
              changelogIsUp (T.stripEnd pull) PR API message

checkChangelogF :: Bool -> IO Bool
checkChangelogF start = do
  printf ("Checking "%s%"\n") changelogFile
  
  history <- gitLatestHistory start
  
  pullCommits <- strict $
    inproc "egrep" ["-o", "^[0-9a-f]+"] (inproc "egrep" [pullExpr, history] empty)
  pulls <- strict $
    inproc "egrep" ["-o", "#[0-9]+"] (inproc "egrep" ["-o", pullExpr, history] empty)
  singles <- strict $
    inproc "egrep" ["-o", "^[0-9a-f]+"] (inproc "egrep" ["-o", singleExpr, history] empty)
  
  pullHeaders <- mapM (commitMessage PR) (map T.stripEnd (T.split (== '\n') pullCommits))
  singleHeaders <- mapM (commitMessage Commit) (map T.stripEnd (T.split (== '\n') singles))
  flagsPR <- mapM (\(i,m) -> changelogIsUp i PR Project m) (zip (T.split (== '\n') pulls) pullHeaders)
  flagsCommit <- mapM (\(i, m) -> changelogIsUp i Commit Project m) (zip (T.split (== '\n') singles) singleHeaders)
  return $ foldr1 (&&) (flagsPR ++ flagsCommit)
  where
    pullExpr = "pull request #[0-9]+"
    singleExpr = "^[0-9a-f]+\\s[^(Merge)]"

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

processChecks :: Bool -> Bool -> Paths -> IO ()
processChecks True _ _ = stdout (inproc "echo" ["-e", warn] empty)
  where
    warn = yellow <> "WARNING: skipping checks for changelogs." <> nc
processChecks False start paths = do
  case start of
    True -> echo "Checking changelogs from start of project"
    False -> return ()
  upToDate <- checkChangelogF start
  case upToDate of
    False -> stdout (inproc "echo" ["-e", pfail Project] empty)
    True -> stdout (inproc "echo" ["-e", pwin API] empty)
  apiUpToDate <- case swaggerFileName paths of
    Nothing -> do
      echo "Do not check API changelog, no swagger file added to paths.dhall"
      return True
    Just file -> checkApiChangelogF start file
  case apiUpToDate of
    False -> stdout (inproc "echo" ["-e", pfail API] empty)
    True -> stdout (inproc "echo" ["-e", pwin API] empty)
  case apiUpToDate && upToDate of
    False -> do
      stdout (inproc "echo" ["-e", dieText] empty)
      exit ExitSuccess

  where
    pfail Project = yellow <> "WARNING: " <> changelogFile <> " is out of date." <> nc
    pfail API = yellow <> "WARNING: " <> apiChangelogFile <> " is out of date." <> nc
    pwin Project = green <> changelogFile <> " is up to date." <> nc
    pwin API = green <> apiChangelogFile <> " is up to date." <> nc
    dieText = red <> "ERROR: some changelogs are not up-to-date. Use -c or --no-check options if you want to ignore changelog checks." <> nc

generateVersionByChangelog :: Bool -> IO Text
generateVersionByChangelog True = do
  echo "You are running it with no explicit version modifiers and changelog checks. It can result in anything. Please retry"
  exit ExitSuccess
generateVersionByChangelog False = do
  major <- fold (inproc "grep" ["Major changes"] unreleased) Fold.length
  minor <- fold (inproc "grep" ["Minor changes"] unreleased) Fold.length
  fixes <- fold (inproc "grep" ["Fixes"] unreleased) Fold.length
  docs  <- fold (inproc "grep" ["Docs"] unreleased) Fold.length
  curVersion <- currentVersion
  
  case major of
    0 -> case minor of
      0 -> case fixes of
        0 -> case docs of
          0 -> do
            stdout (inproc "echo" ["-e", noNews] empty)
            return curVersion
          _ -> generateVersion Doc
        _ -> generateVersion Fix
      _ -> generateVersion Minor
    _ -> generateVersion Major
  
  where
    noNews = yellow <> "WARNING: keep old version since " <>
              changelogFile <>" apparently does not contain any new entries." <> nc
    expr =  "/^[0-9]\\.[0-9]/q"
    unreleased = (inproc "sed" [expr, changelogFile] empty)

main :: IO ()
main = do
  (packages, packageLev, ignoreChecks, fromStart) <- options welcome parser

  paths <- loadPaths

  cd ".."

  processChecks ignoreChecks fromStart paths

  newVersion <- case packageLev of
    Nothing -> generateVersionByChangelog ignoreChecks
    Just lev -> generateVersion (levelFromText lev)

  case packages of
    Just project -> bumpPackages newVersion (T.split (==' ') project)
    Nothing -> case ignoreChecks of
      True  -> stdout (inproc "echo" ["-e", warnNoCheck] empty)
      False -> stdout (inproc "echo" ["-e", warnCheck] empty)

  where
    warnNoCheck = yellow <> "WARNING: No packages specified." <> nc
    warnCheck = yellow <> "WARNING: No packages specified, so only check changelogs" <> nc
