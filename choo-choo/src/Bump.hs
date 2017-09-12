module Bump where

import Turtle
import Prelude hiding (FilePath, log)

import qualified Control.Foldl as Fold

import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple.Select

import System.Console.ANSI (Color(..))

import Types

latestGitTag :: Text
latestGitTag = "git describe --tags origin/master"

currentVersion :: Part -> Maybe Text -> IO Text
currentVersion Project _ = do
  ver <- strict $ inproc "cut" ["-c", "2-"] (inproc "git" ["describe", "--tags", "origin/master"] empty)
  return $ T.stripEnd ver
currentVersion API Nothing = do
  coloredPrint Green "Do not bump API, no swagger file specified in ./paths.\n"
  return ""
currentVersion API (Just swagger) = do
  ver <- case snd (T.breakOnEnd "." swagger) of
    "json" -> strict $ inproc "egrep" ["-o", "[0-9][0-9.]*"] (inproc "egrep" ["\"version\": \"[0-9][0-9.]*\"", swagger] empty)
  return $ T.stripEnd ver

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
  curVersion <- currentVersion Project Nothing
  printf ("Version: "%s%" -> ") curVersion
  coloredPrint Yellow (version <> "\n")

  printf ("Updating packages version to "%s%"\n") version
  mapM_ (bumpPackage version) packages

bumpApiPart :: Text -> (Text, Text) -> IO ()
bumpApiPart version (file, var) = do
    printf ("- Updating API version for "%s%"\n") file
    case snd (T.breakOnEnd "." file) of
      "hs" -> do
        _ <- strict $ inproc "sed" ["-i", "-r", hsExpr, file] empty
        return ()
      "json" -> do
        _ <- strict $ inproc "sed" ["-i", "-r", jsonExpr, file] empty
        return ()
      _ -> coloredPrint Red ("ERROR: Didn't bump version in " <> file <> " : only .hs and .json supported, sorry.")
  where
    jsonExpr = "s/(^\\s*\"" <> var <> "\": )\"[0-9][0-9.]*\"/\\1\"" <> version <> "\"/"
    hsExpr = "s/(^" <> var <> " = )\\\"[0-9][0-9.]*\\\"/\\1\"" <> version <> "\"/"

changelogIsUp :: Text -> Mode -> Part -> Text -> IO Bool
changelogIsUp item mode part message = do
  grepLen <- case part of
    Project -> fold (inproc "grep" [item, changelogFile] empty) Fold.length
    API -> fold (inproc "grep" [item, apiChangelogFile] empty) Fold.length
  case grepLen of
    0 -> do
      printf ("- "%s%" ") (showText mode)
      coloredPrint Cyan item
      case part of
        Project -> printf (" id missing in changelog: "%s%".\n") message
        API -> printf (" id missing in API changelog: "%s%".\n") message
      return False
    _ -> return True

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
    False -> liftIO $ append (process tmpFile) $ inproc "grep" ["-v", "Merge branch"] (inshell history empty)
    True  -> liftIO $ append (process tmpFile) $
      inproc "grep" ["-v", "Merge branch"] (inproc "git" ["log", "--oneline", "--first-parent"] empty)
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

generateVersion :: Level -> Part -> Maybe Text -> IO Text
generateVersion lev part mbSwagger = do
  current <- currentVersion part mbSwagger
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

processChecks :: Bool -> Bool -> Bool -> Maybe Text -> IO ()
processChecks True _ _ _ = coloredPrint Yellow "WARNING: skipping checks for changelog.\n"
processChecks False start force swagger = do
  case start of
    True -> echo "Checking changelogs from start of project"
    False -> return ()
  upToDate <- checkChangelogF start
  case upToDate of
    False -> coloredPrint Yellow ("WARNING: " <> changelogFile <> " is out of date.\n")
    True -> coloredPrint Green (changelogFile <> " is up to date.\n")
  apiUpToDate <- case swagger of
    Nothing -> do
      coloredPrint Yellow "Do not check API changelog, no swagger file added to ./paths.\n"
      return True
    Just file -> checkApiChangelogF start file
  case apiUpToDate of
    False -> coloredPrint Yellow ("WARNING: " <> apiChangelogFile <> " is out of date.\n")
    True -> coloredPrint Green $ (apiChangelogFile<>" is up to date.\n")
  case apiUpToDate && upToDate of
    False -> do
      coloredPrint Red "ERROR: some changelogs are not up-to-date. Use -c or --no-check options if you want to ignore changelog checks and -f to bump anyway.\n"
      case force of
        False -> exit ExitSuccess
        True -> return ()
    True -> return ()

generateVersionByChangelog :: Bool -> Part -> Maybe Text -> IO Text
generateVersionByChangelog True _ _ = do
  coloredPrint Yellow "You are running it with no explicit version modifiers and changelog checks. It can result in anything. Please retry.\n"
  exit ExitSuccess
generateVersionByChangelog False part mbSwagger = do
  major <- fold (inproc "grep" ["Major changes"] unreleased) Fold.length
  minor <- fold (inproc "grep" ["Minor changes"] unreleased) Fold.length
  fixes <- fold (inproc "grep" ["Fixes"] unreleased) Fold.length
  docs  <- fold (inproc "grep" ["Docs"] unreleased) Fold.length
  curVersion <- currentVersion part mbSwagger
  
  case major of
    0 -> case minor of
      0 -> case fixes of
        0 -> case docs of
          0 -> do
            case part of
              Project -> coloredPrint Yellow ("WARNING: keep old version since " <> changelogFile <> " apparently does not contain any new entries.\n")
              API -> coloredPrint Yellow ("WARNING: keep old API version since " <> apiChangelogFile <> " apparently does not contain any new entries.\n")
            return curVersion
          _ -> generateVersion Doc Project Nothing
        _ -> generateVersion Fix Project Nothing
      _ -> generateVersion Minor Project Nothing
    _ -> generateVersion Major Project Nothing
  
  where
    expr =  "/^[0-9]\\.[0-9]/q"
    unreleased = case part of
      Project -> inproc "sed" [expr, changelogFile] empty
      API -> inproc "sed" [expr, apiChangelogFile] empty
