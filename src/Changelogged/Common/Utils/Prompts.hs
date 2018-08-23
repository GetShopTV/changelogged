{-# LANGUAGE OverloadedStrings #-}
module Changelogged.Common.Utils.Prompts where

import System.Console.ANSI (Color (..))

import Data.Monoid ((<>))
import Data.Text (Text)

import Changelogged.Common.Types
import Changelogged.Common.Utils.Printing

promptYesNo :: Text -> Appl Bool
promptYesNo message = do
  no <- gets (optNoPrompts . envOptions)
  if no
    then return False
    else coloredPrint Yellow message >> go
  where go = do
          coloredPrint Cyan "(y/n):  \n"
          answer <- liftIO getLine
          case answer of
            "y" -> return True
            "yes" -> return True
            "n" -> return False
            "no" -> return False
            _ -> do
              liftIO $ putStrLn "Cannot parse answer. Please repeat."
              go

promptGoInteractive :: Appl Bool
promptGoInteractive = promptYesNo "You can go to interactive mode or simply write changes to changelog. Go to interactive mode?\n"

promptBumpVersions :: Appl Bool
promptBumpVersions = promptYesNo "Do you want to bump versions?\n"

promptAcceptPredictedVersion :: Level -> Appl Bool
promptAcceptPredictedVersion level = promptYesNo $ "Latest changes seem to be " <> showHumanReadableLevel level <> ". Is it right?\n"

promptSkip :: Appl Interaction
promptSkip = return Skip

promptSimple :: Appl Interaction
promptSimple = return Write

promptInteractive :: Appl Interaction
promptInteractive = go
  where go = do
          coloredPrint Cyan "(↵/(s)kip/(e)xpand/(r)emind/(i)gnore/(q)uit/(a)ll):  \n"
          answer <- liftIO getLine
          case answer of
            "" -> return Write
            "w" -> return Write
            "W" -> return Write
            "Write" -> return Write
            "write" -> return Write
            "s" -> return Skip
            "skip" -> return Skip
            "Skip" -> return Skip
            "S" -> return Skip
            "e" -> return Expand
            "E" -> return Expand
            "Expand" -> return Expand
            "expand" -> return Expand
            "r" -> return Remind
            "remind" -> return Remind
            "Remind" -> return Remind
            "R" -> return Remind
            "i" -> return IgnoreAlways
            "ignore" -> return IgnoreAlways
            "Ignore" -> return IgnoreAlways
            "I" -> return IgnoreAlways
            "q" -> return Quit
            "quit" -> return Quit
            "a" -> return WriteRest
            "all" -> return WriteRest
            _ -> do
              liftIO $ putStrLn "Cannot parse action. Please repeat."
              go

levelPrompt :: Appl Level
levelPrompt = do
  coloredPrint Yellow $ "Changelogged cannot predict version number by changelog.\n"
                     <> "Choose level of changes:\n"
  go
  where go = do
          coloredPrint Cyan "(app/major/minor/fix/doc):  \n"
          answer <- liftIO getLine
          case answer of
            "App" -> return App
            "app" -> return App
            "Major" -> return Major
            "major" -> return Major
            "Minor" -> return Minor
            "minor" -> return Minor
            "Fix" -> return Fix
            "fix" -> return Fix
            "Doc" -> return Doc
            "doc" -> return Doc
            _ -> do
              liftIO $ putStrLn "Cannot parse level. Please repeat."
              go
