{-# LANGUAGE OverloadedStrings #-}
module Changelogged.Common.Utils.Prompts where

import System.Console.ANSI (Color (..))

import Data.Monoid ((<>))

import Changelogged.Common.Types
import Changelogged.Common.Utils.Printing

promptGoInteractive :: Appl Bool
promptGoInteractive = do
  coloredPrint Yellow $ "You can go to interactive mode or simply write changes to changelog. Go to interactive mode?\n"
  go
  where go = do
          coloredPrint Cyan "(y/n):  \n"
          answer <- liftIO getLine
          case answer of
            "y" -> return True
            "n" -> return False
            _ -> do
              liftIO $ putStrLn "Cannot parse answer. Please repeat."
              go

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

levelPrompt :: Appl (Maybe Level)
levelPrompt = do
  -- FIXME: here will be prompt. And versions must not be inferred, it's too formal.
  coloredPrint Yellow $ "Changelog does not contain any new version level labels.\n"
                     <> "You can specify level of changes explicitly or press Enter to miss bumping versions.\n"
  go
  where go = do
          coloredPrint Cyan "(↵/app↵/major↵/minor↵/fix↵/doc↵):  \n"
          answer <- liftIO getLine
          case answer of
            "" -> return Nothing
            "App" -> return (Just App)
            "app" -> return (Just App)
            "Major" -> return (Just Major)
            "major" -> return (Just Major)
            "Minor" -> return (Just Minor)
            "minor" -> return (Just Minor)
            "Fix" -> return (Just Fix)
            "fix" -> return (Just Fix)
            "Doc" -> return (Just Doc)
            "doc" -> return (Just Doc)
            _ -> do
              liftIO $ putStrLn "Cannot parse level. Please repeat."
              go
