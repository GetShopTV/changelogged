{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Changelogged.Common.Types
  ( module Changelogged.Common.Types.Common
  , module Changelogged.Common.Types.Options
  , module Changelogged.Common.Types.Config
  , module Changelogged.Common.Types.Git
  , module Control.Monad.State.Lazy
  , ChangeloggedEnv(..)
  , Appl(..)
  , runInAppl
  ) where

import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.State.Lazy

import           Changelogged.Common.Types.Common
import           Changelogged.Common.Types.Config
import           Changelogged.Common.Types.Git
import           Changelogged.Common.Types.Options

data ChangeloggedEnv = ChangeloggedEnv
  { envOptions    :: Options
  , envConfig     :: Config
  } deriving Show

newtype Appl a = Appl { runAppl :: StateT ChangeloggedEnv IO a }
  deriving newtype (Functor, Applicative, Monad, MonadState ChangeloggedEnv, MonadIO, MonadBase IO, MonadThrow, MonadCatch)

runInAppl :: ChangeloggedEnv -> Appl a -> IO a
runInAppl env r = evalStateT (runAppl r) env
