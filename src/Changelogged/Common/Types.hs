{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Changelogged.Common.Types
  ( module Changelogged.Common.Types.Common
  , module Changelogged.Common.Types.Options
  , module Changelogged.Common.Types.Config
  , module Changelogged.Common.Types.Git
  , module Control.Monad.Reader
  , ChangeloggedEnv(..)
  , Appl(..)
  , runInAppl
  ) where

import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Reader

import           Changelogged.Common.Types.Common
import           Changelogged.Common.Types.Config
import           Changelogged.Common.Types.Git
import           Changelogged.Common.Types.Options

data ChangeloggedEnv = ChangeloggedEnv
  { envOptions :: Options
  , envConfig  :: Config
  } deriving Show

newtype Appl a = Appl { runAppl :: ReaderT ChangeloggedEnv IO a }
  deriving newtype (Functor, Applicative, Monad, MonadReader ChangeloggedEnv, MonadIO, MonadBase IO, MonadThrow, MonadCatch)

runInAppl :: Options -> Config -> Appl a -> IO a
runInAppl opts cfg r = runReaderT (runAppl r) (ChangeloggedEnv opts cfg)
