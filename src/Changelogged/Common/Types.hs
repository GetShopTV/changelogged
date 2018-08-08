{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Changelogged.Common.Types
  ( module Changelogged.Common.Types.Common
  , module Changelogged.Common.Types.Options
  , module Changelogged.Common.Types.Config
  , module Changelogged.Common.Types.Git
  , module Control.Monad.Reader
  , Appl(..)
  , runInAppl
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader

import Changelogged.Common.Types.Common
import Changelogged.Common.Types.Options
import Changelogged.Common.Types.Config
import Changelogged.Common.Types.Git

newtype Appl a = Appl { runAppl :: ReaderT Options IO a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Options, MonadIO, MonadBase IO, MonadThrow, MonadCatch)

runInAppl :: Options -> Appl a -> IO a
runInAppl opts r = runReaderT (runAppl r) opts