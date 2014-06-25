{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Test.QuickCheck.Monadic.WebDriver where

import Prelude

import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Monadic as QCM

import Test.WebDriver

import Data.Monoid                            ((<>))
import Control.Monad.IO.Class

monadicWD capabilities wd = monadic wdProperty wd
    where runSesh    = runSession defaultSession capabilities
          wdProperty = ioProperty . runSesh

runIO :: IO a -> PropertyM WD a
runIO = QCM.run . liftIO
