{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Test.QuickCheck.Monadic.WebDriver 
    (
    -- * Example
    -- $example
    ) where

import Prelude

import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Monadic as QCM

import Test.WebDriver

import Data.Monoid                            ((<>))
import Control.Monad.IO.Class

-- |Patterned after `Test.QuickCheck.Monadic.monadicIO`.
monadicWD capabilities wd = monadic wdProperty wd
    where runSesh    = runSession defaultSession capabilities
          wdProperty = ioProperty . runSesh

runIO :: IO a -> PropertyM WD a
runIO = QCM.run . liftIO

-- $example
-- > import Test.QuickCheck
-- > import Test.QuickCheck.Monadic (run, pick, assert)
-- > import Test.WebDriver
-- > prop_Example = monadicWD allCaps $ do
-- >     pick $ oneof ("http://www.yesodweb.com", "http://snapframework.com/")
-- >     run $ openPage url
-- >     assert True
