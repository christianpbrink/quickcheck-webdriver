{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}



-- |
-- Module:      Test.QuickCheck.Monadic.WebDriver
-- Copyright:   (c) 2014 Christian Brink
-- License:     MIT
-- Maintainer:  Christian Brink
-- 
-- Example usage:
--
-- > import Test.QuickCheck
-- > import Test.QuickCheck.Monadic (run, pick, assert)
-- > import Test.WebDriver
-- > prop_Example = monadicWD context $ do
-- >     url <- pick $ oneof ("http://www.yesodweb.com", "http://snapframework.com/")
-- >     run $ openPage url
-- >     assert True
-- >     runIO $ putStrLn $ "We've just evaluated a `Gen a` using `pick` and \
-- >                        \a `WD ()` using `run`, then asserted a that a \
-- >                        \property holds using `assert`."
-- >  where context = SessionParams allCaps (return ())

module Test.QuickCheck.Monadic.WebDriver where

import Prelude
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Monadic as QCM
import Test.WebDriver
import Control.Monad.IO.Class

data Context = ExistingSession WDSession 
             | SessionParams Capabilities (WD ())

-- |Patterned after `monadicIO` (and `ioProperty`).
monadicWD context = monadic wdProperty
  where 
    wdProperty = ioProperty . runSesh
    runSesh action = case context of
        ExistingSession sesh -> runWD sesh action
        SessionParams caps setup -> runSession defaultSession caps $ setup >> action
          

runIO :: IO a -> PropertyM WD a
runIO = QCM.run . liftIO

chromeOpts = [ "--log-level=0" ]

chrome' = chrome { chromeOptions = chromeOpts }

caps browser = allCaps {browser=browser} 

capsChrome = caps chrome'
capsFirefox = caps firefox

seshParams browser = SessionParams (caps browser) $ return ()

seshParamsChrome   = seshParams chrome'
seshParamsFirefox' = seshParams firefox

-- $example
--
