{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HEphem.NGCParserSpec where

import           HEphem.NGCParser
import           Test.Hspec

spec :: Spec
spec = describe "NGCObject parser" $ do

  it "parses the entire file without error" $
    pending;
