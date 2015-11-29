{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HEphem.NGCParserSpec where

import           HEphem.NGCParser
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = describe "NGCObject parser" $ do

  describe "readFloatWithComma" $ do
    it "reads back 1,5 as 1.5" $
      readMaybeFloatWithComma "1,5" `shouldBe` Just 1.5
    it "reads back X as Nothing" $
      readMaybeFloatWithComma "X" `shouldBe` Nothing

  it "can preparse a line" $
    length (readRecord (last . take 3 $ lines ngcobjecttext)) `shouldBe` 35;

  describe "NGC data file" $
    it "has allways 35 colums" $ property $
      and [length (readRecord x) == 35 | x <- lines ngcobjecttext]
