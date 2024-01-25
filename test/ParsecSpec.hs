module ParsecSpec (spec) where

import Parsec
import Test.Hspec

spec :: Spec
spec = do
  describe "oneOf" $ do
    it "1" $
      let actual = run (oneOf "abc") "abc"
          expected = Right ("a", "bc")
       in actual `shouldBe` expected
    it "2" $
      let actual = run (oneOf "xyz") "abc"
          expected = Left ([UnexpectedError], "abc")
       in actual `shouldBe` expected
    it "3" $
      let actual = run (manyOf "abc") "abc123"
          expected = Right ("abc", "123")
       in actual `shouldBe` expected
    it "4" $
      let actual = run (manyOf "abc") "123abc"
          expected = Left ([UnexpectedError], "123abc")
       in actual `shouldBe` expected
  describe "parseC" $ do
    it "1" $
      let actual = run (parseC ('c' ==)) "cab"
          expected = Right ('c', "ab")
       in actual `shouldBe` expected
    it "2" $
      let actual = run (parseC ('c' ==)) "abc"
          expected = Left ([UnexpectedError], "abc")
       in actual `shouldBe` expected
  describe "parse" $ do
    it "1" $
      let actual = run (parse "hejhej") "hejhej12345hgje"
          expected = Right ("hejhej", "12345hgje")
       in actual `shouldBe` expected
    it "2" $
      let actual = run (parse "12345hgje") "hejhej12345hgje"
          expected = Left ([UnexpectedError], "hejhej12345hgje")
       in actual `shouldBe` expected
