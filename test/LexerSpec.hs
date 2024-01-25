module LexerSpec where

import Lexer
import Parsec
import Test.Hspec

spec :: Spec
spec = do
  describe "test" $ do
    it "1" $
      let actual = run tokens "(this is (a test) \"We must have spaces and ()\" (brackets))"
          expected =
            Right
              ( [ LeftBracket,
                  Symbol "this",
                  Symbol "is",
                  LeftBracket,
                  Symbol "a",
                  Symbol "test",
                  RightBracket,
                  String "We must have spaces and ()",
                  LeftBracket,
                  Symbol "brackets",
                  RightBracket,
                  RightBracket
                ],
                ""
              )
       in actual `shouldBe` expected