module LexerSpec where

import Lexer
import Parsec
import Test.Hspec

spec :: Spec
spec = do
  describe "tokens" $ do
    it "1" $
      let actual = run tokens "(th(is is (a test) \"We must have spaces and ()\" (brackets)) and numbers 1423\nand (this) is row number 2"
          expected =
            Right
              ( [ LeftBracket,
                  Symbol "th",
                  LeftBracket,
                  Symbol "is",
                  Symbol "is",
                  LeftBracket,
                  Symbol "a",
                  Symbol "test",
                  RightBracket,
                  String "We must have spaces and ()",
                  LeftBracket,
                  Symbol "brackets",
                  RightBracket,
                  RightBracket,
                  Symbol "and",
                  Symbol "numbers",
                  Number "1423",
                  Symbol "and",
                  LeftBracket,
                  Symbol "this",
                  RightBracket,
                  Symbol "is",
                  Symbol "row",
                  Symbol "number",
                  Number "2"
                ],
                ""
              )
       in actual `shouldBe` expected