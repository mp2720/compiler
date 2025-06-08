{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module LexerSpec (spec) where

import Control.Applicative (Alternative (many))
import Grammar.Combinators (runParser)
import Grammar.Lexer
import Test.Hspec

rl :: Lexer v -> String -> Either Position v
rl lexer s = case runParser lexer (LexerState s (Position 0 1 1) 8) of
  Left state -> Left $ lexerPosition state
  Right (v, _) -> Right v

spec :: Spec
spec =
  describe "Lexer" $ do
    it "identifiers and literals" $ do
      rl ident "abc__123_.b" `shouldBe` Right "abc__123_"
      rl ident "_" `shouldBe` Right "_"
      rl (many literal) "1 00000_0 09 14 0xA1_bAf_e 0b1____100" `shouldBe` Right [1, 0, 9, 14, 0xA1BAFE, 12]
      rl literal "\t \t0o18" `shouldBe` Left (Position 6 1 19)
      rl literal "001a" `shouldBe` Left (Position 3 1 4)
      rl literal "001_" `shouldBe` Left (Position 4 1 5)
      rl literal "0x_01/" `shouldBe` Left (Position 2 1 3)
      rl ident "do" `shouldBe` Left (Position 2 1 3)
    it "identifiers and keywords" $ do
      rl (keyword "if") "if" `shouldBe` Right ()
      rl (keyword "if") "if_" `shouldBe` Left (Position 3 1 4)
      rl ident "if_" `shouldBe` Right "if_"
    it "operators" $ do
      rl (operator ">=") ">=" `shouldBe` Right ()
      rl (operator "+") "+=" `shouldBe` Right ()
      rl
        ( (,,,,)
            <$> ident
            <*> operator "+"
            <*> operator "-"
            <*> operator "-"
            <*> ident
        )
        "a+--b"
        `shouldBe` Right ("a", (), (), (), "b")
      rl (operator "~") "~>" `shouldBe` Left (Position 2 1 3)
    it "comments" $ do
      rl ((,) <$> ident <*> ident) "    a//bc\n//\nd/   //e   " `shouldBe` Right ("a", "d")
      rl
        ( (,)
            <$> ident
            <*> ident
            <* operator "/"
            <* operator "*"
        )
        "a/******//*\n\n* /\n// */b/**// *"
        `shouldBe` Right ("a", "b")
    it "test lines count" $ do
      rl ((,) <$> ident <*> literal) "a\n\r\n " `shouldBe` Left (Position 5 3 1)
