{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module System.Process.BetterSpec (main, spec) where

import           Test.Hspec

import           System.Process.Better

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "readProcess" $ do
    it "can be used with RawCommand" $ do
      readProcess (RawCommand "echo" ["foo"]) "" `shouldReturn` "foo\n"

    it "can be used with ShellCommand" $ do
      readProcess (ShellCommand "echo foo") "" `shouldReturn` "foo\n"

    it "can be used with a String as command" $ do
      readProcess "echo foo" "" `shouldReturn` "foo\n"
