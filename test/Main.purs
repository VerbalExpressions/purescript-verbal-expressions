module Test.Main where

import Prelude
import Control.Apply ((*>))
import Data.Maybe (Maybe(..))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Test.Unit as Unit
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Assert (assert, assertFalse, equal)

import Data.String.VerEx (VerEx, VerExMatch, digit, upper, lower, capture,
                          find, match, endOfLine, some, possibly, startOfLine,
                          exactly, replaceWith, anythingBut, replace, insert,
                          word, whitespace, test, findAgain, withAnyCase, tab,
                          lineBreak, many, anyOf, something, anything)

url :: VerExMatch
url = do
  startOfLine
  protocol <- capture do
    find "http"
    possibly $ find "s"
  find "://"
  domain <- capture do
    possibly $ find "www."
    anythingBut " "
  endOfLine

  pure [protocol, domain]

number :: VerEx
number = do
  startOfLine
  possibly (anyOf "+-")
  some digit
  possibly do
    find "."
    some digit
  endOfLine

main :: Eff (console :: CONSOLE, testOutput :: TESTOUTPUT) Unit
main = runTest do
  Unit.test "URL VerEx" do
    let isUrl = test url
    assert "should match valid URL" $ isUrl "https://www.google.com"
    assert "should match valid URL" $ isUrl "http://google.com"
    assert "should match valid URL" $ isUrl "http://google.com"
    assertFalse "should not match invalid URL" $ isUrl "http://google com"
    assertFalse "should not match invalid URL" $ isUrl "ftp://google com"

  Unit.test "startOfLine" do
    let vStartOfLine = startOfLine *> find "a"
    assert "should match 'a' at start of the line" $
      test vStartOfLine "a"
    assertFalse "should not match if no 'a' is at the start of the line" $
      test vStartOfLine "ba"

  Unit.test "endOfLine" do
    let vEndOfLine = find "a" *> endOfLine
    assert "should match 'a' at the end of the line" $
      test vEndOfLine "a"
    assertFalse "should not match if no 'a' is at the end of the line" $
      test vEndOfLine "ab"

  Unit.test "find" do
    assert "should match a and then b" $
      test (find "a" *> find "b") "ab"
    assert "should properly find special characters" $
      test (find "^)(.$[") "^)(.$["

  Unit.test "possibly" do
    let vPossibly = do
          find "a"
          possibly do
            find "("
            some (find "bc")
            find ")"
          find "d"
    assert "should match" $ test vPossibly "ad"
    assert "should match" $ test vPossibly "a(bc)d"
    assert "should match" $ test vPossibly "a(bcbcbcbc)d"
    assertFalse "should not match" $ test vPossibly "a()d"
    assertFalse "should not match" $ test vPossibly "abcd"

  Unit.test "anything" do
    assert "should match any character" $ test anything "$(#!"
    assert "should match empty string" $ test anything ""

  Unit.test "anythingBut" do
    let vAnythingBut = startOfLine *> anythingBut "a" *> endOfLine
    assert "should match anything but an 'a'" $ test vAnythingBut "b"
    assert "should match the empty string" $ test vAnythingBut ""
    assertFalse "should not match an 'a'" $ test vAnythingBut "a"

  Unit.test "something" do
    assert "should match any character" $ test something "$(#!"
    assertFalse "should not match the empty string" $ test something ""

  Unit.test "anyOf" do
    let vAnyOf = startOfLine *> find "a" *> anyOf "xyz"
    assert "should match an x" $ test vAnyOf "ax"
    assert "should match a y" $ test vAnyOf "az"
    assertFalse "should not match a b" $ test vAnyOf "ab"

  Unit.test "some" do
    let vSome = startOfLine *> some (anyOf ".[]") *> endOfLine
    assert "should match a single occurence" $ test vSome "."
    assert "should handle special characters" $ test vSome "["
    assert "should match more than one occurence" $ test vSome "[..]..]"
    assertFalse "should not match the 'a'" $ test vSome "..a.."
    assertFalse "should not match the empty string" $ test vSome ""

  Unit.test "many" do
    let vMany = startOfLine *> many whitespace *> endOfLine
    assert "should match a single occurence" $ test vMany " "
    assert "should match the empty string" $ test vMany ""
    assert "should match many occurences" $ test vMany "      "
    assert "should handle the sub-expression correctly" $ test vMany "   \t \t"

  Unit.test "lineBreak" do
    let vLineBreak = startOfLine *> find "abc" *> lineBreak *> find "def"
    assert "should match unix newlines" $ test vLineBreak "abc\ndef"
    assert "should match windows newlines" $ test vLineBreak "abc\r\ndef"
    assertFalse "should not match other things after the newline" $
      test vLineBreak "abc\nghi"

  Unit.test "tab" do
    assert "should match a tab character" $
      test (find "a" *> tab *> find "b") "a\tb"

  Unit.test "word" do
    assert "should match a whole word" $
      test (word *> whitespace *> word) "Hello World"

  Unit.test "digit" do
    assert "should match any digit" $
      test (find "(" *> some digit *> find ")") "(0123456789)"

  Unit.test "upper" do
    assert "should match uppercase ASCII characters" $
      test (find "(" *> some upper *> find ")") "(ABCDEFGHIJKLMNOPQRSTUVWXYZ)"
    assertFalse "should not match anything else" $
      test upper "42!#a"

  Unit.test "lower" do
    assert "should match lowercase ASCII characters" $
      test (find "(" *> some lower *> find ")") "(abcdefghijklmnopqrstuvwxyz)"
    assertFalse "should not match anything else" $
      test lower "42!#A"

  Unit.test "number VerEx" do
    let isNumber = test number
    assert "should match a single digit" $ isNumber "1"
    assert "should match an integer" $ isNumber "4242"
    assert "should match a signed integer" $ isNumber "+42"
    assert "should match a signed integer" $ isNumber "-42"
    assert "should match a float" $ isNumber "42.123"
    assert "should match a negative float" $ isNumber "-42.123"
    assertFalse "should not match a charater" $ isNumber "a"
    assertFalse "should not match just the float part" $ isNumber ".123"
    assertFalse "should not match a trailing '.'" $ isNumber "0."

  Unit.test "whitespace" do
    assert "should match all whitespace characters" $
      test (find "a" *> some whitespace *> find "b") "a \n \t   b"

  Unit.test "withAnyCase" do
    assertFalse "should be case-sensitive by default" $
      test (find "foo") "Foo"
    assert "should enable case-insensitivity" $
      test (withAnyCase *> find "foo") "Foo"

  Unit.test "capture" do
    let vCapture = do
          firstWord <- capture word
          whitespace
          capture word
          whitespace
          findAgain firstWord
    assert "should match 'foo bar foo'" $
      test vCapture "foo bar foo"
    assertFalse "should not match 'foo bar baz'" $
      test vCapture "foo bar baz"

  Unit.test "replace" do
    let verexReplace = do
          first  <- capture word
          blank  <- capture (some whitespace)
          second <- capture word
          replaceWith (insert second <> insert blank <> insert first)
    equal (replace verexReplace "Foo   Bar")
          "Bar   Foo"

    let censor = replace $ find "[" *> anythingBut "]" *> find "]" *> replaceWith "---"
    equal
      (censor "Censor [all!!] things [inside(42)] brackets")
      "Censor --- things --- brackets"

  Unit.test "match" do
    equal (match url "https://google.com")
          (Just [Just "https", Just "google.com"])
    equal (match url "ftp://google.com")
          Nothing

    let date = do
          startOfLine
          year <- capture do
            possibly (exactly 2 digit)
            exactly 2 digit
          find "-"
          month <- capture (exactly 2 digit)
          find "-"
          day <- capture (exactly 2 digit)
          endOfLine
          pure [year, month, day]

    equal (match date "2016-01-11")
          (Just [Just "2016", Just "01", Just "11"])
    equal (match date "16-01-11")
          (Just [Just "16", Just "01", Just "11"])
    equal (match date "016-01-11")
          Nothing

    let matchNumber = match do
          startOfLine
          intPart <- capture (some digit)
          floatPart <- possibly do
            find "."
            capture (some digit)
          endOfLine

          pure [intPart, floatPart]

    equal (matchNumber "3.14")
          (Just [Just "3", Just "14"])
    equal (matchNumber "42")
          (Just [Just "42", Nothing])
    equal (matchNumber ".3")
          Nothing

    let matchNested = match do
          a <- capture digit
          find ","
          inner <- capture do
            void $ capture digit
          find ","
          b <- capture digit
          pure [a, inner, b]

    equal (matchNested "1,2,3")
          (Just [Just "1", Just "2", Just "3"])
