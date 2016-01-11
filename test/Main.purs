module Test.Main where

import Prelude
import Control.Apply ((*>))
import Control.Monad.Eff.Console (log)
import Data.Maybe (Maybe(..))
import Test.Assert (assert)

import Data.String.VerEx

url :: VerExMatch
url = do
  startOfLine
  protocol <- capture do
    find "http"
    possibly "s"
  find "://"
  domain <- capture do
    possibly "www."
    anythingBut " "
  endOfLine

  return [protocol, domain]

main = do
  log "URL example"
  let isUrl = test (void url)
  assert $ isUrl "https://www.google.com"
  assert $ isUrl "http://google.com"
  assert $ isUrl "http://google.com"
  assert $ not $ isUrl "http://google com"
  assert $ not $ isUrl "ftp://google com"

  log "startOfLine"
  let vStartOfLine = startOfLine *> find "a"
  assert $ test vStartOfLine "a"
  assert $ not $ test vStartOfLine "ba"

  log "endOfLine"
  let vEndOfLine = find "a" *> endOfLine
  assert $ test vEndOfLine "a"
  assert $ not $ test vEndOfLine "ab"

  log "find"
  assert $ test (find "a" *> find "b") "ab"

  log "Special characters"
  assert $ test (find "^)(.$[") "^)(.$["

  log "possibly"
  let vPossibly = startOfLine *> find "a" *> possibly "..." *> find "b"
  assert $ test vPossibly "ab"
  assert $ test vPossibly "a...b"

  log "possiblyV"
  let vPossiblyV = do
        find "a"
        possiblyV do
          find "("
          some (find "bc")
          find ")"
        find "d"
  assert $ test vPossiblyV "ad"
  assert $ test vPossiblyV "a(bc)d"
  assert $ test vPossiblyV "a(bcbcbcbc)d"
  assert $ not $ test vPossiblyV "a()d"
  assert $ not $ test vPossiblyV "abcd"

  log "anything"
  assert $ test anything "$(#!"
  assert $ test anything ""

  log "anythingBut"
  let vAnythingBut = startOfLine *> anythingBut "a" *> endOfLine
  assert $ test vAnythingBut "b"
  assert $ test vAnythingBut ""
  assert $ not $ test vAnythingBut "a"

  log "something"
  assert $ test something "$(#!"
  assert $ not $ test something ""

  log "anyOf"
  let vAnyOf = startOfLine *> find "a" *> anyOf "xyz"
  assert $ test vAnyOf "ax"
  assert $ test vAnyOf "az"
  assert $ not $ test vAnyOf "ab"

  log "some"
  let vSome = startOfLine *> some (anyOf ".[]") *> endOfLine
  assert $ test vSome "."
  assert $ test vSome "["
  assert $ test vSome "[..]..]"
  assert $ not $ test vSome "..a.."
  assert $ not $ test vSome ""

  log "many"
  let vMany = startOfLine *> many whitespace *> endOfLine
  assert $ test vMany " "
  assert $ test vMany "      "
  assert $ test vMany "   \t \t"
  assert $ test vMany ""

  log "lineBreak"
  let vLineBreak = startOfLine *> find "abc" *> lineBreak *> find "def"
  assert $ test vLineBreak "abc\ndef"
  assert $ test vLineBreak "abc\r\ndef"
  assert $ not $ test vLineBreak "abc\nghi"

  log "tab"
  assert $ test (find "a" *> tab *> find "b") "a\tb"

  log "word"
  assert $ test (word *> whitespace *> word) "Hello World"

  log "digit"
  assert $ test (find "(" *> some digit *> find ")") "(0123456789)"
  let isNumber = test do
        startOfLine
        possiblyV (anyOf "+-")
        some digit
        possiblyV do
          find "."
          some digit
        endOfLine
  assert $ isNumber "1"
  assert $ isNumber "42"
  assert $ isNumber "+42"
  assert $ isNumber "-42"
  assert $ isNumber "42.123"
  assert $ isNumber "-42.123"
  assert $ not (isNumber "a")
  assert $ not (isNumber ".123")
  assert $ not (isNumber "0.")

  log "whitespace"
  assert $ test (find "a" *> some whitespace *> find "b") "a \n \t   b"

  log "withAnyCase"
  assert $ not $ test (find "foo") "Foo"
  assert $ test (withAnyCase *> find "foo") "Foo"
  assert $ test (withAnyCase *> find "foo") "FOO"

  log "capture"
  let vCapture = do
        firstWord <- capture word
        whitespace
        capture word
        whitespace
        findAgain firstWord
  assert $ test vCapture "foo bar foo"
  assert $ not $ test vCapture "foo bar baz"

  log "replace"
  let verexReplace = do
        first  <- capture word
        blank  <- capture (some whitespace)
        second <- capture word
        replaceWith (insert second <> insert blank <> insert first)
  assert $ replace verexReplace "Foo   Bar" == "Bar   Foo"

  log "match"
  assert $ match url "https://google.com" == Just [Just "https", Just "google.com"]
  assert $ match url "ftp://google.com" == Nothing

  let date = do
        startOfLine
        year <- capture (exactly 4 digit)
        find "-"
        month <- capture (exactly 2 digit)
        find "-"
        day <- capture (exactly 2 digit)
        endOfLine
        return [year, month, day]

  assert $ match date "2016-01-11" == Just [Just "2016", Just "01", Just "11"]
  assert $ match date "2016-1-11" == Nothing

  let matchNumber = match do
        startOfLine
        intPart <- capture (some digit)
        floatPart <- possiblyV do
          find "."
          capture (some digit)
        endOfLine

        return [intPart, floatPart]

  assert $ matchNumber "3.14" == Just [Just "3", Just "14"]
  assert $ matchNumber "42" == Just [Just "42", Nothing]

  let matchNested = match do
        a <- capture digit
        find ","
        inner <- capture do
          void $ capture digit
        find ","
        b <- capture digit
        return [a, inner, b]

  assert $ matchNested "1,2,3" == Just [Just "1", Just "2", Just "3"]
