module Test.Main where

import Prelude
import Control.Apply ((*>))
import Control.Monad.Eff.Console (log)
import Test.Assert (assert)

import Data.String.VerEx

url :: VerEx
url = do
  startOfLine
  find "http"
  possibly "s"
  find "://"
  possibly "www."
  anythingBut " "
  endOfLine

main = do
  log "URL example"
  assert $ test url "https://www.google.com"
  assert $ test url "http://google.com"
  assert $ test url "http://google.com"
  assert $ not $ test url "http://google com"
  assert $ not $ test url "ftp://google com"

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
        some digit
        possiblyV do
          find "."
          some digit
        endOfLine
  assert $ isNumber "1"
  assert $ isNumber "42"
  assert $ isNumber "42.123"
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
  let inBrackets = find "(" *> anythingBut ")" *> find ")"
  assert $ (== "Start (...) Middle (...) End") $
    replace inBrackets "(...)" "Start (everything in here) Middle (another) End"

  log "replaceM"
  let verexReplace = do
        first  <- capture word
        blank  <- capture (some whitespace)
        second <- capture word
        replaceWith (insert second <> insert blank <> insert first)
  assert $ replaceM verexReplace "Foo   Bar" == "Bar   Foo"
