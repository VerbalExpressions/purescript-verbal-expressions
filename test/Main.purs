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
  let vPossibly = startOfLine *> find "a" *> possibly "b"
  assert $ test vPossibly "abc"
  assert $ test vPossibly "acc"

  log "anything"
  assert $ test anything "a"
  assert $ test anything ""

  log "anythingBut"
  let vAnythingBut = startOfLine *> anythingBut "a" *> endOfLine
  assert $ test vAnythingBut "b"
  assert $ test vAnythingBut ""
  assert $ not $ test vAnythingBut "a"

  log "something"
  assert $ test something "a"
  assert $ not $ test something ""

  log "anyOf"
  let vAnyOf = startOfLine *> find "a" *> anyOf "xyz"
  assert $ test vAnyOf "ax"
  assert $ test vAnyOf "az"
  assert $ not $ test vAnyOf "ab"

  log "lineBreak, br"
  let vLineBreak = startOfLine *> find "abc" *> br *> find "def"
  assert $ test vLineBreak "abc\ndef"
  assert $ test vLineBreak "abc\r\ndef"

  log "tab"
  assert $ test (find "a" *> tab *> find "b") "a\tb"

  log "word"
  assert $ test (word *> whitespace *> word) "Hello World"

  log "whitespace"
  assert $ test (find "a" *> whitespace *> find "b") "a b"

  log "withAnyCase"
  assert $ not $ test (find "a") "A"
  assert $ test (withAnyCase *> find "a") "A"
  assert $ test (withAnyCase *> find "a") "a"

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
        first <- capture word
        whitespace
        second <- capture word
        replaceWith (insert second <> " " <> insert first)
  assert $ replaceM verexReplace "Foo Bar" == "Bar Foo"
