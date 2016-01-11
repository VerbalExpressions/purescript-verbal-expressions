# purescript-verbal-expressions

A free monad implementation of [Verbal Expressions](https://github.com/VerbalExpressions/JSVerbalExpressions) for [PureScript](https://github.com/purescript/purescript).

## Examples

### Using do-notation to construct verbal expressions
``` purs
url :: VerEx
url = do
  startOfLine
  find "http"
  possibly "s"
  find "://"
  possibly "www."
  anythingBut " "
  endOfLine

> test url "https://www.google.com"
true
```

### Capture groups and back references
The monadic interface allows us to bind the indices of capture groups to named expressions:
``` purs
pattern :: VerEx
pattern = do
  firstWord <- capture word
  whitespace
  capture word
  whitespace
  findAgain firstWord
```
This VerEx matches "foo bar *foo*" but not "foo bar *baz*".

### Replacing with named groups
Here, we use the result of the monadic action to return a replacement string with 'named' capture groups:
``` purs
swap :: VerExReplace
swap = do
  first <- capture word
  whitespace
  second <- capture word
  replaceWith (insert second <> " " <> insert first)

> replaceM swap "Foo Bar"
"Bar Foo"
```

### Using Applicative notation
Note that special characters like `[` and `]` are properly escaped for us:
``` purs
> let pattern = find "[" *> anythingBut "]" *> find "]"
> replace pattern "---" "Censor [all!!] things [inside(42)] brackets"
"Censor --- things --- brackets"
```

For more examples, see the [tests](test/Main.purs).

## Installation
```
bower install purescript-verbal-expressions
```
