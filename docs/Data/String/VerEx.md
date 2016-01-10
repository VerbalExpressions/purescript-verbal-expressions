## Module Data.String.VerEx

This module contains a PureScript implementation of
[Verbal Expressions](https://github.com/VerbalExpressions/JSVerbalExpressions).

#### `VerExF`

``` purescript
data VerExF a
```

The grammar for Verbal Expressions, used internally.

#### `VerExM`

``` purescript
type VerExM = Free VerExF
```

The free monad over the `VerExF` type constructor.

#### `VerEx`

``` purescript
type VerEx = VerExM Unit
```

A Verbal Expression.

#### `startOfLine'`

``` purescript
startOfLine' :: Boolean -> VerExM Unit
```

Set whether or not the expression has to start at the beginning of the
line. Default: `false`.

#### `startOfLine`

``` purescript
startOfLine :: VerExM Unit
```

Mark the expression to start at the beginning of the line.

#### `endOfLine'`

``` purescript
endOfLine' :: Boolean -> VerExM Unit
```

Set whether or not the expression has to end at the end of the line.
Default: `false`.

#### `endOfLine`

``` purescript
endOfLine :: VerExM Unit
```

Mark the expression to end at the end of the line.

#### `find`

``` purescript
find :: String -> VerExM Unit
```

Add a string to the expression.

#### `possibly`

``` purescript
possibly :: String -> VerExM Unit
```

Add a string to the expression that might appear once (or not).
This combinator is called `maybe` in the original API.

#### `anything`

``` purescript
anything :: VerExM Unit
```

Match any charcter, any number of times.

#### `anythingBut`

``` purescript
anythingBut :: String -> VerExM Unit
```

Match anything but the specified characters.

#### `something`

``` purescript
something :: VerExM Unit
```

Match any charcter, at least one time.

#### `anyOf`

``` purescript
anyOf :: String -> VerExM Unit
```

Any of the given characters.

#### `lineBreak`

``` purescript
lineBreak :: VerExM Unit
```

Add universal line break expression.

#### `br`

``` purescript
br :: VerExM Unit
```

Shorthand for `lineBreak`.

#### `tab`

``` purescript
tab :: VerExM Unit
```

Add expression to match a tab character.

#### `word`

``` purescript
word :: VerExM Unit
```

Adds an expression to match a word.

#### `whitespace`

``` purescript
whitespace :: VerExM Unit
```

Any whitespace character

#### `withAnyCase`

``` purescript
withAnyCase :: VerExM Unit
```

Enable case-insensitive matching

#### `toRegex`

``` purescript
toRegex :: VerEx -> Regex
```

Convert a Verbal Expression to a Regular Expression.

#### `test`

``` purescript
test :: VerEx -> String -> Boolean
```

Check whether a given `String` matches the Verbal Expression.

#### `replace`

``` purescript
replace :: VerEx -> String -> String -> String
```

Replace occurences of the `VerEx` with the given replacement.


