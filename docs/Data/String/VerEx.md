## Module Data.String.VerEx

This module contains a free monad implementation of
[Verbal Expressions](https://github.com/VerbalExpressions/JSVerbalExpressions).
for PureScript.

#### `CaptureGroup`

``` purescript
newtype CaptureGroup
```

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

A monadic action that constructs a Verbal Expression.

#### `VerExReplace`

``` purescript
type VerExReplace = VerExM String
```

A monadic action that constructs a Verbal Expression and returns a
replacement string.

#### `VerExMatch`

``` purescript
type VerExMatch = VerExM (Array CaptureGroup)
```

A monadic action that constructs a Verbal Expression and returns an
array of capture group indices.

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

#### `possiblyV`

``` purescript
possiblyV :: forall a. VerExM a -> VerExM a
```

Like `possibly`, but works on a sub-VerEx.

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

#### `some`

``` purescript
some :: VerEx -> VerExM Unit
```

Repeat the inner expression one or more times.

#### `many`

``` purescript
many :: VerEx -> VerExM Unit
```

Repeat the inner expression zero or more times.

#### `exactly`

``` purescript
exactly :: Int -> VerEx -> VerExM Unit
```

Repeat the inner expression exactly the given number of times.

#### `lineBreak`

``` purescript
lineBreak :: VerExM Unit
```

Add universal line break expression.

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

#### `digit`

``` purescript
digit :: VerExM Unit
```

Adds an expression to match a single digit.

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

#### `capture`

``` purescript
capture :: VerEx -> VerExM CaptureGroup
```

Add a new capture group which matches the given VerEx. Returns the index
of the capture group.

#### `findAgain`

``` purescript
findAgain :: CaptureGroup -> VerExM Unit
```

Match a previous capture group again (back reference).

#### `replaceWith`

``` purescript
replaceWith :: String -> VerExReplace
```

Replace the matched string with the given replacement.

#### `insert`

``` purescript
insert :: CaptureGroup -> String
```

Add the contents of a given capture group in the replacement string.

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
replace :: VerExReplace -> String -> String
```

Replace occurences of the `VerEx` with the `String` that is returned by
the monadic action.

#### `match`

``` purescript
match :: VerExMatch -> String -> Maybe (Array (Maybe String))
```

Match the `VerEx` against the string argument and (maybe) return an Array
of possible results from the specified capture groups.


