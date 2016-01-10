-- | This module contains a PureScript implementation of
-- | [Verbal Expressions](https://github.com/VerbalExpressions/JSVerbalExpressions).
module Data.String.VerEx
  ( VerExF()
  , VerExM()
  , VerEx()
  , CaptureGroup()
  -- VerEx combinators
  , startOfLine'
  , startOfLine
  , endOfLine'
  , endOfLine
  , find
  , possibly
  , anything
  , anythingBut
  , something
  , anyOf
  , lineBreak
  , br
  , tab
  , word
  , whitespace
  , withAnyCase
  -- Capture groups
  , capture
  , findAgain
  -- Conversion to Regex
  , toRegex
  -- Pattern matching
  , test
  , replace
  ) where

import Prelude hiding (add)

import Control.Apply ((*>))
import Control.Monad.Free (Free(), liftF, foldFree)
import Control.Monad.State (State(), modify, get, put, execState)

import Data.String.Regex as R

newtype CaptureGroup = CaptureGroup Int

-- | The grammar for Verbal Expressions, used internally.
data VerExF a
  = Add String a
  | StartOfLine Boolean a
  | EndOfLine Boolean a
  | AddFlags String a
  | Capture VerEx (CaptureGroup -> a)

-- | The free monad over the `VerExF` type constructor.
type VerExM = Free VerExF

-- | A Verbal Expression.
type VerEx = VerExM Unit

-- | Set whether or not the expression has to start at the beginning of the
-- | line. Default: `false`.
startOfLine' :: Boolean -> VerExM Unit
startOfLine' flag = liftF $ StartOfLine flag unit

-- | Mark the expression to start at the beginning of the line.
startOfLine :: VerExM Unit
startOfLine = startOfLine' true

-- | Set whether or not the expression has to end at the end of the line.
-- | Default: `false`.
endOfLine' :: Boolean -> VerExM Unit
endOfLine' flag = liftF $ EndOfLine flag unit

-- | Mark the expression to end at the end of the line.
endOfLine :: VerExM Unit
endOfLine = endOfLine' true

-- | Append additional Regex flags, used internally.
addFlags :: String -> VerExM Unit
addFlags flags = liftF $ AddFlags flags unit

-- | Escape special regex characters
escape :: String -> String
escape = R.replace (R.regex "([\\].|*?+(){}^$\\\\:=[])" g) "\\$&"
  where g = R.parseFlags "g"

-- | Internal function to add a pattern to the VerEx.
add :: String -> VerExM Unit
add str = liftF $ Add str unit

-- | Add a string to the expression.
find :: String -> VerExM Unit
find str = add $ "(?:" <> escape str <> ")"

-- | Add a string to the expression that might appear once (or not).
-- | This combinator is called `maybe` in the original API.
possibly :: String -> VerExM Unit
possibly str = add $ "(?:" <> escape str <> ")?"

-- | Match any charcter, any number of times.
anything :: VerExM Unit
anything = add "(?:.*)"

-- | Match anything but the specified characters.
anythingBut :: String -> VerExM Unit
anythingBut str = add $ "(?:[^" <> escape str <> "]*)"

-- | Match any charcter, at least one time.
something :: VerExM Unit
something = add "(?:.+)"

-- | Any of the given characters.
anyOf :: String -> VerExM Unit
anyOf str = add $ "(?:[" <> escape str <> "])"

-- | Add universal line break expression.
lineBreak :: VerExM Unit
lineBreak = add "(?:(?:\\n)|(?:\\r\\n))"

-- | Shorthand for `lineBreak`.
br :: VerExM Unit
br = lineBreak

-- | Add expression to match a tab character.
tab :: VerExM Unit
tab = add "(?:\\t)"

-- | Adds an expression to match a word.
word :: VerExM Unit
word = add "(?:\\w+)"

-- | Any whitespace character
whitespace :: VerExM Unit
whitespace = add "\\s"

-- | Enable case-insensitive matching
withAnyCase :: VerExM Unit
withAnyCase = addFlags "i"

-- | Add a new capture group which matches the given VerEx. Returns the index
-- | of the capture group.
capture :: VerEx -> VerExM CaptureGroup
capture inner = liftF $ Capture inner id

-- | Match a previous capture group again (back reference).
findAgain :: CaptureGroup -> VerExM Unit
findAgain (CaptureGroup ind) = add $ "(?:\\" <> show ind <> ")"

type VerExState =
  { startOfLine :: Boolean
  , endOfLine :: Boolean
  , flags :: String
  , pattern :: String
  , captureGroupIndex :: Int }

empty :: VerExState
empty =
  { startOfLine: false
  , endOfLine: false
  , flags: ""
  , pattern: ""
  , captureGroupIndex: 1 }

-- | Natural transformation from `VerExF` to `State VerExState`.
toVerExState :: forall a. VerExF a -> State VerExState a
toVerExState (Add str a) = const a <$>
  modify (\s -> s { pattern = s.pattern <> str })
toVerExState (StartOfLine flag a) = const a <$>
  modify (\s -> s { startOfLine = flag })
toVerExState (EndOfLine flag a) = const a <$>
  modify (\s -> s { endOfLine = flag })
toVerExState (AddFlags flags a) = const a <$>
  modify (\s -> s { flags = s.flags <> flags })
toVerExState (Capture inner f) = f <$> do
  s <- get
  put s { pattern = s.pattern <> "(" <> toString inner <> ")"
        , captureGroupIndex = s.captureGroupIndex + 1 }
  return (CaptureGroup s.captureGroupIndex)

-- | Convert a Verbal Expression to a Regular Expression.
toRegex :: VerEx -> R.Regex
toRegex verex = R.regex (prefix <> vs.pattern <> suffix) flags
  where
    vs :: VerExState
    vs = execState (foldFree toVerExState verex) empty

    flags = R.parseFlags vs.flags

    prefix = if vs.startOfLine then "^" else ""
    suffix = if vs.endOfLine then "$" else ""

-- | Convert the pattern (without the flags) of a VerEx to a `String`.
toString :: VerEx -> String
toString verex = R.source (toRegex verex)

-- | Check whether a given `String` matches the Verbal Expression.
test :: VerEx -> String -> Boolean
test verex = R.test (toRegex verex)

-- | Replace occurences of the `VerEx` with the first string. The replacement
-- | string can include special replacement patterns escaped with `"$"`
-- | See [reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace).
replace :: VerEx -> String -> String -> String
replace verex = R.replace (toRegex verex')
  where verex' = verex *> addFlags "g"
