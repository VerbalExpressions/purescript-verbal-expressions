-- | This module contains a free monad implementation of
-- | [Verbal Expressions](https://github.com/VerbalExpressions/JSVerbalExpressions).
-- | for PureScript.
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
  , possiblyV
  , anything
  , anythingBut
  , something
  , anyOf
  , some
  , many
  , lineBreak
  , tab
  , word
  , whitespace
  , withAnyCase
  -- Capture groups
  , capture
  , findAgain
  -- Replacements
  , replaceWith
  , insert
  -- Conversion to Regex
  , toRegex
  -- Pattern matching
  , test
  , replace
  , replaceM
  ) where

import Prelude hiding (add)

import Control.Apply ((*>))
import Control.Monad.Free (Free(), liftF, foldFree)
import Control.Monad.State (State(), modify, get, put, runState)

import Data.String.Regex as R
import Data.Tuple (fst, snd)

newtype CaptureGroup = CaptureGroup Int

-- | The grammar for Verbal Expressions, used internally.
data VerExF a
  = Add String a
  | StartOfLine Boolean a
  | EndOfLine Boolean a
  | AddFlags String a
  | AddSubexpression VerEx a
  | Capture VerEx (CaptureGroup -> a)

-- | The free monad over the `VerExF` type constructor.
type VerExM = Free VerExF

-- | A monadic action that constructs a Verbal Expression.
type VerEx = VerExM Unit

-- | A monadic action that constructs a Verbal Expression and returns a
-- | replacement string.
type VerExReplace = VerExM String

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

-- | Append a sub-expression in a non-capturing group
addSubexpression :: VerEx -> VerExM Unit
addSubexpression inner = liftF $ AddSubexpression inner unit

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

-- | Like `possibly`, but works on a sub-VerEx.
possiblyV :: VerEx -> VerExM Unit
possiblyV sub = addSubexpression sub *> add "?"

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

-- | Repeat the inner expression one or more times.
some :: VerEx -> VerExM Unit
some pattern = addSubexpression pattern *> add "+"

-- | Repeat the inner expression zero or more times.
many :: VerEx -> VerExM Unit
many pattern = addSubexpression pattern *> add "*"

-- | Add universal line break expression.
lineBreak :: VerExM Unit
lineBreak = add "(?:(?:\\n)|(?:\\r\\n))"

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

-- | Replace the matched string with the given replacement.
replaceWith :: String -> VerExReplace
replaceWith = return

-- | Add the contents of a given capture group in the replacement string.
insert :: CaptureGroup -> String
insert (CaptureGroup ind) = "$" <> show ind

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
toVerExState (AddSubexpression inner a) = const a <$>
  modify (\s -> s { pattern = s.pattern <> "(?:" <> toString inner <> ")" })
toVerExState (Capture inner f) = f <$> do
  s <- get
  put s { pattern = s.pattern <> "(" <> toString inner <> ")"
        , captureGroupIndex = s.captureGroupIndex + 1 }
  return (CaptureGroup s.captureGroupIndex)

-- | Convert a Verbal Expression to a Regular Expression and return the result
-- | of the monadic action.
toRegex' :: forall a. VerExM a -> { result :: a, regex :: R.Regex }
toRegex' verex = { result, regex }
  where
    both = runState (foldFree toVerExState verex) empty
    result = fst both
    verexS = snd both

    flags = R.parseFlags verexS.flags

    prefix = if verexS.startOfLine then "^" else ""
    suffix = if verexS.endOfLine then "$" else ""

    regex = R.regex (prefix <> verexS.pattern <> suffix) flags

-- | Convert a Verbal Expression to a Regular Expression.
toRegex :: VerEx -> R.Regex
toRegex verex = _.regex (toRegex' verex)

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

-- | Replace occurences of the `VerEx` with the `String` that is returned by
-- | the monadic action.
replaceM :: VerExReplace -> String -> String
replaceM verex = R.replace pattern.regex pattern.result
  where pattern = toRegex' verex
