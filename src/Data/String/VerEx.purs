-- | This module contains a free monad implementation of
-- | [Verbal Expressions](https://github.com/VerbalExpressions/JSVerbalExpressions).
-- | for PureScript.
module Data.String.VerEx
  ( VerExF()
  , VerExM()
  , VerEx()
  , VerExReplace()
  , VerExMatch()
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
  , exactly
  , lineBreak
  , tab
  , word
  , digit
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
  , match
  ) where

import Prelude hiding (add)

import Control.Apply ((<*), (*>))
import Control.Monad.Free (Free(), liftF, foldFree)
import Control.Monad.State (State(), modify, get, put, runState)

import Data.Array (index)
import Data.Functor ((<$))
import Data.Maybe (Maybe())
import Data.String.Regex as R
import Data.Tuple (fst, snd)

newtype CaptureGroup = CaptureGroup Int

-- | The grammar for Verbal Expressions, used internally.
data VerExF a
  = Add String a
  | StartOfLine Boolean a
  | EndOfLine Boolean a
  | AddFlags String a
  | AddSubexpression (VerExM a) (a -> a)
  | Capture VerEx (CaptureGroup -> a)

-- | The free monad over the `VerExF` type constructor.
type VerExM = Free VerExF

-- | A monadic action that constructs a Verbal Expression.
type VerEx = VerExM Unit

-- | A monadic action that constructs a Verbal Expression and returns a
-- | replacement string.
type VerExReplace = VerExM String

-- | A monadic action that constructs a Verbal Expression and returns an
-- | array of capture group indices.
type VerExMatch = VerExM (Array CaptureGroup)

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
addSubexpression :: forall a. VerExM a -> VerExM a
addSubexpression inner = liftF $ AddSubexpression inner id

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
possiblyV :: forall a. VerExM a -> VerExM a
possiblyV sub = addSubexpression sub <* add "?"

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

-- | Repeat the inner expression exactly the given number of times.
exactly :: Int -> VerEx -> VerExM Unit
exactly n pattern = addSubexpression pattern *> add ("{" <> show n <> "}")

-- | Add universal line break expression.
lineBreak :: VerExM Unit
lineBreak = add "(?:(?:\\n)|(?:\\r\\n))"

-- | Add expression to match a tab character.
tab :: VerExM Unit
tab = add "(?:\\t)"

-- | Adds an expression to match a word.
word :: VerExM Unit
word = add "(?:\\w+)"

-- | Adds an expression to match a single digit.
digit :: VerExM Unit
digit = add "\\d"

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
toVerExState (Add str a) = a <$
  modify (\s -> s { pattern = s.pattern <> str })
toVerExState (StartOfLine flag a) = a <$
  modify (\s -> s { startOfLine = flag })
toVerExState (EndOfLine flag a) = a <$
  modify (\s -> s { endOfLine = flag })
toVerExState (AddFlags flags a) = a <$
  modify (\s -> s { flags = s.flags <> flags })
toVerExState (AddSubexpression inner f) = f <$> do
  s <- get
  let res = toRegex' s.captureGroupIndex inner
  put s { pattern = s.pattern <> "(?:" <> R.source (res.regex) <> ")"
        , captureGroupIndex = res.lastIndex }
  return res.result
toVerExState (Capture inner f) = f <$> do
  s <- get
  let cg = s.captureGroupIndex
      res = toRegex' (cg + 1) inner
  put s { pattern = s.pattern <> "(" <> R.source (res.regex) <> ")"
        , captureGroupIndex = res.lastIndex }
  return (CaptureGroup cg)

-- | Convert a Verbal Expression to a Regular Expression. Also returns the
-- | result of the monadic action and the last capture group index. The first
-- | argument is the first capture group index that should be used.
toRegex' :: forall a. Int -> VerExM a -> { result :: a, regex :: R.Regex, lastIndex :: Int }
toRegex' first verex = { result, regex, lastIndex }
  where
    both = runState (foldFree toVerExState verex) (empty { captureGroupIndex = first })
    result = fst both
    verexS = snd both

    flags = R.parseFlags verexS.flags

    prefix = if verexS.startOfLine then "^" else ""
    suffix = if verexS.endOfLine then "$" else ""

    regex = R.regex (prefix <> verexS.pattern <> suffix) flags
    lastIndex = verexS.captureGroupIndex

-- | Convert a Verbal Expression to a Regular Expression.
toRegex :: VerEx -> R.Regex
toRegex verex = _.regex (toRegex' 1 verex)

-- | Convert the pattern (without the flags) of a VerEx to a `String`.
toString :: VerEx -> String
toString verex = R.source (toRegex verex)

-- | Check whether a given `String` matches the Verbal Expression.
test :: VerEx -> String -> Boolean
test verex = R.test (toRegex verex)

-- | Replace occurences of the `VerEx` with the `String` that is returned by
-- | the monadic action.
replace :: VerExReplace -> String -> String
replace verex = R.replace pattern.regex pattern.result
  where pattern = toRegex' 1 (verex <* addFlags "g")

-- | Match the `VerEx` against the string argument and (maybe) return an Array
-- | of possible results from the specified capture groups.
match :: VerExMatch -> String -> Maybe (Array (Maybe String))
match verex str = do
    matches <- maybeMatches
    return (fromIndex matches <$> pattern.result)
  where pattern = toRegex' 1 verex
        maybeMatches = R.match pattern.regex str
        fromIndex matches (CaptureGroup j) = do
          maybeResult <- matches `index` j
          result <- maybeResult
          return result
