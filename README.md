# purescript-verbal-expressions

A free monad implementation of [Verbal Expressions](https://github.com/VerbalExpressions/JSVerbalExpressions) for [PureScript](https://github.com/purescript/purescript).

## Examples
Using `do` notation to construct Verbal Expressions:
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

Using Functor/Applicative notation (note that special characters are properly escaped for us):
``` purs
> let pattern = find "[" *> anythingBut "]" *> find "]"
> replace pattern "***" "Censor [all!!] things [inside(42)] brackets"
"Censor *** things *** brackets"
```

For more examples, see the [tests](test/Main.purs).
