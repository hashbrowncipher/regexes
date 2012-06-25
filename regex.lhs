>{-# LANGUAGE ViewPatterns #-}
>-- module Regex (parse) where
> import qualified Data.Map as Map
> import qualified Data.Set as Set
> import Control.Monad

We represent a regular expression as an automaton, roughly following Russ Cox's
"Regular Expression Matching, the Virtual Machine Approach" [1].  Each
automaton is based recursively on its successor automaton.

The (Take c a) automaton (known as "char" in [1]) is an automaton that consumes
a single character c from the input string, and then continues with automaton
a.  The (Split x y) automaton does nothing to the input string, but is followed
by two automata x and y.  The Match automaton indicates that the regular
expression matches, iff the string has been fully consumed.

> data Automaton = 
>   Take Char Automaton
>   | Split Automaton Automaton
>   | Match
>   deriving (Show, Eq)

We will build an automaton piecemeal by parsing the input regex from left to
right.  When we parse a plain token -- the letter 'a', for instance -- we will
know to add (Take 'a') to our automaton, but we will not yet have the successor
automaton.  Therefore we define a type Active to represent a partial Automaton
that need to be completed by a successor automaton.

> type Active = Automaton -> Automaton

Regular expressions can contain alternations.  When we encounter an
alternation, we begin work on a new automaton, saving the old state.  Because
alternations can be nested, this creates the possibility of a tree of states.

> data Loc = -- a location in the tree of states
>       Lft State -- the left side of an alternation
>       | Rgt [Active] State -- the right side of an alternation
>       | Top -- not in an alternation; the root of the tree

> data Thing =
>       F { a :: String }
>       | G { a :: String, b :: String }

A state is the tuple of a Loc in the tree of states, and a list of Actives that
will compose the final automatons

> type State = (Loc, [Active])

Some tokens are 

> type Token = (Char, Bool)


We will consider five basic operations for 

There are three basic operations involved in regular expression matching.
* ab (concatenation)
* a|b (alternation)
* a* (Kleene star)

We temporarily assume that tokens are just Chars


The Parse (a,b) type contains all of the state for the ongoing parsing operation.
a) the automaton being operated upon
b) a Loc object which holds the state of the parsing process

The state can be
1) on the left side of an alternation (Lft Parse) in which case the Parse 
object from before we entered the alternation is held in the Lft object

2) on the right side (Rgt Automaton Parse)
and we hold the automaton that was generated on the left side of the alternation
and the parent Parse object

c) not in an alternation (Top)

> parse :: String -> Automaton 
> parse xs = case foldl go (Top, []) (escape xs) of (Top, a) -> coalesce a Match
>          where
>   escape :: [Char] -> [Token]
>   escape [] = []
>   escape ('\\':x:xs) = (x, True) : (escape xs)
>   escape (x:xs) = (x, False) : (escape xs)
>
>   go :: State -> Token ->  State
>   go state ('(', False)                           = (Lft state, [])
>   go (Lft parent, left) ('|', False)              = (Rgt left parent, [])
>   go (Rgt left (ploc, pton), right) (')',False)   = (ploc, (csplit left right) : pton)
>   go (loc, t:xt) ('*', False)                     = (loc,  (kleene t) : xt)
>   go (loc, t:xt) ('?', False)                     = (loc,  (question t) : xt)
>   go (loc, t:xt) ('+', False)                     = (loc,  (plus t) : xt)
>   go (loc, ton) (char, _)                         = (loc,  (Take char) : ton)

> split :: Active -> Active -> Active
> split = liftM2 Split

   | lm && rm = Match
   | lm = MSplit rf
   | rm = MSplit lf
   | otherwise = Split lf rf
   where
   isTerminal (Match) = True
   isTerminal (MSplit a) = True
   isTerminal _ = False
   lf = left f
   lm = isTerminal lf
   rf = right f
   rm = isTerminal rf

> coalesce :: [Active] -> Active
> coalesce = flip (foldl (flip ($)))
> c = coalesce

> csplit left right = split (c left) (c right)

The question modifier is a choice:
a) go to the following state (equivalent to executing id)
b) 

> question :: Active -> Active
> question = split id

The kleene (star) modifier is a choice:
a) go to the following state
b) execute the underlying automaton and then go back to the starting state

> kleene :: Active -> Active
> kleene t = let ret = question (c [ret, t]) in ret

> plus :: Active -> Active 
> plus t = c [kleene t, t] 

> run :: Automaton -> String -> Bool
> run Match xs = xs == []
> run (Take c xt) (x:xs)
>   | x == c = run xt xs
>   | otherwise = False
> run (Split a b) xs = (run a xs) || (run b xs)
> run _ [] = False


[1] http://swtch.com/~rsc/regexp/regexp2.html
