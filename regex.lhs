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
>   | Split Int [Automaton]
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

A state is the tuple of a Loc in the tree of states, and a list of Actives that
will compose the final automatons

Some tokens are 

> data Token = 
>   Raw Char
>   | Op Char
    


We will consider five basic operations for 

There are three basic operations involved in regular expression matching.
* ab (concatenation)
* a|b (alternation)
* a* (Kleene star)

We temporarily assume that tokens are just Chars

> type StateView = ([Active], [Active]) -- siblings, current
> type State = [StateView]

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

> compile :: String -> Automaton 
> compile xs = case foldl count (0, [([], [])]) (lex xs) of (_, [([], ton)]) -> coalesce ton Match
>          where
>   lex :: [Char] -> [Token]
>   lex [] = []
>   lex ('\\':x:xs) = (Raw x) : (lex xs)
>   lex (x:xs) = (Op x) : (lex xs)
>
>   count :: (Int, State) -> Token -> (Int, State)
>   count (n, s) t = (n + 1, go n s t)
>
>   go :: Int -> State -> Token ->  State
>   go n p (Op '(')                                 = ([], []):p
>   go n ((ss, c):p) (Op '|')                       = ((coalesce c):ss, []):p
>   go n ((ss, c):(pss, pc):gp) (Op ')')            = ((split n prev ((coalesce c):ss)):pss, pc):gp
>   go n (p, ss, c:tc) (Op '*')                     = (ss, (kleene n c):tc):p
>   go n (p, ss, c:tc) (Op '?')                     = (ss, (question n c):tc):p
>   go n (p, ss, c:tc) (Op '+')                     = (ss, (plus n c):tc):p
>   go n state (Op t)                               = go n state (Raw t)
>   go n (p, ss, c:tc) (Raw t)                      = (ss, (Take t):tc):p

> split :: Int -> [Int] -> [Active] -> Active
> split n prev ts f = Split n (map f (filter (not . isLoop) ts)) where
>   isLoop t@(Split d _) = elem d prev --it's a loop if we are going somewhere in prev
>   isLoop t = False

> coalesce :: [Active] -> Active
> coalesce = flip (foldl (flip ($)))

The question modifier is a choice:
a) go to the following state (equivalent to executing id)
b) 

> question :: (Int, [Int]) -> Active -> Active
> question n prev = split n prev . (:[id])

The kleene (star) modifier is a choice:
a) go to the following state
b) execute the underlying automaton and then go back to the starting state

> kleene :: Int -> [Int] -> Active -> Active
> kleene n prev t = let ret = question n prev (coalesce [ret, t]) in ret

> plus :: Int -> [Int] -> Active -> Active 
> plus n prev t = coalesce [kleene n prev t, t] 

 run :: Automaton -> String -> Bool
 run Match xs = xs == []
 run (Take c xt) (x:xs)
   | x == c = run xt xs
   | otherwise = False
 run (Split a b) xs = (run a xs) || (run b xs)
 run _ [] = False


[1] http://swtch.com/~rsc/regexp/regexp2.html
