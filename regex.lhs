>{-# LANGUAGE ViewPatterns #-}
>-- module Regex (parse) where
> import qualified Data.Set as Set
> import Control.Monad

We represent a regular expression as an automaton, roughly following Russ Cox's
"Regular Expression Matching, the Virtual Machine Approach" [1].  Each
automaton is based recursively on its successor automat(a).

The `Take n c t` automaton (known as "char" in [1]) is an automaton that
consumes a single character c from the input string, and then continues with
automaton a.  The `Split n {ts}` automaton does nothing with the input string,
but is followed by the automata in {ts}.  `Take` and `Split` are both labeled
with the position in the regexp that catalyzed the node's generation.  This is
used to disambiguate the nodes. The `Match` automaton indicates that the
regular expression matches, iff the string has been fully consumed.

[1] http://swtch.com/~rsc/regexp/regexp2.html

> data Automaton =
>   Take Int Char Automaton
>   | Split Int (Set.Set Automaton)
>   | Match

> index (Split a _) = a
> index (Take a _ _) = a

We will be storing `Set`s of automata.  These typeclasses are required by the
`Set` module.

> instance Eq Automaton where
>   (==) Match Match  = True
>   (==) Match _       = False
>   (==) _ Match       = False
>   (==) a b            = index a == index b
>
> instance Ord Automaton where
>   (<=) Match _       = True
>   (<=) _ Match       = False
>   (<=) a b            = (<=) (index a) (index b)

We will build an automaton piecemeal by parsing the input regex from left to
right.  When we parse a plain token -- the letter 'a', for instance -- we will
know to add (Take 'a') to our automaton, but we will not yet have the successor
automaton.  Therefore we define a type Active to represent a partial Automaton
that need to be completed by a successor automaton.

Regular expressions can contain alternations.  When we encounter an
alternation, we begin work on a new automaton, saving the old state.  Because
alternations can be nested, this creates the possibility of a tree of states.

Some tokens are 

> data Token = 
>   Raw Char
>   | Op Char

> type PreActive = [Int] -> Automaton
> type Active = PreActive -> PreActive

We will consider five basic operations for 

There are three basic operations involved in regular expression matching.
* ab (concatenation)
* a|b (alternation)
* a* (Kleene star)

We temporarily assume that tokens are just Chars

> type State = [([Active], [Active])]

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
> compile xs = case foldl parse start (lex xs) of (_, [([], ton)]) -> (coalesce ton) (const Match) []
>          where
>   lex :: [Char] -> [Token]
>   lex [] = []
>   lex ('\\':x:xs) = (Raw x) : (lex xs)
>   lex (x:xs) = (Op x) : (lex xs)
>
>   start = (0, [([], [])])
>
>   parse :: (Int, State) -> Token -> (Int, State)
>   parse (n, p) t = (n + 1, go p t) where
>       sibling ((ss, c):p)             = ((coalesce c):ss, []):p
>       finish ((ss, []):(pss, pc):gp)  = (pss, (mksplit n ss):pc):gp
>
>       go s (Op '(')                   = ([], []):s
>       go s (Op '|')                   = sibling s
>       go s (Op ')')                   = finish $ sibling s
>       go ((ss, c:cs):p) (Op '*')      = (ss, (kleene n c):cs):p
>       go ((ss, c:cs):p) (Op '?')      = (ss, (question n c):cs):p
>       go ((ss, c:cs):p) (Op '+')      = (ss, (plus n c):cs):p
>       go s (Op t)                     = go s (Raw t)
>       go ((ss, c):p) (Raw t)          = (ss, (mktake n t):c):p

> coalesce :: [Active] -> Active
> coalesce as f = foldr ($) f (reverse as)

`mksplit` is a wrapper of the `Split` summand defined above.  It performs
a conversion to a DFA before returning the final `Split` node.

If we have been asked to create a node with index `n` that is in the list of
previous ε-moves `prev`, we have followed a loop of ε-moves.  Instead of
continuing the loop ad infinitum, simply return a dummy node.

Otherwise, compute the successor node, converting to a DFA as we go.

> mksplit :: Int -> [Active] -> Active
> mksplit n as f prev
>   | elem n prev = Split 0 Set.empty
>   | otherwise = ret
>   where

    We've gotten a list of `Active`s in.  To make them `Automaton`s, we supply a
    context by providing a successor function and a list of ε-moves we've made.

>   contextualize   :: Active -> Automaton
>   contextualize a = a f (n:prev)

    As we go, we will create a massive graph of `Split`s which represent the
    NFA. To convert to a DFA, we need to "collapse" this graph to get all of the
    successor automata which are reachable using only ε-moves from a given node.
    `collapse` achieves this goal by returning the `Set` of all automata that can
    be reached using ε-moves when provided an automaton *a*. It makes the
    assumption that `collapse` has already been called on *a*'s successors.

    `collapse` has the ancillary benefit of removing the `Split 0 Set.empty` nodes
    inserted when `mksplit` detects a cycle.

>   collapse                :: Automaton -> Set.Set Automaton
>   collapse (Split n as)   = as
>   collapse a              = Set.singleton a

    Compute our successor nodes.  We union the result to weed out duplicates.

>   successors  = Set.unions $ map (collapse . contextualize) as

    Once we know our successor nodes, we will need to the results into a new Split,
    with one exception.  If there is only one node in the set of successors, return
    it on its own.

>   wrap s
>       | Set.size s == 1 = Set.findMin s
>       | otherwise = Split n s

>   ret         = wrap successors

> mktake :: Int -> Char -> Active
> mktake n t f prev = Take n t (f [])

The question modifier is a choice:
a) go to the following state (equivalent to executing id)
b) execute the underlying automaton and then go to the successor

> question :: Int -> Active -> Active
> question n = mksplit n . (:[id])

The kleene (star) modifier is a choice:
a) go to the following state
b) execute the underlying automaton and then go back to the starting state

> kleene :: Int -> Active -> Active
> kleene n t = let ret = question n (t . ret) in ret

> plus :: Int -> Active -> Active 
> plus n t = t . (kleene n t)

> run :: String -> Automaton -> Bool
> run [] Match              = True
> run xs (Split n as)       = any (run xs) (Set.toList as)
> run (x:xs) (Take n c f)
>   | x == c                = run xs f
>   | otherwise             = False
> run _ _                   = False

> expand 0 _                = Split (-1) Set.empty
> expand n Match            = Match
> expand n (Take i c f)     = Take i c (expand (n-1) f)
> expand n (Split i as)     = Split i $ Set.map (expand (n-1)) as

[1] http://swtch.com/~rsc/regexp/regexp2.html
