> module Regex (parseRegex, parse, reverse) where

We represent a regular expression as a finite state automaton.  Initially, this is a NFA.

We will consider five basic operations for 

There are three basic operations involved in regular expression matching.
* ab (concatenation)
* a|b (alternation)
* a* (Kleene star)

We temporarily assume that tokens are just Chars

> type Token = Char

type Parse = (Automaton,[Token])

An action consumes a token (Just x) or nothing (Nothing), and leads to a new state
An automaton matches a regular expression. 

> data Automaton =
> 	Take Token | 
> 	Split Automaton Automaton |
>	Cons Automaton Automaton |
> 	Epsilon
>	deriving Show

parseRegex takes a regular expression string and parses it, producing a Haskell
data structure which represents the NFA that consumes such a regex.

> parseRegex :: String -> Either Loc Automaton
> parseRegex xs = case foldl parse (Epsilon, Nil) xs of
>	(ton, Nil)	-> Right (invert ton)
>	(_, loc)	-> Left loc

The Parse (a,b) type contains all of the state for the ongoing parsing operation.
a) During the parsing process there is always an automaton being operated upon.
b) The alternation operator causes the regex parsing operation to be stateful.

The state can be
1) on the left side of an alternation (Lft Parse)
in which case the Parse object from before we entered the alternation is held in the Lft object

2) on the right side (Rgt Automaton Parse)
and we hold the automaton that was generated on the left side of the alternation
and the parent Parse object

c) not in an alternation (Nil)

> type Parse = (Automaton, Loc)
> data Loc = Lft Parse | Rgt Automaton Parse | Nil deriving Show

> parse						:: Parse -> Char -> Parse
> parse (left, Lft parent) '|'			= (Epsilon, Rgt (invert left) parent)
> parse (ton, Rgt left (pton, ploc)) ')'	= (Cons pton (Split left (invert ton)), ploc)
> parse state '('				= (Epsilon, Lft state)
> parse (ton, loc) '*'				= (kleene (invert ton), loc)
> parse (ton, loc) c				= (Cons ton (Take c), loc)

> invert :: Automaton -> Automaton
> invert xs = inv xs Epsilon
>   where
>     inv (Cons a b) accum = inv a (Cons b accum)
>     inv Epsilon accum = accum
>     inv a accum = Cons a accum

> car (Cons a b) = a
> cdr (Cons a b) = b
> f (Split a b) = a
> s (Split a b) = b

The star automaton is a choice:
a) execute the underlying automaton and then go back to the starting state
b) an Epsilon move

> kleene :: Automaton -> Automaton
> kleene ton = 
>            let ret = Split 
>                    (Cons ton ret)
>                    Epsilon
>            in ret

