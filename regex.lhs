> module Regex (parseRegex, parse, take1, match) where

We represent a regular expression as a finite state automaton.  Initially, this is a NFA.

We will consider five basic operations for 

There are three basic operations involved in regular expression matching.
* ab (concatenation)
* a|b (alternation)
* a* (Kleene star)

We temporarily assume that tokens are just Chars

> type Token = Char

An action consumes a token (Just x) or nothing (Nothing), and leads to a new state
An automaton matches a regular expression.  We represent automatons as lists of instructions.  Each instruction in the list thus has its successor (the next item in the list).

> type Automaton = [Instruction]
> data Instruction =
> 	Take Token
> 	| Split Automaton Automaton
>	deriving (Eq)

> instance Show Instruction where
>  show (Take x) = "Take " ++ show x
>  show (Split _ _) = "Split"

parseRegex takes a regular expression string and parses it, producing a Haskell
data structure which represents the NFA that consumes such a regex.

> parseRegex :: String -> Either Loc Automaton
> parseRegex xs = case foldl parse ([], Nil) xs of
>	(ton, Nil)	-> Right (reverse ton)
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
> parse (left, Lft parent) '|'			= ([], Rgt (reverse left) parent)
> parse (ton, Rgt left (pton, ploc)) ')'	= ((Split left (reverse ton)):pton, ploc)
> parse state '('				= ([], Lft state)
> parse (inst:ton, loc) '*'			= ((kleene inst):ton, loc)
> parse (ton, loc) c				= ((Take c):ton, loc)

The star automaton is a choice:
a) execute the underlying automaton and then go back to the starting state
b) an Epsilon move

> kleene :: Instruction -> Instruction
> kleene ton = let ret = Split (ton:[ret]) [] in ret

Take instructions generate zero (failed to take) or one (took) new automata
Split instructions generate two new automata to test, one for each branch
Match instructions are either true or false

> match :: Automaton -> String -> Bool
> match a = run (split a)

> run :: [Automaton] -> String -> Bool
> run tons "" = any null tons
> run tons (x:xs) = run (concatMap (take1 x) tons) xs

> take1 :: Char -> Automaton -> [Automaton]
> take1 c ((:) (Take x) ton)
>   | x == c = split ton
>   | otherwise = []
> take1 c [] = []
> take1 c a  = error ("this shouldn't happen" ++ show (c, a))

> split :: Automaton -> [Automaton]
> split ((:) (Split a b) ton) = (split (a ++ ton)) ++ [(b ++ ton)]
> split ton = [ton]
