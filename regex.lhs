We represent a regular expression as a finite state automaton.  Initially, this is a NFA.

We will consider five basic operations for 

There are three basic operations involved in regular expression matching.
* ab (concatenation)
* a|b (alternation)
* a* (Kleene star)

We temporarily assume that tokens are just Chars

> type Token = Char
> type Parse = (Automaton,[Token])

An action consumes a token (Just x) or nothing (Nothing), and leads to a new state
An automaton matches a regular expression. 

> data Automaton =
> 	Take Token | 
> 	Split Automaton Automaton |
>	Cons Automaton Automaton |
> 	Epsilon
>	deriving Show

> s :: Int -> Automaton -> String
> s n a = replicate n '|' ++ s1 n a

> s1 n (Split a b) = "Split\n" ++ s (n + 1) a ++ "\n" ++ s (n + 1) b
> s1 n (Cons a b) = "Cons\n" ++ s (n + 1) a ++ "\n" ++ s (n + 1) b
> s1 n a = show a

The star automaton is a choice:
a) execute the underlying automaton and then go back to the starting state
b) an Epsilon move

> kleene :: Automaton -> Automaton
> kleene ton = 
>            let ret = Split 
>                    (Cons ton ret)
>                    Epsilon
>            in ret

> char :: Automaton -> Char -> Automaton
> char ton x = Cons ton (Take x)

reg :: Automaton -> String -> Automaton
reg ton []       = ton
reg ton ('(':xs) = let (s, rem) = left (Epsilon, xs) in reg (Cons ton s) rem
reg ton (x:xs)   = reg (char ton x) xs

right :: Parse -> Parse
right (ton, (')':xs)) = (ton, xs)
right (ton, ('(':xs)) = let (s, rem) = left (Epsilon, xs) in right ((Cons ton s), rem)
right (ton, (x:xs))   = right (char ton x, xs)

left :: Parse -> Parse
left (ton, ('|':xs)) = let (r, rem) = right (Epsilon, xs) in (Split ton r, rem)
left (ton, ('(':xs)) = let (s, rem) = left (Epsilon, xs) in left (Cons ton s, rem)
left (ton, (x:xs))   = left ((char ton x), xs)

> data Direction = Lft | Rgt

> parse :: [Direction] -> Parse -> Parse
> parse [] ret@(ton,[]) = ret
> parse (Lft:state) (ton, ('|':xs)) = let (r, rem) = parse (Rgt:state) (Epsilon, xs) in (Split ton r, rem)
> parse (Rgt:state) (ton, (')':xs)) = (ton, xs)
> parse state (ton, ('(':xs)) = let (s, rem) = parse (Lft:state) (Epsilon, xs) in parse state ((Cons ton s), rem)
> parse state ((Cons ton x), ('*':xs)) = ((Cons ton $ kleene x), xs)
> parse state (ton, (x:xs)) = parse state (Cons ton (Take x), xs)

data Tree a = Nil | Leaf a | Fork (Tree a) (Tree a) deriving Show
data Direction = Lft | Rgt | Fwd
data Context a = Top | L (Context a) (Tree a) | R (Tree a) (Context a) deriving Show
type Loc a = (Tree a, Context a)

left :: Loc a -> Loc a
left (Fork l r, c) = (l, L c r)

right :: Loc a -> Loc a
right (Fork l r, c) = (r, R l c)

up :: Loc a -> Loc a
up (l, L c r) = (Fork l r, c)
up (r, R l c) = (Fork l r, c)

fwd :: Loc a -> Loc a
fwd (

parseRegex :: String -> Tree
parseRegex 

go :: Loc Char -> [Char] -> Loc Char
go l ('(':xs) = go (
go l ('|':xs) = go (right $ up l) xs
go l (')':xs) = go (
{-

"(ab|ba)*"
"ab|ba|aa"

CHAR a
CHAR b
OR
CHAR b
CHAR a
OR
CHAR a
CHAR a

       OR
CHAR a        OR
CHAR b  CHAR b  CHAR a
        CHAR a  CHAR a


parseRegex string = parse [] string

parse :: String -> (Tree Char, String)
parse ast ('|':xs) = 
parse ast ('(':xs) = 
parse ast (')':xs) = 
parse ast (x:xs) = 

-}
