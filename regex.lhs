> module Regex (parseRegex, parse, take1, match, split) where
> import qualified Data.Map as Map
> import qualified Data.Set as Set

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

We store with each instruction the location in the original regular expression string from where it came.  This allows us to check for equality when traversing the graph.

> type Automaton = [Instruction]
> type Pos = Integer
> data Instruction =
> 	Take Pos Token
> 	| Split Pos Automaton Automaton
>	deriving (Eq)

> instance Show Instruction where
>  show (Take n x) = "Take@" ++ show n ++ " " ++ show x
>  show (Split n _ _) = "Split@" ++ show n

parseRegex takes a regular expression string and parses it, producing a Haskell
data structure which represents the NFA that consumes such a regex.

> parseRegex :: String -> Either Loc Automaton
> parseRegex = p 0 ([], Nil)
>   where
>     p n (ton, Nil) [] = Right (reverse ton)
>     p n (_, loc) []   = Left loc
>     p n state (x:xs)  = p (n+1) (parse n state x) xs

The Parse (a,b) type contains all of the state for the ongoing parsing operation.
a) the automaton being operated upon
b) a Loc object which holds the state of the parsing process

The state can be
1) on the left side of an alternation (Lft Parse) in which case the Parse 
object from before we entered the alternation is held in the Lft object

2) on the right side (Rgt Automaton Parse)
and we hold the automaton that was generated on the left side of the alternation
and the parent Parse object

c) not in an alternation (Nil)

> type Parse = (Automaton, Loc)
> data Loc = Lft Parse | Rgt Automaton Parse | Nil deriving Show

> parse						:: Pos -> Parse -> Char -> Parse
> parse n state '('				= ([], Lft state)
> parse n (left, Lft parent) '|'		= ([], Rgt (reverse left) parent)
> parse n (ton, Rgt left (pton, ploc)) ')'	= ((Split n left (reverse ton)):pton, ploc)
> parse n (inst:ton, loc) '*'			= ((kleene n inst):ton, loc)
> parse n (inst:ton, loc) '?'			= ((question n inst):ton, loc)
> parse n (inst:ton, loc) '+'			= ((plus n inst) ++ ton, loc)
> parse n (ton, loc) c				= ((Take n c):ton, loc)

> instance Ord Instruction where
>  compare x y = compare (pos x) (pos y)

The kleene (star) automaton is a choice:
a) execute the underlying automaton and then go back to the starting state
b) an Epsilon move

> kleene :: Pos -> Instruction -> Instruction
> kleene n ton = let ret = Split n (ton:[ret]) [] in ret

> question :: Pos -> Instruction -> Instruction
> question n ton = Split n [] [ton]

> plus :: Pos -> Instruction -> Automaton
> plus n inst = let ret = inst:[Split n [] ret] in (reverse ret) -- return the reverse, it will be reversed again later

> match :: Automaton -> String -> Bool
> match a = run (split a)

> run :: [Automaton] -> String -> Bool
> run tons "" = any null tons
> run tons (x:xs) = run (concatMap (take1 x) tons) xs

> take1 :: Char -> Automaton -> [Automaton]
> take1 c ((:) (Take _ x) ton)
>   | x == c = split ton
>   | otherwise = []

split traverses an automaton, _splitting_ all Split nodes into two separate automata. Where the splitted node also contained a Split node, split continues recursively into the child nodes. The result is a list of automata x such that (head x) is a Take node.

split must be careful not to follow loops that have no Take nodes. Such loops exist in regular expressions such as "(a|)*", because the Kleene closure of the alternation creates a loop over no Take nodes.  It is unclear whether this kind of regex has any useful purpose, but split simply treats the loops as Epsilon transitions to the successor automaton.  split achieves this by comparing against a list (inefficient) of nodes that is has been to since it last saw a Take node.

> split :: Automaton -> [Automaton]
> split ton = s [] ton
>   where
>     s pos ((:) (Split p a b) ton)
>       | not (elem p pos) = (s (p:pos) (a ++ ton)) ++ (s (p:pos) (b ++ ton))
>       | otherwise        = []
>     s _ ton = [ton]

> type DFAPos = (Bool, [Pos])

> pos (Take p c) = p
> pos (Split p a b) = p

> dfaNode ton = foldl info (False, []) (split ton)
>   where
>     info :: DFAPos -> Automaton -> DFAPos
>     info (_ , p) [] = (True, p)
>     info (accept, p) ton = (accept, (pos $ head ton) : p)

> next :: Automaton -> (Token, [Automaton])
> next ((:) (Take _ c) ton) = (c, split ton)

succ 0 

dfaEdges :: [Automaton] -> ((Pos, Token), [Pos])
dfaEdges ton = edges 0 (split ton)
  where
    edges :: ((Pos, Token), [Pos]) -> (Pos,Token) -> ((Pos, Token), [Pos])
    edges known current ton
      | not (elem 
dfaEdge :: [Automaton]

> dfaEdges ton :: Automaton -> Map [Pos] [(Token, [Pos])]
>
> ify :: Map.Map [Pos] [(Token, Automaton)] -> [Automaton] -> Map.Map [Pos] [(Token, [Pos])]
> ify map ton = Map.map (\x -> (Set.fold addpos [] x, Set.toList x)) $ Map.fromListWith (Set.union) $ map (transform . next) ton
>   where
>     mapper = transform . next
>
>     transform :: (Token, [Automaton]) -> (Token, Set.Set Automaton)
>     transform (c, tons) = (c, Set.fromList tons)
>
>     -- folds the position of an automaton in with a list
>     addpos :: Automaton -> [Pos] -> [Pos]
>     addpos x accum = pos (head x) : accum 
>
>     reducer :: ([Pos], [Automaton]) -> ([Pos], [Automaton]) -> ([Pos], [Automaton])
>     reducer (x1, y1) (x2, y2) = (x1 ++ x2, y1 ++ y2)
