-- PFL 2023/24 - Haskell practical assignment quickstart

import Data.List

import Lexer
import Parser

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data Values = IntVal Integer | BoolVal Bool deriving Show
type Stack = [Values]
 
type State = [(String, Values)]

-- Stack and State functions
createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str (x:[]) = (showVal x)
stack2Str (x:xs) = (showVal x) ++ "," ++ stack2Str xs

showVal :: Values -> String
showVal (IntVal a) = show a
showVal (BoolVal b) = show b

createEmptyState :: State
createEmptyState = [] 

state2Str :: State -> String
state2Str [] = ""
state2Str ((a,b):[]) = a ++ "=" ++ (showVal b)
state2Str ((a,b):xs) = a ++ "=" ++ (showVal b) ++ "," ++ state2Str xs 

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([],a,b) = ([],a,b)

-- Add
run ((Add: xs), (IntVal s1: IntVal s2:sR), state) = run(xs, ((IntVal (s1+s2)):sR), state)
run ((Add: xs), _, _) = error "Add Error: Wrong types"

-- Mult
run ((Mult: xs), (IntVal s1: IntVal s2:sR), state) = run(xs, ((IntVal (s1*s2)):sR), state)
run ((Mult: xs), _, _) = error "Mult Error: Wrong types"

-- Sub
run ((Sub: xs), (IntVal s1: IntVal s2:sR), state) = run(xs, ((IntVal (s1-s2)):sR), state)
run ((Sub: xs), _, _) = error "Sub Error: Wrong types"

-- Eq
run ((Equ: xs), (IntVal s1: IntVal s2:sR), state) = run(xs, ((BoolVal (s1==s2)):sR), state)
run ((Equ: xs), (BoolVal s1: BoolVal s2:sR), state) = run(xs, ((BoolVal (s1==s2)):sR), state)
run ((Equ: xs), _, _) = error "Equ Error: Wrong types"

-- Le
run ((Le: xs), (IntVal s1: IntVal s2:sR), state) = run(xs, ((BoolVal (s1<=s2)):sR), state)
run ((Le: xs), _, _) = error "Le Error: Wrong types"

-- Push
run ((Push a: xs), s, state) = run(xs, ((IntVal a):s), state)

-- Tru
run ((Tru: xs), s, state) = run(xs, ((BoolVal True):s), state)

-- Fals
run ((Fals: xs), s, state) = run(xs, ((BoolVal False):s), state)

-- Neg
run ((Neg: xs), (BoolVal s1:sR), state) = run(xs, ((BoolVal (not s1)):sR), state)
run ((Neg: xs), _, _) = error "Neg Error: Wrong types"

-- And
run ((And: xs), (BoolVal s1: BoolVal s2:sR), state) = run(xs, ((BoolVal (s1 && s2)):sR), state)
run ((And: xs), _, _) = error "And Error: Wrong types"

-- Noop
run ((Noop: xs), s, state) = run(xs, s, state)

-- Fetch
run ((Fetch a:xs), stack, state) = run (xs, ((case (find (\(x,_) -> x == a) state) of 
                                                Just val -> snd val
                                                Nothing -> error ("Can't fetch variable: " ++ a)
                                              ):stack), state)

-- Store
run ((Store a: xs), (IntVal s:sR), state) = run (xs, sR, ((a, IntVal s):[(x,y) | (x,y) <- state, x /= a]))
run ((Store a: xs), (BoolVal s:sR), state) = run (xs, sR, ((a, BoolVal s):[(x,y) | (x,y) <- state, x /= a]))

-- Branch
run ((Branch c1 c2:xs), (BoolVal s:sR), state) | s == True = run ((c1 ++ xs), sR, state)
                                               | s == False = run ((c2 ++ xs), sR, state)
run ((Branch c1 c2:xs), (IntVal s:sR), state) = error "Can't branch on non-bool"

-- Loop
run ((Loop c1 c2:xs), stack, state) = run (((c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]]) ++ xs), stack, state)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
--PASSES-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
--PASSES-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
--PASSES-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
--PASSES-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
--PASSES-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")

-- Part 2

compA :: Aexp -> Code
compA expr = case expr of
  IntLit a -> [Push a]
  IntVar a -> [Fetch a]
  AddExp a b -> (compA a) ++ (compA b) ++ [Add]
  SubExp a b -> (compA b) ++ (compA a) ++ [Sub]
  MultExp a b -> (compA a) ++ (compA b) ++ [Mult]

compB :: Bexp -> Code
compB expr = case expr of
  BoolLit a -> if a == True then [Tru] else [Fals]
  IntEquals a b -> (compA a) ++ (compA b) ++ [Equ]
  BoolEquals a b -> (compB a) ++ (compB b) ++ [Equ]
  LessEquals a b -> (compA b) ++ (compA a) ++ [Le]
  AndExp a b -> (compB a) ++ (compB b) ++ [And]
  NotExp a -> (compB a) ++ [Neg]

compile :: [Stm] -> Code
compile [] = []
compile (x:xs) = case x of
  IntAttribution a b -> (compA b) ++ [Store a] ++ (compile xs)
  BoolAttribution a b -> (compB b) ++ [Store a] ++ (compile xs)
  While a b -> [Loop (compB a) (compile [b])] ++ (compile xs)
  IfElse a b (Just c) -> (compB a) ++ [Branch (compile [b]) (compile [c])] ++ (compile xs)
  IfElse a b Nothing -> (compB a) ++ [Branch (compile [b]) [Noop]] ++ (compile xs)

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")