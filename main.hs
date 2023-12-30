import Data.List
import Lexer
import Parser

-- Part 1

-- Datas and Types --------------------------------------------------------------------------------
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data Values = IntVal Integer | BoolVal Bool deriving Show
type Stack = [Values]
 
type State = [(String, Values)]

-- Stack and State functions ----------------------------------------------------------------------

-- Creates an empty stack
createEmptyStack :: Stack
createEmptyStack = []


-- Creates an empty state
createEmptyState :: State
createEmptyState = [] 


-- Prints the Values to the terminal
showVal :: Values -> String
showVal (IntVal a) = show a
showVal (BoolVal b) = show b


-- Prints the stack as a string to the terminal
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str (x:[]) = (showVal x)
stack2Str (x:xs) = (showVal x) ++ "," ++ stack2Str xs


-- Prints the state as a string to the terminal
state2Str :: State -> String
state2Str [] = ""
state2Str ((a,b):[]) = a ++ "=" ++ (showVal b)
state2Str ((a,b):xs) = a ++ "=" ++ (showVal b) ++ "," ++ state2Str xs 


-- Run function -----------------------------------------------------------------------------------
run :: (Code, Stack, State) -> (Code, Stack, State)
-- Base case, when the code is empty
run ([],a,b) = ([],a,b)

-- Add instruction
-- pops two values from the stack, adds them and pushes the result
run ((Add: xs), (IntVal s1: IntVal s2:sR), state) = run(xs, ((IntVal (s1+s2)):sR), state)
run ((Add: xs), _, _) = error "Run-time error"


-- Mult instruction
-- pops two values from the stack, multiplies them and pushes the result
run ((Mult: xs), (IntVal s1: IntVal s2:sR), state) = run(xs, ((IntVal (s1*s2)):sR), state)
run ((Mult: xs), _, _) = error "Run-time error"


-- Sub instruction
-- pops two values from the stack, subtracts the top value to the one below it and pushes the result
run ((Sub: xs), (IntVal s1: IntVal s2:sR), state) = run(xs, ((IntVal (s1-s2)):sR), state)
run ((Sub: xs), _, _) = error "Run-time error"


-- Eq instruction
-- pops two values from the stack, checks if they are equal and pushes the result
run ((Equ: xs), (IntVal s1: IntVal s2:sR), state) = run(xs, ((BoolVal (s1==s2)):sR), state)
run ((Equ: xs), (BoolVal s1: BoolVal s2:sR), state) = run(xs, ((BoolVal (s1==s2)):sR), state)
run ((Equ: xs), _, _) = error "Run-time error"


-- Le instruction
-- pops two values from the stack, checks if the top value is less or equal to the one below it and pushes the result
run ((Le: xs), (IntVal s1: IntVal s2:sR), state) = run(xs, ((BoolVal (s1<=s2)):sR), state)
run ((Le: xs), _, _) = error "Run-time error"


-- Push instruction
-- pushes an integer value to the stack
run ((Push a: xs), s, state) = run(xs, ((IntVal a):s), state)


-- Tru instruction
-- pushes a boolean value True to the stack
run ((Tru: xs), s, state) = run(xs, ((BoolVal True):s), state)


-- Fals instruction
-- pushes a boolean value False to the stack
run ((Fals: xs), s, state) = run(xs, ((BoolVal False):s), state)


-- Neg instruction
-- pops a boolean value from the stack, negates it and pushes the result
run ((Neg: xs), (BoolVal s1:sR), state) = run(xs, ((BoolVal (not s1)):sR), state)
run ((Neg: xs), _, _) = error "Run-time error"


-- And instruction
-- pops two boolean values from the stack, performs the And operation and pushes the result
run ((And: xs), (BoolVal s1: BoolVal s2:sR), state) = run(xs, ((BoolVal (s1 && s2)):sR), state)
run ((And: xs), _, _) = error "Run-time error"


-- Noop instruction
-- does nothing
run ((Noop: xs), s, state) = run(xs, s, state)


-- Fetch instruction
-- searches for a string given as argument in the state and pushes the value associated with it on the stack
run ((Fetch a:xs), stack, state) = run (xs, ((case (find (\(x,_) -> x == a) state) of 
                                                Just val -> snd val
                                                Nothing -> error "Run-time error"
                                              ):stack), state)


-- Store instruction
-- pops a value from the stack and stores it in the state with the string given as argument
run ((Store a: xs), (IntVal s:sR), state) = run (xs, sR, (sortOn fst ((a, IntVal s):[(x,y) | (x,y) <- state, x /= a])))
run ((Store a: xs), (BoolVal s:sR), state) = run (xs, sR, (sortOn fst ((a, BoolVal s):[(x,y) | (x,y) <- state, x /= a])))


-- Branch instruction
-- pops a boolean value from the stack and executes the first code if it's True, the second one if it's False
run ((Branch c1 c2:xs), (BoolVal s:sR), state) | s == True = run ((c1 ++ xs), sR, state)
                                               | s == False = run ((c2 ++ xs), sR, state)
run ((Branch c1 c2:xs), (IntVal s:sR), state) = error "Run-time error"


-- Loop instruction
-- pops a boolean value from the stack and executes the code if it's True, stops if it's False
-- repeats the code until the boolean value is False
run ((Loop c1 c2:xs), stack, state) = run (((c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]]) ++ xs), stack, state)
---------------------------------------------------------------------------------------------------

-- Part 2

-- Compiling functions ----------------------------------------------------------------------------

-- Compiles an arithmetic expression into code
compA :: Aexp -> Code
compA expr = case expr of
  IntLit a -> [Push a]
  IntVar a -> [Fetch a]
  AddExp a b -> (compA a) ++ (compA b) ++ [Add]
  SubExp a b -> (compA b) ++ (compA a) ++ [Sub]
  MultExp a b -> (compA a) ++ (compA b) ++ [Mult]


-- Compiles a boolean expression into code
compB :: Bexp -> Code
compB expr = case expr of
  BoolLit a -> if a == True then [Tru] else [Fals]
  IntEquals a b -> (compA a) ++ (compA b) ++ [Equ]
  BoolEquals a b -> (compB a) ++ (compB b) ++ [Equ]
  LessEquals a b -> (compA b) ++ (compA a) ++ [Le]
  AndExp a b -> (compB a) ++ (compB b) ++ [And]
  NotExp a -> (compB a) ++ [Neg]


-- Compiles a list of statements into code
compile :: [Stm] -> Code
compile [] = []
compile (x:xs) = case x of
  Aexp a -> compA a ++ (compile xs)
  Bexp a -> compB a ++ (compile xs)
  Attribution a b -> (compA b) ++ [Store a] ++ (compile xs)
  While a b -> [Loop (compB a) (compile b)] ++ (compile xs)
  IfElse a b (Just c) -> (compB a) ++ [Branch (compile b) (compile c)] ++ (compile xs)
  IfElse a b Nothing -> (compB a) ++ [Branch (compile b) [Noop]] ++ (compile xs)


-- Main parsing function --------------------------------------------------------------------------

-- Parses a string into a list of statements
parse :: String -> [Stm]
parse program = parseProgram (lexer program)


-- Program execution functions --------------------------------------------------------------------

-- Executes a string as a program
execute :: String -> (String, String)
execute programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)


-- Reads a file and executes it as a program
executeFile :: String -> IO ()
executeFile fileName = do
  programCode <- readFile fileName
  putStrLn (show (execute programCode))



-- TESTING FUNCTIONS ------------------------------------------------------------------------------  
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

--PASSES-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
--PASSES-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
--PASSES-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
--PASSES-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
--PASSES-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
--PASSES-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
--PASSES-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
--PASSES-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
--PASSES-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")



testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

--PASSES-- testParser "x := 5; x := x - 1;" == ("","x=4")
--PASSES-- testParser "x := 0 - 2;" == ("","x=-2")
--PASSES-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
--PASSES-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
--PASSES-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
--PASSES-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
--PASSES-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
--PASSES-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
--PASSES-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
--PASSES-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
--PASSES-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
--PASSES-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")