module Parser where

import Lexer

-- Data defining an arithmetic expression
data Aexp
  = IntLit Integer
  | IntVar String
  | AddExp Aexp Aexp
  | SubExp Aexp Aexp
  | MultExp Aexp Aexp
  deriving Show


-- Data defining a boolean expression
data Bexp
  = BoolLit Bool
  | IntEquals Aexp Aexp
  | BoolEquals Bexp Bexp
  | LessEquals Aexp Aexp
  | AndExp Bexp Bexp
  | NotExp Bexp
  deriving Show


-- Data defining a statement
data Stm
  = Attribution String Aexp
  | While Bexp [Stm]
  | IfElse Bexp [Stm] (Maybe [Stm])
  | Aexp Aexp
  | Bexp Bexp
  deriving Show

-- <afactor> ::= <var> | <num> | "(" <aexpr> ")" | "-" <afactor>
parseAFactor :: [Token] -> Maybe(Aexp, [Token])
-- <var>
parseAFactor (VarTok var : restTokens) = Just(IntVar var, restTokens)

-- <num>
parseAFactor (IntTok num : restTokens) = Just(IntLit num, restTokens)

-- "(" <aexpr> ")"
parseAFactor (OpenTok : restTokens) = case parseAExpr restTokens of
  Just(aexpr, CloseTok : restTokens) -> Just(aexpr, restTokens)
  _ -> Nothing

-- "-" <afactor>
parseAFactor (MinusTok : restTokens) = case parseAFactor restTokens of
  Just(afactor, restTokens) -> Just(MultExp (IntLit (-1)) afactor, restTokens)
  _ -> Nothing

-- No afactor
parseAFactor _ = Nothing


--  <aterm> ::= <afactor> { "*" <aterm> }
parseATerm :: [Token] -> Maybe(Aexp, [Token])
parseATerm tokens = case parseAFactor tokens of
    Just (afactor1, (TimesTok : restTokens)) -> case parseATerm restTokens of   -- "*" <aterm>
        Just (afactor2, restTokens) -> Just (MultExp afactor1 afactor2, restTokens)
        _ -> Nothing
    Just (afactor, restTokens) -> Just(afactor, restTokens)
    _ -> Nothing


-- <aexpr> ::= <aterm> { ("+" | "-") <aexpr>}
parseAExpr :: [Token] -> Maybe(Aexp, [Token])
parseAExpr tokens = case parseATerm tokens of
    Just (aterm1, (PlusTok : restTokens)) -> case parseAExpr restTokens of      -- "+" <aexpr>
        Just (aterm2, restTokens) -> Just (AddExp aterm1 aterm2, restTokens)
        _ -> Nothing
    Just (aterm1, (MinusTok : restTokens)) -> case parseAExpr restTokens of     -- "-" <aexpr>
        Just (aterm2, restTokens) -> Just (SubExp aterm1 aterm2, restTokens)
        _ -> Nothing
    Just (aterm, restTokens) -> Just(aterm, restTokens)
    _ -> Nothing


-- <bfactor> ::= "(" <bexpr> ")" | <aexpr> "<=" <aexpr> | <aexpr> "==" <aexpr> | "not" <bfactor>  | "True" | "False"
parseBFactor :: [Token] -> Maybe(Bexp, [Token])
-- "(" <bexpr> ")"
parseBFactor (OpenTok : restTokens) = case parseBExpr restTokens of
    Just (bexpr, CloseTok : restTokens) -> Just (bexpr, restTokens)
    _ -> Nothing

-- "not" <bfactor>
parseBFactor (NotTok : restTokens) = case parseBFactor restTokens of
    Just(bfactor, restTokens) -> Just(NotExp bfactor, restTokens)
    _ -> Nothing

-- "True" | "False"
parseBFactor (BoolTok bool : restTokens) = Just (BoolLit bool, restTokens)

-- <aexpr> "<=" <aexpr> | <aexpr> "==" <aexpr>
parseBFactor tokens = case parseAExpr tokens of
    Just (aexpr1, LeTok : restTokens) -> case parseAExpr restTokens of       -- "<=" <aexpr>
        Just (aexpr2, restTokens) -> Just (LessEquals aexpr1 aexpr2, restTokens)
        _ -> Nothing
    Just (aexpr1, IntEqTok : restTokens) -> case parseAExpr restTokens of   -- "==" <aexpr>
        Just (aexpr2, restTokens) -> Just (IntEquals aexpr1 aexpr2, restTokens)
        _ -> Nothing
    _ -> Nothing


-- <bterm> ::= <bfactor> { "=" <bterm> }
parseBTerm :: [Token] -> Maybe(Bexp, [Token])
parseBTerm tokens = case parseBFactor tokens of
    Just (bfactor1, BoolEqTok : restTokens) -> case parseBTerm restTokens of    -- "=" <bterm>
        Just (bfactor2, restTokens) -> Just (BoolEquals bfactor1 bfactor2, restTokens)
        _ -> Nothing
    Just (bfactor, restTokens) -> Just(bfactor, restTokens)
    _ -> Nothing


-- <bexpr> ::= <bterm> { "and" <bexpr> }
parseBExpr :: [Token] -> Maybe(Bexp, [Token])
parseBExpr tokens = case parseBTerm tokens of
    Just (bterm1, AndTok : restTokens) -> case parseBExpr restTokens of       -- "and" <bexpr>
        Just (bterm2, restTokens) -> Just (AndExp bterm1 bterm2, restTokens)
        _ -> Nothing
    Just (bterm, restTokens) -> Just(bterm, restTokens)
    _ -> Nothing


-- <attribution> ::= <var> ":=" (<aexpr> | <bexpr>)
parseAttribution :: [Token] -> Maybe(Stm, [Token])
parseAttribution (VarTok var : AttrTok : restTokens) = case parseAExpr restTokens of
    Just (aexpr, EndTok : restTokens) -> Just (Attribution var aexpr, restTokens)
    _ -> Nothing

-- No attribution
parseAttribution _ = Nothing


-- <ifelse> ::= "if" <bexpr> "then" <stm> ["else" <stm>]
parseIf :: [Token] -> Maybe(Stm, [Token])
-- Parentheses around boolean expression
parseIf (IfTok : OpenTok : restTokens) = case parseBExpr restTokens of
    Just (bexpr, CloseTok : ThenTok : restTokens) -> parseThen restTokens bexpr
    _ -> Nothing

-- No parentheses around boolean expression
parseIf (IfTok : restTokens) = case parseBExpr restTokens of
    Just (bexpr, ThenTok : restTokens) -> parseThen restTokens bexpr
    _ -> Nothing

-- No if statement
parseIf _ = Nothing


parseThen :: [Token] -> Bexp -> Maybe(Stm, [Token])
-- Has parentheses -> multiple statements possible
parseThen (OpenTok : restTokens) bexpr = case parseStmGroup restTokens of
    -- Semicolon after closing parentheses means no else statement
    Just (stm, CloseTok : EndTok : restTokens) -> Just (IfElse bexpr stm Nothing, restTokens)
    Just (stm, CloseTok : ElseTok : restTokens) -> parseElse (ElseTok:restTokens) bexpr stm
    _ -> Nothing

-- No parentheses -> only one statement
parseThen restTokens bexpr = case parseStm restTokens of
    Just (stm, restTokens) -> parseElse restTokens bexpr [stm]
    _ -> Nothing


parseElse :: [Token] -> Bexp -> [Stm] -> Maybe(Stm, [Token])
-- Has parentheses -> multiple statements possible
parseElse (ElseTok : OpenTok : restTokens) bexpr stm1 = case parseStmGroup restTokens of
    Just (stm2, CloseTok : EndTok : restTokens) -> Just (IfElse bexpr stm1 (Just stm2), restTokens)
    _ -> Nothing

-- No parentheses -> only one statement
parseElse (ElseTok : restTokens) bexpr stm1 = case parseStm restTokens of
    Just (stm2, restTokens) -> Just (IfElse bexpr stm1 (Just [stm2]), restTokens)
    _ -> Nothing

-- No else statement
parseElse restTokens bexpr stm1 = Just (IfElse bexpr stm1 Nothing, restTokens)


-- <while> ::= "while" <bexpr> "do" <stm>
parseWhile :: [Token] -> Maybe(Stm, [Token])
-- Parentheses around boolean expression
parseWhile (WhileTok : OpenTok : restTokens) = case parseBExpr restTokens of
    Just (bexpr, CloseTok : DoTok : restTokens) -> parseDo restTokens bexpr
    _ -> Nothing

-- No parentheses around boolean expression
parseWhile (WhileTok : restTokens) = case parseBExpr restTokens of
    Just (bexpr, DoTok : restTokens) -> parseDo restTokens bexpr
    _ -> Nothing

parseWhile _ = Nothing

parseDo :: [Token] -> Bexp -> Maybe(Stm, [Token])
-- Has parentheses -> multiple statements possible
parseDo (OpenTok : restTokens) bexpr = case parseStmGroup restTokens of
    Just (stm, CloseTok : EndTok : restTokens) -> Just (While bexpr stm, restTokens)
    _ -> Nothing

-- No parentheses -> only one statement
parseDo restTokens bexpr = case parseStm restTokens of
    Just (stm, restTokens) -> Just (While bexpr [stm], restTokens)
    _ -> Nothing


-- <stm> ::= <attribution> | <ifelse> | <while> | <aexpr> | <bexpr>
parseStm :: [Token] -> Maybe(Stm, [Token])
parseStm tokens = case parseAttribution tokens of
    Just (attribution, restTokens) -> Just (attribution, restTokens)
    _ -> case parseIf tokens of
        Just (ifelse, restTokens) -> Just (ifelse, restTokens)
        _ -> case parseWhile tokens of
            Just (while, restTokens) -> Just (while, restTokens)
            _ -> case parseAExpr tokens of
                Just (aexpr, EndTok : restTokens) -> Just (Aexp aexpr, restTokens)
                _ -> case parseBExpr tokens of
                    Just (bexpr, EndTok : restTokens) -> Just (Bexp bexpr, restTokens)
                    _ -> Nothing


-- Parse multiple statements until failure
----------------------------------------------------------------------------
-- If used for parsing ifelse and while, failure will usually be a closing parentheses. 
-- In this case the program will continue, as it is the intended behaviour.
-- In the calling function, if a closing parentheses is not found, the program will fail (there is a syntax error).
parseStmGroup :: [Token] -> Maybe([Stm], [Token])
parseStmGroup tokens = case parseStm tokens of
    Just (stm, restTokens) -> case parseStmGroup restTokens of
        Just (stms, restTokens) -> Just (stm : stms, restTokens)
        _ -> Just ([stm], restTokens)
    _ -> Nothing

-- Parses a list of tokens, statement by statement, until there is nothing left or an error occurs
parseProgram :: [Token] -> [Stm]
parseProgram tokens = case parseStm tokens of
    Just (stm, []) -> [stm]
    Just (stm, restTokens) -> stm : parseProgram restTokens
    Nothing -> error "Parsing Error"