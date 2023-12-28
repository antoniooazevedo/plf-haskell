module Parser where

import Lexer

data Aexp
  = IntLit Integer
  | IntVar String
  | AddExp Aexp Aexp
  | SubExp Aexp Aexp
  | MultExp Aexp Aexp
  deriving Show

data Bexp
  = BoolLit Bool
  | BoolVar String
  | IntEquals Aexp Aexp
  | BoolEquals Bexp Bexp
  | LessEquals Aexp Aexp
  | AndExp Bexp Bexp
  | NotExp Bexp
  deriving Show

data Stm
  = IntAttribution String Aexp
  | BoolAttribution String Bexp
  | While Bexp [Stm]
  | IfElse Bexp [Stm] (Maybe [Stm])
  | NoStm
  deriving Show

-- <afactor> ::= <var> | <num> | "(" <aexpr> ")" | "-" <afactor>
parseAFactor :: [Token] -> Maybe(Aexp, [Token])
parseAFactor (VarTok var : restTokens) = Just(IntVar var, restTokens)

parseAFactor (IntTok num : restTokens) = Just(IntLit num, restTokens)

parseAFactor (OpenTok : restTokens) = case parseAExpr restTokens of
  Just(aexpr, CloseTok : restTokens) -> Just(aexpr, restTokens)
  _ -> Nothing

parseAFactor (MinusTok : restTokens) = case parseAFactor restTokens of
  Just(afactor, restTokens) -> Just(MultExp (IntLit (-1)) afactor, restTokens)
  _ -> Nothing

parseAFactor _ = Nothing


--  <aterm> ::= <afactor> { "*" <afactor> }
parseATerm :: [Token] -> Maybe(Aexp, [Token])
parseATerm tokens = case parseAFactor tokens of
    Just (afactor1, (TimesTok : restTokens)) -> case parseAFactor restTokens of
        Just (afactor2, restTokens) -> Just (MultExp afactor1 afactor2, restTokens)
        _ -> Nothing
    Just (afactor, restTokens) -> Just(afactor, restTokens)
    _ -> Nothing


-- <aexpr> ::= <aterm> { ("+" | "-") <aterm>}
parseAExpr :: [Token] -> Maybe(Aexp, [Token])
parseAExpr tokens = case parseATerm tokens of
    Just (aterm1, (PlusTok : restTokens)) -> case parseATerm restTokens of
        Just (aterm2, restTokens) -> Just (AddExp aterm1 aterm2, restTokens)
        _ -> Nothing
    Just (aterm1, (MinusTok : restTokens)) -> case parseATerm restTokens of
        Just (aterm2, restTokens) -> Just (SubExp aterm1 aterm2, restTokens)
        _ -> Nothing
    Just (aterm, restTokens) -> Just(aterm, restTokens)
    _ -> Nothing


-- <bfactor> ::= "(" <bexpr> ")" | <aexpr> "<=" <aexpr> | <aexpr> "==" <aexpr> | "not" <bfactor> | <bvar> | "True" | "False"
parseBFactor :: [Token] -> Maybe(Bexp, [Token])
parseBFactor (OpenTok : restTokens) = case parseBExpr restTokens of
    Just (bexpr, CloseTok : restTokens) -> Just (bexpr, restTokens)
    _ -> Nothing

parseBFactor (NotTok : restTokens) = case parseBFactor restTokens of
    Just(bfactor, restTokens) -> Just(NotExp bfactor, restTokens)
    _ -> Nothing

parseBFactor (BoolTok bool : restTokens) = Just (BoolLit bool, restTokens)

parseBFactor tokens = case parseAExpr tokens of
    Just (aexpr1, LeTok : restTokens) -> case parseAExpr restTokens of
        Just (aexpr2, restTokens) -> Just (LessEquals aexpr1 aexpr2, restTokens)
        _ -> Nothing
    Just (aexpr1, IntEqTok : restTokens) -> case parseAExpr restTokens of
        Just (aexpr2, restTokens) -> Just (IntEquals aexpr1 aexpr2, restTokens)
        _ -> Nothing
    _ -> case tokens of             -- aexpr can also contain vartokens so this must be checked at the end
        VarTok var : restTokens -> Just (BoolVar var, restTokens)
        _ -> Nothing

-- <bterm> ::= <bfactor> { "=" <bfactor> }
parseBTerm :: [Token] -> Maybe(Bexp, [Token])
parseBTerm tokens = case parseBFactor tokens of
    Just (bfactor1, BoolEqTok : restTokens) -> case parseBTerm restTokens of
        Just (bfactor2, restTokens) -> Just (BoolEquals bfactor1 bfactor2, restTokens)
        _ -> Nothing
    Just (bfactor, restTokens) -> Just(bfactor, restTokens)
    _ -> Nothing


-- <bexpr> ::= <bterm> { "and" <bterm> }
parseBExpr :: [Token] -> Maybe(Bexp, [Token])
parseBExpr tokens = case parseBTerm tokens of
    Just (bterm1, AndTok : restTokens) -> case parseBTerm restTokens of
        Just (bterm2, restTokens) -> Just (AndExp bterm1 bterm2, restTokens)
        _ -> Nothing
    Just (bterm, restTokens) -> Just(bterm, restTokens)
    _ -> Nothing


-- <attribution> ::= <var> ":=" (<aexpr> | <bexpr>)
parseAttribution :: [Token] -> Maybe(Stm, [Token])
parseAttribution (VarTok var : AttrTok : restTokens) = case parseAExpr restTokens of
    Just (aexpr, EndTok : restTokens) -> Just (IntAttribution var aexpr, restTokens)
    _ -> case parseBExpr restTokens of
        Just (bexpr, EndTok : restTokens) -> Just (BoolAttribution var bexpr, restTokens)
        _ -> Nothing

parseAttribution _ = Nothing


-- <ifelse> ::= "if" <bexpr> "then" <stm> ["else" <stm>]
parseIf :: [Token] -> Maybe(Stm, [Token])
parseIf (IfTok : OpenTok : restTokens) = case parseBExpr restTokens of
    Just (bexpr, CloseTok : ThenTok : restTokens) -> parseThen restTokens bexpr
    _ -> Nothing

parseIf (IfTok : restTokens) = case parseBExpr restTokens of
    Just (bexpr, ThenTok : restTokens) -> parseThen restTokens bexpr
    _ -> Nothing

parseIf _ = Nothing


parseThen :: [Token] -> Bexp -> Maybe(Stm, [Token])
-- Has parentheses -> multiple statements possible
parseThen (OpenTok : restTokens) bexpr = case parseStmGroup restTokens of
    Just (stm, CloseTok : restTokens) -> parseElse restTokens bexpr stm
    _ -> Nothing

-- No parentheses -> only one statement
parseThen restTokens bexpr = case parseStm restTokens of
    Just (stm, restTokens) -> parseElse restTokens bexpr [stm]
    _ -> Nothing


parseElse :: [Token] -> Bexp -> [Stm] -> Maybe(Stm, [Token])
-- Has parentheses -> multiple statements possible
parseElse (ElseTok : OpenTok : restTokens) bexpr stm1 = case parseStmGroup restTokens of
    Just (stm2, CloseTok : restTokens) -> Just (IfElse bexpr stm1 (Just stm2), restTokens)
    _ -> Nothing

-- No parentheses -> only one statement
parseElse (ElseTok : restTokens) bexpr stm1 = case parseStm restTokens of
    Just (stm2, restTokens) -> Just (IfElse bexpr stm1 (Just [stm2]), restTokens)
    _ -> Nothing

-- No else statement
parseElse restTokens bexpr stm1 = Just (IfElse bexpr stm1 Nothing, restTokens)


-- <while> ::= "while" <bexpr> "do" <stm>
parseWhile :: [Token] -> Maybe(Stm, [Token])
parseWhile (WhileTok : OpenTok : restTokens) = case parseBExpr restTokens of
    Just (bexpr, CloseTok : DoTok : restTokens) -> parseDo restTokens bexpr
    _ -> Nothing

parseWhile (WhileTok : restTokens) = case parseBExpr restTokens of
    Just (bexpr, DoTok : restTokens) -> parseDo restTokens bexpr
    _ -> Nothing

parseWhile _ = Nothing

parseDo :: [Token] -> Bexp -> Maybe(Stm, [Token])
-- Has parentheses -> multiple statements possible
parseDo (OpenTok : restTokens) bexpr = case parseStmGroup restTokens of
    Just (stm, CloseTok : restTokens) -> Just (While bexpr stm, restTokens)
    _ -> Nothing

-- No parentheses -> only one statement
parseDo restTokens bexpr = case parseStm restTokens of
    Just (stm, restTokens) -> Just (While bexpr [stm], restTokens)
    _ -> Nothing


-- <stm> ::= (<attribution> ";") | <ifelse> | <while> | (<aexpr> ";") | (<bexpr> ";")
parseStm :: [Token] -> Maybe(Stm, [Token])
parseStm tokens = case parseAttribution tokens of
    Just (attribution, restTokens) -> Just (attribution, restTokens)
    _ -> case parseIf tokens of
        Just (ifelse, restTokens) -> Just (ifelse, restTokens)
        _ -> case parseWhile tokens of
            Just (while, restTokens) -> Just (while, restTokens)
            _ -> case parseAExpr tokens of
                Just (aexpr, EndTok : restTokens) -> Just (NoStm, restTokens)
                Just (_, _) -> Nothing
                _ -> case parseBExpr tokens of
                    Just (bexpr, EndTok : restTokens) -> Just (NoStm, restTokens)
                    Just (_, _) -> Nothing
                    _ -> Nothing


-- Parse multiple statements until failure
----------------------------------------------------------------------------
-- If used for parsing ifelse and while, failure will usually be a closing parentheses. 
-- In this case the program will continue, as it is the intended behaviour.
-- If a closing parentheses is not found, the program will fail.
parseStmGroup :: [Token] -> Maybe([Stm], [Token])
parseStmGroup tokens = case parseStm tokens of
    Just (stm, restTokens) -> case parseStmGroup restTokens of
        Just (stms, restTokens) -> Just (stm : stms, restTokens)
        _ -> Just ([stm], restTokens)
    _ -> Nothing

parseProgram :: [Token] -> [Stm]
parseProgram tokens = case parseStm tokens of
    Just (stm, []) -> [stm]
    Just (stm, restTokens) -> stm : parseProgram restTokens
    Nothing -> error "Parsing Error"