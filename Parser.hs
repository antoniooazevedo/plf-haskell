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
  | IntEquals Aexp Aexp
  | BoolEquals Bexp Bexp
  | LessEquals Aexp Aexp
  | AndExp Bexp Bexp
  | NotExp Bexp
  deriving Show

data Stm
  = Attribution String Aexp
  | While Bexp Stm
  | IfElse Bexp Stm (Maybe Stm)
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


-- <bfactor> ::= "(" <bexpr> ")" | <aexpr> "<=" <aexpr> | <aexpr> "==" <aexpr> | "not" <bfactor> | "True" | "False"
parseBFactor :: [Token] -> Maybe(Bexp, [Token])
parseBFactor (OpenTok : restTokens) = case parseBExpr restTokens of
    Just (bexpr, CloseTok : restTokens) -> Just (bexpr, restTokens)
    _ -> Nothing

parseBFactor (NotTok : restTokens) = case parseBFactor restTokens of
    Just(bfactor, restTokens) -> Just(NotExp bfactor, restTokens)
    _ -> Nothing    

parseBFactor (BoolTok bool : restTokens) = Just (BoolLit bool, restTokens)

parseBFactor tokens = case parseAExpr tokens of
    Just (aexpr, LeTok : restTokens) -> case parseAFactor restTokens of
        Just (afactor, restTokens) -> Just (LessEquals aexpr afactor, restTokens)
        _ -> Nothing
    Just (aexpr, IntEqTok : restTokens) -> case parseAFactor restTokens of
        Just (afactor, restTokens) -> Just (IntEquals aexpr afactor, restTokens)
        _ -> Nothing
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


-- <attribution> ::= <var> ":=" <aexpr>
parseAttribution :: [Token] -> Maybe(Stm, [Token])
parseAttribution (VarTok var : AttrTok : restTokens) = case parseAExpr restTokens of
    Just (aexpr, restTokens) -> Just (Attribution var aexpr, restTokens)
    _ -> Nothing
parseAttribution _ = Nothing


-- <ifelse> ::= "if" <bexpr> "then" <stm> ["else" <stm>]
parseIfElse :: [Token] -> Maybe(Stm, [Token])
parseIfElse (IfTok : restTokens) = case parseBExpr restTokens of
    Just (bexpr, ThenTok : restTokens) -> case parseStm restTokens of
        Just (stm1, ElseTok : restTokens) -> case parseStm restTokens of
            Just (stm2, restTokens) -> Just (IfElse bexpr stm1 (Just stm2), restTokens)
            _ -> Nothing
        Just (stm, restTokens) -> Just (IfElse bexpr stm Nothing, restTokens)
        _ -> Nothing
    _ -> Nothing
parseIfElse _ = Nothing

-- <while> ::= "while" <bexpr> "do" <stm>
parseWhile :: [Token] -> Maybe(Stm, [Token])
parseWhile (WhileTok : restTokens) = case parseBExpr restTokens of
    Just (bexpr, DoTok : restTokens) -> case parseStm restTokens of
        Just (stm, restTokens) -> Just (While bexpr stm, restTokens)
        _ -> Nothing
    _ -> Nothing
parseWhile _ = Nothing


-- <stm> ::= <attribution> | <ifelse> | <while>
parseStm :: [Token] -> Maybe(Stm, [Token])
parseStm tokens = case parseAttribution tokens of
    Just (attribution, restTokens) -> Just (attribution, restTokens)
    _ -> case parseIfElse tokens of
        Just (ifelse, restTokens) -> Just (ifelse, restTokens)
        _ -> case parseWhile tokens of
            Just (while, restTokens) -> Just (while, restTokens)
            _ -> Nothing