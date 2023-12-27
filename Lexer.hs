module Lexer where

import Data.Char

data Token
    = PlusTok
    | MinusTok
    | TimesTok
    | OpenTok
    | CloseTok
    | EndTok
    | IntEqTok
    | LeTok
    | BoolEqTok
    | IfTok
    | ThenTok
    | ElseTok
    | WhileTok
    | DoTok
    | AttrTok
    | AndTok
    | NotTok
    | IntTok Integer
    | BoolTok Bool
    | VarTok String
    deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer ('+' : restStr) = PlusTok : lexer restStr
lexer ('-' : restStr) = MinusTok : lexer restStr
lexer ('*' : restStr) = TimesTok : lexer restStr
lexer ('(' : restStr) = OpenTok : lexer restStr
lexer (')' : restStr) = CloseTok : lexer restStr
lexer (';' : restStr) = EndTok : lexer restStr
lexer ('=':'=' : restStr) = IntEqTok : lexer restStr
lexer ('<':'=' : restStr) = LeTok : lexer restStr
lexer ('=' : restStr) = BoolEqTok : lexer restStr
lexer ('i':'f': restStr) = IfTok : lexer restStr
lexer ('t':'h':'e':'n': restStr) = ThenTok : lexer restStr
lexer ('e':'l':'s':'e': restStr) = ElseTok : lexer restStr
lexer ('w':'h':'i':'l':'e': restStr) = WhileTok : lexer restStr
lexer ('d':'o': restStr) = DoTok : lexer restStr
lexer (':':'=' : restStr) = AttrTok : lexer restStr
lexer ('a':'n':'d': restStr) = AndTok : lexer restStr
lexer ('n':'o':'t': restStr) = NotTok : lexer restStr
lexer ('T':'r':'u':'e': restStr) = BoolTok True : lexer restStr
lexer ('F':'a':'l':'s':'e': restStr) = BoolTok False : lexer restStr
lexer (x : restStr)
    | isDigit x = IntTok (read (x : takeWhile isDigit restStr)) : lexer (dropWhile isDigit restStr)
    | isAlpha x && isLower x = VarTok (x : takeWhile isAlphaNum restStr) : lexer (dropWhile isAlphaNum restStr)
    | isSpace x = lexer restStr
    | otherwise = error ("lexer: unexpected character " ++ show x)
