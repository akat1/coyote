--
-- This software is released under the beerware licence.
-- ( Borrowed from FreeBSD code )
-- 
-- <shm@digitalsun.pl> wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me a beer in return. :)
--
--                                                       Mateusz Kocielski
--

module ParserExpr where

import Lexer
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

data Expr    =
        Const Integer
        | CharC Char
        | BoolTrue
        | BoolFalse
        | Var String
        | VarArray String Expr
        | Neg Expr
        | Not Expr
        | Div Expr Expr
        | Mod Expr Expr
        | Mul Expr Expr
        | Sub Expr Expr
        | Add Expr Expr
        | And Expr Expr
        | Or  Expr Expr
        | LTh  Expr Expr
        | GTh  Expr Expr
        | LE  Expr Expr
        | GE  Expr Expr
        | NE  Expr Expr
        | EQU  Expr Expr
        | Tab  [Expr]
        | Fcall String Expr

        deriving Show

expr    :: Parser Expr

expr    = buildExpressionParser table factor

table    =
    [
        [ opP "-" (Neg), opP "~" (Not) ],
        [ opI "*" (Mul) AssocLeft, opI "div" (Div) AssocLeft, opI "mod" (Mod) AssocLeft ],
        [ opI "+" (Add) AssocLeft, opI "-" (Sub) AssocLeft ],
        [ opI "<=" (LE) AssocRight, opI ">=" (GE) AssocRight, opI "<" (LTh) AssocRight, opI ">" (GTh) AssocRight,
          opI "!=" (NE) AssocLeft, opI "==" (EQU) AssocLeft ],
        [ opI "or" (Or) AssocLeft ],
        [ opI "and" (And) AssocRight ]
    ]
    where
        opI s f a = Infix ( do { reservedOp s ; return f } ) a
        opP s f = Prefix ( do { reservedOp s ; return f } )

number    =
    do
    x <- natural
    return $ Const x

varArray =
    do
    x <- identifier
    y <- tabExpr
    return $ VarArray x y

variable =
    do
    x <- identifier
    return $ Var x

tabExpr    =
    do
    x <- squares (commaSep1 expr)
    return $ Tab x

bool'    =
    do
    reserved "true"
    return BoolTrue
    <|>
    do
    reserved "false"
    return BoolFalse

fcall    =
    do
    x <- identifier
    y <- parens (commaSep expr)
    return $ Fcall x (Tab y)

charC    =
    do
    x <- charLiteral
    return $ CharC x

factor     =
    try charC
    <|>
    try fcall
    <|>
    try number
    <|>
    try (parens expr)
    <|>
    try varArray
    <|>
    try variable
    <|>
    try bool'
    <?>
    "Expression"
