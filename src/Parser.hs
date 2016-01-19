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

module Parser where

import Lexer
import ParserExpr
import Text.ParserCombinators.Parsec
import System.Exit

data Type' = Int | Char | Bool | Array Type' Int deriving Show
data VarDec = Arg Type' String | VarDec Type' String Expr deriving Show

data Instruction = 
      Assgn String Expr
    | AssgnTab String Expr Expr
    | Skip
    | Return Expr
    | For String Expr Expr [Instruction]
    | IfThenElse Expr [Instruction] [Instruction]
    | IfThen Expr [Instruction]
    | While Expr [Instruction]
    | FunCall String Expr
    deriving Show

data Function =
    Fun String [VarDec] Type' [VarDec] [Instruction]

demension    =
    do
    x <- many1 $ char '@'
    return $ length x
    <?>
    "Demension"

simpleType    =
    do
    reserved "bool"
    return Bool
    <|>
    do
    reserved "int"
    return Int
    <|>
    do
    reserved "char"
    return Char
    <?> 
    "Simple type"

arrayType    =
    do
    x <- demension
    whiteSpace
    y <- simpleType
    return $ Array y x
    <?>
    "Array type"

type'    =
    (simpleType
    <|>
    arrayType)
    <?>
    "Type"


argDef    =
    do
    x <- type'
    y <- identifier
    return $ Arg x y
    <?>
    "Argument"

args    =
    commaSep argDef

varSimpleDef    =
    do
    x <- simpleType
    y <- identifier
    reservedOp "="
    z <- expr
    return $ VarDec x y z
    <?>
    "Variable declaration"

varArrayDef    =
    do
    x <- arrayType
    y <- identifier
    reserved "size"
    z <- tabExpr
    return $ VarDec x y z
    <?>
    "Variable declaration"


varDec    =
    do
    reserved "with"
    x <- commaSep (varSimpleDef <|> varArrayDef)
    return x
    <|>
    return []

assgn    =
    do
    x <- identifier
    reservedOp "<-"
    y <- expr
    return $ Assgn x y
    
assgnTab    =
    do
    x <- identifier
    y <- tabExpr
    reservedOp "<-"
    z <- expr
    return $ AssgnTab x y z

while    =
    do
    cond <- between (reserved "while") (reserved "do") expr
    ins <- instructions
    reserved "done"
    return $ While cond ins

return'    =
    do
    reservedOp "^"
    e <- expr
    return $ Return e

for    =
    do
    reserved "for"
    x <- identifier
    reserved "from"
    from <- expr
    reserved "to"
    to <- expr
    reserved "do"
    ins <- instructions
    reserved "done"
    return $ For x from to ins

ifThen    =
    do
    reserved "if"
    cond <- expr
    reserved "then"
    ins <- instructions
    reserved "fi"
    return $ IfThen cond ins

ifThenElse    =
    do
    reserved "if"
    cond <- expr
    reserved "then"
    insThen <- instructions
    reserved "else"
    insElse <- instructions
    reserved "fi"
    return $ IfThenElse cond insThen insElse

funCall    =
    do
    x <- identifier
    y <- parens $ commaSep expr
    return $ FunCall x (Tab y)

skip    =
    do
    reserved "skip"
    return Skip

instruction    =
    try ifThen
    <|>
    try ifThenElse
    <|>
    try while
    <|>
    try for
    <|>
    try return'
    <|>
    try funCall
    <|>
    try assgn
    <|>
    try assgnTab
    <|>
    try skip
    <?>
    "Instruction"

instructions    =
    do
    x <- instruction `sepBy1` symbol "."
    return x
    <|>
    return [Skip]

function    =
    do
    id <- identifier
    symbol ":"
    args <- args
    symbol "->"
    rType <- simpleType
    varDef <- varDec
    reserved "is"
    instr <- instructions
    return $ Fun id args rType varDef instr
    <?>
    "Function"


program    = 
    do
    whiteSpace
    x <- many1 function
    eof
    return x
    <?>
    "Program"
    

parseProgram input     =
        case (parse program "coyote" input) of
            Left err    -> 
                    do
                    putStr "parse error at "
                    print err
                    exitWith $ ExitFailure 1
                    return Nothing
            Right x        -> 
                    do
                    return $ Just x
