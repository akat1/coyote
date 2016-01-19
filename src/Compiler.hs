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

module Compiler where

import Parser
import ParserExpr
import Arguments
import Data.Char

compileExternText :: String
compileExternText     =
    "extern readInt,readChar,writeInt,writeChar,arrayGetValue,arraySetValue,buildArray,freeArray\n"

compileHeaderText :: String
compileHeaderText     =
    "section .text\n" ++
    "global _start\n\n"

compileStart :: String
compileStart     =
    "_start: \n"++
    "\tmov  eax, esp\n" ++
    "\tlea  eax, [eax+4]\n" ++
    "\tmov  ecx, [esp]\n" ++
    "\tpush eax\n" ++
    "\tpush ecx\n" ++
    "\tcall main\n" ++
    "\tmov  ebx, eax\n" ++
    "\tmov  eax, 1\n" ++
    "\tint  80h\n\n" -- sys_exit()

compileProlog :: String -> Int -> String
compileProlog name varLen     =
    name ++ ":\n" ++
    "\tpush    ebp\n" ++
    "\tmov    ebp, esp\n"++
    "\tsub  esp, "++(show $ 4*varLen)++"\n"

compileEpilog :: [(VarDec,String)] -> String
compileEpilog var     =
    ".epilog\n" ++
    free ++
    "\tleave\n" ++
    "\tret\n" ++
    "\n"
    where
    free = compileVarDestroy var

compileCall :: Int -> String -> [(VarDec,String)] -> Expr -> (Int,String)
compileCall lbl fname var (Tab argv)     =
    (ulbl,
    arguments ++
    "\tcall "++fname++"\n"++
    cleanStack)
    where
    (ulbl,arguments) = 
        foldr (\z (x, y) -> (x + (fst $ compileExpr x var z), y ++ (snd $ compileExpr x var z))) (lbl,"") argv
    cleanStack = "\tadd esp,"++(show ((length argv)*4))++"\n"

compileExpr :: Int -> [(VarDec,String)] -> Expr -> (Int,String)
compileExpr lbl var x     =
    case x of
    Const x ->
        (lbl,
        "\tpush "++(show x)++"\n")
    BoolTrue -> 
        (lbl,
        "\tpush 1\n")
    BoolFalse -> 
        (lbl,
        "\tpush 0\n")
    CharC x ->
        (lbl,
        "\tpush "++(show $ ord x)++"\n")

    Var x    ->
        (lbl,
        "\tmov ecx,["++(varAddr var x)++"]\n" ++
        "\tpush ecx\n")

    VarArray x (Tab y) ->
        (ulbl,
        code ++
        "\tpush "++(show $ length y)++"\n"++
        "\tmov ecx,["++(varAddr var x)++"]\n" ++
        "\tpush ecx\n" ++
        "\tcall arrayGetValue\n"++
        "\tadd esp, "++(show $ 4*(length y)+8)++"\n"++
        "\tpush eax\n")
        where
        (ulbl, code) =
            foldr (\z (x,y) -> (x + (fst $ compileExpr x var z), y ++ (snd $ compileExpr x var z))) (lbl,"") y
    Neg x ->
        (ulbl,
        code ++
        "\tpop ecx\n" ++
        "\tneg ecx\n" ++
        "\tpush ecx\n")
        where
        (ulbl, code) = compileExpr lbl var x
    
    Not x ->
        (ulbl,
        code ++
        "\tpop ecx\n" ++
        "\tnot ecx\n" ++
        "\tand ecx, 1\n" ++
        "\tpush ecx\n")
        where
        (ulbl, code) = compileExpr lbl var x

    And x y ->
        (ulbl,
        left ++
        right ++
        "\tpop ecx\n" ++
        "\tpop edx\n" ++
        "\tand ecx, edx\n" ++
        "\tpush ecx\n")
        where 
        (nlbl,left) = compileExpr lbl var x
        (ulbl,right) = compileExpr nlbl var y

    Or x y ->
        (ulbl,
        left ++
        right ++
        "\tpop ecx\n" ++
        "\tpop edx\n" ++
        "\tor  ecx, edx\n" ++
        "\tpush ecx\n")
        where 
        (nlbl,left) = compileExpr lbl var x
        (ulbl,right) = compileExpr nlbl var y

    Add x y ->
        (ulbl,
        left ++
        right ++
        "\tpop ecx\n" ++
        "\tpop edx\n" ++
        "\tadd ecx, edx\n" ++
        "\tpush ecx\n")
        where 
        (nlbl,left) = compileExpr lbl var x
        (ulbl,right) = compileExpr nlbl var y

    Sub x y ->
        (ulbl,
        left ++
        right ++
        "\tpop edx\n" ++
        "\tpop ecx\n" ++
        "\tsub ecx, edx\n" ++
        "\tpush ecx\n")
        where 
        (nlbl,left) = compileExpr lbl var x
        (ulbl,right) = compileExpr nlbl var y

    Mul x y ->
        (ulbl,
        left ++
        right ++
        "\tpop eax\n" ++
        "\tpop ecx\n" ++
        "\timul ecx\n" ++
        "\tpush eax\n")
        where 
        (nlbl,left) = compileExpr lbl var x
        (ulbl,right) = compileExpr nlbl var y

    Div x y ->
        (ulbl,
        left ++
        right ++
        "\tpop ecx\n" ++
        "\tpop eax\n" ++
        "\txor edx, edx\n" ++
        "\tidiv ecx\n" ++
        "\tpush eax\n")
        where 
        (nlbl,left) = compileExpr lbl var x
        (ulbl,right) = compileExpr nlbl var y

    Mod x y ->
        (ulbl,
        left ++
        right ++
        "\tpop ecx\n" ++
        "\tpop eax\n" ++
        "\txor edx, edx\n" ++
        "\tidiv ecx\n" ++
        "\tpush edx\n")
        where 
        (nlbl,left) = compileExpr lbl var x
        (ulbl,right) = compileExpr nlbl var y


    EQU x y ->
        (ulbl,
        left ++
        right ++
        "\tpop ecx\n" ++
        "\tpop edx\n" ++
        "\txor eax, eax\n"++
        "\tcmp ecx, edx\n" ++
        "\tjnz .equ_l"++(show $ lbl+1)++"\n"++
        "\tinc eax\n"++
        ".equ_l"++(show $ lbl+1)++"\n"++
        "\tpush eax\n")
        where 
        (nlbl,left) = compileExpr (lbl+1) var x
        (ulbl,right) = compileExpr nlbl var y

    LTh x y ->
        (ulbl,
        left ++
        right ++
        "\tpop edx\n" ++
        "\tpop ecx\n" ++
        "\txor eax, eax\n"++
        "\tcmp ecx, edx\n" ++
        "\tjnl .lth_l"++(show $ lbl+1)++"\n"++
        "\tinc eax\n"++
        ".lth_l"++(show $ lbl+1)++"\n"++
        "\tpush eax\n")
        where 
        (nlbl,left) = compileExpr (lbl+1) var x
        (ulbl,right) = compileExpr nlbl var y

    GTh x y ->
        (ulbl,
        left ++
        right ++
        "\tpop edx\n" ++
        "\tpop ecx\n" ++
        "\txor eax, eax\n"++
        "\tcmp ecx, edx\n" ++
        "\tjng .gth_l"++(show $ lbl+1)++"\n"++
        "\tinc eax\n"++
        ".gth_l"++(show $ lbl+1)++"\n"++
        "\tpush eax\n")
        where 
        (nlbl,left) = compileExpr (lbl+1) var x
        (ulbl,right) = compileExpr nlbl var y

    LE x y ->
        (ulbl,
        left ++
        right ++
        "\tpop edx\n" ++
        "\tpop ecx\n" ++
        "\txor eax, eax\n"++
        "\tcmp ecx, edx\n" ++
        "\tjnle .le_l"++(show $ lbl+1)++"\n"++
        "\tinc eax\n"++
        ".le_l"++(show $ lbl+1)++"\n"++
        "\tpush eax\n")
        where 
        (nlbl,left) = compileExpr (lbl+1) var x
        (ulbl,right) = compileExpr nlbl var y

    GE x y ->
        (ulbl,
        left ++
        right ++
        "\tpop edx\n" ++
        "\tpop ecx\n" ++
        "\txor eax, eax\n"++
        "\tcmp ecx, edx\n" ++
        "\tjnge .ge_l"++(show $ lbl+1)++"\n"++
        "\tinc eax\n"++
        ".ge_l"++(show $ lbl+1)++"\n"++
        "\tpush eax\n")
        where 
        (nlbl,left) = compileExpr (lbl+1) var x
        (ulbl,right) = compileExpr nlbl var y

    NE x y ->
        compileExpr lbl var (Not (EQU x y))

    Fcall x y ->
        (ulbl,
        code ++
        "\tpush eax\n")
        where 
        (ulbl, code) = compileCall lbl x var y
        

    x -> (lbl,"\tnop ;UNKNOWN "++(show x)++"\n")

compileInstruction :: Int -> [(VarDec,String)] -> Instruction -> (Int,String)
compileInstruction lbl var x =
    case x of
    Skip -> 
        (0,
        "\tnop\n")

    Assgn x y -> 
        (ulbl,
        code ++ 
        "\tpop ecx\n" ++
        "\tmov ["++(varAddr var x)++"], ecx\n")
        where
        (ulbl,code) = compileExpr lbl var y

    AssgnTab x (Tab y) z ->
        (ulbl,
        code ++
        "\tpush "++(show $ length y)++"\n" ++
        "\tmov ecx,["++(varAddr var x)++"]\n" ++
        "\tpush ecx\n" ++
        val ++
        "\tcall arraySetValue\n"++
        "\tadd esp, "++(show $ 4*(length y)+12)++"\n")
        where
        (tlbl, code) =
            foldr (\z (x,y) -> (x + (fst $ compileExpr x var z), y ++ (snd $ compileExpr x var z))) (lbl,"") y
        (ulbl, val) = compileExpr tlbl var z

    Return y -> 
        (ulbl,
        code ++ 
        "\tpop eax\n" ++ 
        "\tjmp .epilog\n")
        where
        (ulbl,code) = compileExpr lbl var y

    IfThen x y ->
        (ulbl,
        condition ++
        "\tpop eax\n" ++
        "\tcmp eax, 0\n" ++
        "\tjz NEAR .ifThen_l" ++ (show $ lbl+1) ++ "\n"++
        body ++
        ".ifThen_l"++(show $ lbl+1)++"\n")
        where
        (nlbl,condition) = compileExpr (lbl+1) var x
        (ulbl,body) = compileIns nlbl var y


    IfThenElse x y z ->
        (ulbl,
        condition ++
        "\tpop eax\n" ++
        "\tcmp eax, 0\n"++
        "\tjz NEAR .ifThenElse_l"++ (show $ lbl+1) ++ "\n"++
        bodythen++
        "\tjmp NEAR .ifThenElse_l"++ (show $ lbl+2) ++ "\n"++
        ".ifThenElse_l" ++ (show $ lbl+1) ++ "\n"++
        bodyelse ++
        ".ifThenElse_l" ++ (show $ lbl+2) ++ "\n")
        where
        (exprlbl,condition) = compileExpr (lbl+2) var x
        (thenlbl,bodythen) = compileIns exprlbl var y
        (ulbl,bodyelse) = compileIns thenlbl var z

    For x y z i ->
        (ulbl,
        fromCode ++
        "\tpop ecx\n" ++
        "\tmov ["++(varAddr var x)++"],ecx\n" ++
        ".for_l"++(show $ lbl + 1)++"\n"  ++
        condCode ++
        "\tmov ecx,["++(varAddr var x)++"]\n" ++
        "\tpop edx\n" ++
        "\tcmp ecx, edx\n" ++
        "\tjng .for_l"++(show $ lbl + 3 )++"\n" ++
        "\tjmp NEAR .for_l"++(show $ lbl + 2)++"\n" ++
        ".for_l"++(show $ lbl + 3 )++"\n" ++
        body ++
        "\tmov ecx,["++(varAddr var x)++"]\n" ++
        "\tinc ecx\n" ++
        "\tmov [" ++(varAddr var x)++"],ecx\n" ++
        "\tjmp NEAR .for_l"++(show $ lbl + 1)++"\n" ++
        ".for_l"++(show $ lbl + 2)++"\n")
        where
        (flbl,fromCode) = compileExpr (lbl+2) var y
        (tlbl,condCode) = compileExpr flbl var z
        (ulbl,body) = compileIns tlbl var i

    While x y ->
        (ulbl,
        ".while_l"++(show $ lbl+1)++"\n"++
        condition++
        "\tpop ecx\n"++
        "\tcmp ecx, 0\n"++
        "\tjz NEAR .while_l"++(show $ lbl+2)++"\n"++
        body++
        "\tjmp .while_l"++(show $ lbl+1)++"\n"++
        ".while_l"++(show $ lbl+2)++"\n")
        where 
        (exprlbl, condition) = compileExpr (lbl+2) var x
        (ulbl, body) = compileIns exprlbl var y

    FunCall x y ->
        compileCall lbl x var y

    y -> (lbl,"\tnop ;UNKNOWN "++(show y)++"\n")

compileIns :: Int -> [(VarDec,String)] -> [Instruction] -> (Int,String)
compileIns lbl var x =
    foldl (\(x,y) z -> (x + (fst $ compileInstruction x var z), y ++ (snd $ compileInstruction x var z))) (lbl,"") x
    

getArg' :: Int -> [VarDec] -> [(VarDec,String)]
getArg' _ [] = []
getArg' x (y:ys) = [(y,"ebp+"++(show x))] ++ getArg' (x+4) ys

getArg :: [VarDec] -> [(VarDec,String)]
getArg x = getArg' 8 x

getVar' :: Int -> [VarDec] -> [(VarDec,String)]
getVar' _ [] = []
getVar' x (y:ys) = [(y,"ebp-"++(show x))] ++ getVar' (x+4) ys

getVar :: [VarDec] -> [(VarDec,String)]
getVar x = getVar' 4 x

varAddr :: [(VarDec,String)] -> String -> String

varAddr [] _ = "UNKNOWN"
varAddr ((Arg _ x,z):xs) y 
    | y == x = z
    | otherwise = varAddr xs y

varAddr ((VarDec _ x _,z):xs) y 
    | x == y = z
    | otherwise = varAddr xs y

compileVarInit :: Int -> [(VarDec,String)] -> [VarDec] -> (Int, String)

compileVarInit lbl _ [] = (lbl,"")

compileVarInit lbl var (x:xs) =

    case x of
    VarDec (Array _ d) x (Tab y) ->
        (ulbl,
        code ++
        "\tpush "++(show d)++"\n"++
        "\tcall buildArray\n"++
        "\tadd esp,"++(show $ 4*(length y)+4)++"\n"++
        "\tmov ["++(varAddr var x)++"], eax\n" ++
        rest)
        where
        (clbl, code) =
            foldr (\z (x,y) -> (x + (fst $ compileExpr x var z), y ++ (snd $ compileExpr x var z))) (lbl,"") y
        (ulbl, rest) = compileVarInit clbl var xs

    VarDec _ x y ->
        (ulbl, code ++ rest)
        where
        (clbl, code) = compileInstruction lbl var (Assgn x y)
        (ulbl, rest) = compileVarInit clbl var xs

    _ -> (lbl,"\tnop ;UNKNOWN - VARDEC \n")

compileVarDestroy :: [(VarDec,String)] -> String

compileVarDestroy [] = ""

compileVarDestroy ((x,y):xs) =
    case x of
    VarDec (Array _ d) n _ ->
        "\tmov ecx, ["++(varAddr ((x,y):xs) n)++"]\n" ++
        "\tpush ecx\n" ++
        "\tpush "++(show d)++"\n" ++
        "\tcall freeArray\n" ++
        "\tadd esp, 8\n" ++
        compileVarDestroy xs
    _ -> compileVarDestroy xs

compileFun :: Int -> [Function] -> (Int,String)

compileFun lbl [] = 
    (lbl,"")

compileFun lbl ((Fun name arg _ var ins):xs) =
    (ulbl,prolog ++ varInit ++ code ++ epilog ++ rest)
    where
    prolog = compileProlog name (length $ getVar var)
    epilog = compileEpilog (getVar var)
    varList = (getArg arg) ++ (getVar var)
    (flbl,varInit) = compileVarInit lbl varList var
    (nlbl,code) = compileIns flbl varList ins
    (ulbl,rest) = compileFun nlbl xs
    

compile ast    =
    do
    return (externText ++ headerText ++ start ++ code)
    where
    externText = compileExternText
    headerText = compileHeaderText
    start  = compileStart
    code = snd $ compileFun 0 ast

