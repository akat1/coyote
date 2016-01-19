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

module Lexer where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language( emptyDef )
import qualified Text.ParserCombinators.Parsec.Token as Lex

lexer :: Lex.TokenParser()

lexer    = Lex.makeTokenParser
    emptyDef
    {
     Lex.commentStart = "{-",
     Lex.commentEnd = "-}",
     Lex.commentLine = "//",
     Lex.identStart = letter,
     Lex.identLetter = alphaNum <|> char '_',
     Lex.opStart = oneOf "=><+-*!",
     Lex.opLetter = char '=',
     Lex.reservedOpNames = ["~","-","*","+","div","mod","and","or",">=","<=","<-",">","<","="],

     Lex.reservedNames = 
        ["int","char","bool"] ++
        ["true","false"] ++
        ["with","is","size"] ++
        ["for","from","to","do","done"] ++
        ["if","then","else","fi"] ++
        ["div","mod","and","or","not"] ++
        ["return"]
    }
    
whiteSpace    = Lex.whiteSpace lexer
symbol        = Lex.symbol lexer
natural       = Lex.natural lexer
parens        = Lex.parens lexer
identifier    = Lex.identifier lexer
reserved      = Lex.reserved lexer
reservedOp    = Lex.reservedOp lexer
commaSep      = Lex.commaSep lexer
commaSep1     = Lex.commaSep1 lexer
squares       = Lex.squares lexer
charLiteral   = Lex.charLiteral lexer

