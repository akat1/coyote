--
-- This software is released under the beerware licence.
-- ( Borrowed from FreeBSD code )
-- 
-- <shm@digitalsun.pl> wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me a beer in return. :)
--
--                              Mateusz Kocielski
--

import Arguments
import Parser
import Compiler

main     =    
    do

    -- parse arguments
    (inputFile, outputFile) <- getFromArgs

    -- parse inputfile
    code <- readFile inputFile
    Just ast <- parseProgram code

    -- compile
    x <- compile ast

    -- write outputfile
    writeFile outputFile x
    
    -- quit
    return Right

