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

module Arguments where

import    System.Console.GetOpt
import    System.Exit
import    System.Environment

version = "0.1"
header    = "Coyote version "++version

defaultOutputFile    = "a.s"

--
-- GetOpt's flags
--

data    Flag
    = InputFile String    -- -i
    | OutputFile String    -- -o
    | Help            -- -h
    | Version        -- -v
    | Credits        -- -c
    deriving Eq

flags    =
    [
        -- Credits
        Option    ['c']     ["credits"]    (NoArg Credits)
        "Display Coyote's credits",

        -- InputFile
        Option    ['i']    ["input"]    (ReqArg InputFile "FILE")
        "Input file",

        -- OutputFile
        Option    ['o']    ["output"]    (ReqArg OutputFile "FILE")
        "Output file",

        -- Help
        Option    ['h']    ["help"]    (NoArg Help)
        "Display help message",

        -- Version
        Option    ['v']    ["Version"]    (NoArg Version)
        "Display version"
    ]

showCredits :: IO()

showCredits    =
    do
    putStrLn header
    putStrLn "Written by Mateusz Kocielski <shm@digitalsun.pl>"

showHelp :: IO()

showHelp    =
    putStrLn $ usageInfo header flags

showVersion :: IO()

showVersion    =
    putStrLn version

--
-- Parse Arguments
--

parseArgs argv =

    case getOpt Permute flags argv of

    (args, [], [])    -> 
            return args

    (_, _, errs)    -> do
            ioError (userError (concat errs ++ usageInfo header flags))
            exitWith (ExitFailure 1)

--
-- Action Args
--

actionArgs []        = return ""    
actionArgs (Credits:_)    = showCredits >> exitWith (ExitFailure 1)
actionArgs (Version:_)    = showVersion >> exitWith (ExitFailure 1)
actionArgs (Help:_)    = showHelp >> exitWith (ExitFailure 1)
actionArgs (_:xs)    = actionArgs xs


getInputFile []            = putStrLn "No input files" >> exitWith (ExitFailure 1)
getInputFile (InputFile x:_)    = return x
getInputFile (_:xs)        = getInputFile xs

getOutputFile []        = return defaultOutputFile
getOutputFile (OutputFile x:_)    = return x
getOutputFile (_:xs)        = getOutputFile xs

getFromArgs     =
    do
    -- parse arguments
    
    args <- getArgs >>= parseArgs
    actionArgs args
    inputFile <- getInputFile args
    outputFile <- getOutputFile args
    return (inputFile,outputFile)

