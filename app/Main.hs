module Main where

import System.Environment

import Lib
import Com.NoSyn.Parser.NoSynParser
import Com.NoSyn.Parser.Lexer

main = do
    (x:_) <- getArgs
    readFile x >>= print.parse.lexer
