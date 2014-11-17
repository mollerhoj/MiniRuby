module Main
       (main)
where

import MiniRubyParser
import MiniRubyInterpreter

import System.Environment

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> do
              s <- readFile file
              case parseString s of
                Left e -> error $ show e
                Right prog ->
                  case runProg prog of
                    Left e -> error $ show e
                    Right output -> putStr output
            _ ->
              error "Give me a (single) argument!"
