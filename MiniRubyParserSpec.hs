import MiniRubyAST
import MiniRubyParser
import Data.String

test f = do x <- parseFile f
            return $ x

result f = readFile f

-- Removing whitespace and characters that I am confused how haskell's `show` method print.
transform s = filter (/='\\') $ filter (/='"') $ filter (/='/') $ filter (/='\n') $ filter (/=' ') s

-- Run the tests
main = do putStrLn "Positive tests:"
          compar "examples/fact.rb" "examples/fact.ast"
          compar "examples/observable.rb" "examples/observable.ast"
          compar "examples/proxy.rb" "examples/proxy.ast"
          compar "examples/return.rb" "examples/return.ast"
          compar "examples/strings.rb" "examples/strings.ast"
          compar "examples/integers.rb" "examples/integers.ast"
          compar "examples/precedence.rb" "examples/precedence.ast"
          compar "examples/whitespace.rb" "examples/whitespace.ast"
          compar "examples/chaining_calls.rb" "examples/chaining_calls.ast"
          compar "examples/names.rb" "examples/names.ast"
          compar "examples/symbols.rb" "examples/symbols.ast"

          putStrLn "\nNegative tests:"
          negati "examples/unfinished_string.rb"
          negati "examples/double_dots.rb"
          negati "examples/negative_integer.rb"
          negati "examples/names_with_spaces.rb"
          negati "examples/capital_keyword.rb"
          negati "examples/at_whitespace.rb"

negati :: String -> IO()
negati i = do t <- test i
              case t of Left x -> putStrLn "True"

compar :: String -> String -> IO()
compar i o = do
  t <- test i
  r <- result o
  t' <- do return $ transform $ show $ case t of Right x -> x
                                                 Left x -> fail "..."
  r' <- do return $ transform $ r
  putStrLn $ (show (r' == t')) ++ "\t" ++ show i

