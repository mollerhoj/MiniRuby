module MiniRubyParser
       ( Error
       , parseString
       , parseFile
       )
       where

import MiniRubyAST
import SimpleParse
import Control.Applicative ((<$>))
import MiniRubyParserInternal

data Error = SomeError String deriving (Show,Eq)

parseString :: String -> Either Error Prog
parseString s = case parse program s of
                  (x1,""):_    -> Right x1
                  (_,rest):_   -> Left $ SomeError rest
                  []           -> Left $ SomeError "Unknown error"

parseFile :: FilePath -> IO (Either Error Prog)
parseFile filename = parseString <$> readFile filename
