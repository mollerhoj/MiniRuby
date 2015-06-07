module MiniRubyParserInternal where

import MiniRubyAST
import SimpleParse
import Control.Applicative ((<|>))
import Data.Char (isLetter,isDigit)

program :: Parser [ClassDecl]
program = do x <- classDecls
             more space
             return x

-- Utilities
digits :: Parser String
digits = munch1 isDigit

isUnderscore :: Char -> Bool
isUnderscore = (==) '_'

isNotQuote :: Char -> Bool
isNotQuote = (/=) '"'

isConstituent :: Char -> Bool
isConstituent c = isLetter c || isDigit c || isUnderscore c

letterOrUnderscore :: Parser Char
letterOrUnderscore = satisfy isLetter <|> satisfy isUnderscore

terminator :: Parser Char
terminator = schar ';' <|> schar '\n'

---- Int Const, String Const

intConst :: Parser Expr
intConst = token (do x <- digits
                     return $ IntConst $ read x)

constInt :: Parser Pattern
constInt = token (do x <- digits
                     return $ ConstInt $ read x)

stringConst :: Parser Expr
stringConst = do schar '"'
                 x <- munch isNotQuote
                 schar '"'
                 return $ StringConst $ x

trueConst :: Parser Expr
trueConst = do symbol "True"
               return $ BooleanConst True

falseConst :: Parser Expr
falseConst = do symbol "False"
                return $ BooleanConst False

symbolConst :: Parser Expr
symbolConst = do schar ':'
                 n <- anyName
                 return $ SymbolConst $ n

constString :: Parser Pattern
constString = do schar '"'
                 x <- munch isNotQuote
                 schar '"'
                 return $ ConstString $ x

-- Classes
classDecls :: Parser [ClassDecl]
classDecls = more classDecl

classDecl :: Parser ClassDecl
classDecl = do symbol "class"
               n <- name
               terminator
               mcd  <- maybeConstructorDecl
               nds <- namedMethodDecls
               mrd  <- maybeReceiveDecl
               symbol "end"
               return ClassDecl { className = n,
                                  classConstructor = mcd,
                                  classMethods = nds,
                                  classReceive = mrd }

namedMethodDecls :: Parser [NamedMethodDecl]
namedMethodDecls = more namedMethodDecl

namedMethodDecl :: Parser NamedMethodDecl
namedMethodDecl = do symbol "def"
                     n <- name
                     schar '('
                     ps <- params
                     schar ')'
                     terminator
                     es <- exprs
                     symbol "end"
                     return $ NamedMethodDecl n (MethodDecl { methodParameters = ps,
                                                              methodBody = es })

maybeReceiveDecl :: Parser (Maybe ReceiveDecl)
maybeReceiveDecl = do symbol "def"
                      symbol "method_missing"
                      schar '('
                      ps <- params
                      schar ')'
                      terminator
                      es <- exprs
                      symbol "end"
                      return $ Just (ReceiveDecl {receiveParameters = ps,
                                                  receiveBody = es})
                   <|> do return Nothing

maybeConstructorDecl :: Parser (Maybe ConstructorDecl)
maybeConstructorDecl = do symbol "def"
                          symbol "initialize"
                          schar '('
                          ps <- params
                          schar ')'
                          terminator
                          es <- exprs
                          symbol "end"
                          return $ Just (MethodDecl { methodParameters = ps,
                                                      methodBody = es })
                          <|> return Nothing

-- Parameters and args
-- These aliases are used to make the code more readable:
type Param = Name
type Params = [Param]
type Arg = Expr
type Args = [Arg]

args :: Parser Args
args = sepBy arg $ schar ','

arg :: Parser Expr
arg = expr

params :: Parser Params
params = sepBy param $ schar ','

param :: Parser Param
param = name

-- Exprs
exprs :: Parser Exprs
exprs = do return []
        <|> exprs1

exprs1 :: Parser Exprs
exprs1 = do e <- exprEnd
            return [e]
         <|> do e' <- exprChained
                es <- exprs
                return (e':es)
                    
exprChained :: Parser Expr
exprChained = do e <- expr
                 terminator 
                 return e

exprEnd :: Parser Expr
exprEnd = do e <- expr
             schar ';'
             return e
          <|> expr

subExpr :: Parser Expr
subExpr = term `chainl1` addOp

expr :: Parser Expr
expr = do s1 <- subExpr 
          compareExpr s1
       
compareExpr :: Expr -> Parser Expr
compareExpr e = do lessThanExpr e <|> greaterThanExpr e <|> return e

lessThanExpr :: Expr -> Parser Expr
lessThanExpr e1 = do schar '<'
                     e2 <- subExpr
                     return $ LessThan e1 e2

greaterThanExpr :: Expr -> Parser Expr
greaterThanExpr e1 = do schar '>'
                        e2 <- subExpr
                        return $ GreaterThan e1 e2

-- Booleans
lessThan :: Parser Expr
lessThan = do schar '<'
              e1 <- expr
              e2 <- expr
              return $ BooleanConst False

-- Ariteritmics
term :: Parser Expr
term = factorChain `chainl1` mulOp

factorChain :: Parser Expr
factorChain = do f <- factor
                 rest f
              where
                 rest f = do schar '.'
                             n <- name
                             schar '('
                             as <- args
                             schar ')'
                             rest (CallMethod f n as)
                          <|> return f

addOp :: Parser (Expr -> Expr -> Expr)
addOp = do schar '+'
           return Plus
        <|> do schar '-'
               return Minus

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = do schar '*'
           return Times
        <|> do schar '/'
               return DividedBy

-- Identifiers & Keywords

anyName :: Parser Name
anyName = do x  <- letterOrUnderscore
             xs <- munch isConstituent
             if (x : xs ) `elem` keywords then reject
             else return $ x : xs

name :: Parser Name
name = do token anyName

fieldName :: Parser Name
fieldName = do schar '@'
               anyName

keywords :: [String]
keywords = ["end", "self", "class", "new", "method_missing", "initialize", "case", "return", "when", "else", "True", "False"]

factor :: Parser Expr
factor =     self
         <|> return'
         <|> new
         <|> parenExpr
         <|> setField
         <|> setVar
         <|> readVar
         <|> readField
         <|> case'
         <|> stringConst
         <|> symbolConst
         <|> trueConst
         <|> falseConst
         <|> intConst
         <|> selfCall

parenExpr :: Parser Expr
parenExpr = do schar '('
               e <- expr
               schar ')'
               return e

new :: Parser Expr
new = do symbol "new"
         n <- name
         schar '('
         as <- args
         schar ')'
         return $ New n as

self :: Parser Expr
self = do symbol "self"
          return Self

return' :: Parser Expr
return' = do symbol "return"
             e <- expr
             return $ Return e

-- Read & Write variables 
setVar :: Parser Expr
setVar = do n <- name
            schar '='
            e <- expr
            return $ SetVar n e

setField :: Parser Expr
setField = do n <- fieldName
              schar '='
              e <- expr
              return $ SetField n e

readVar :: Parser Expr
readVar = do n <- name
             return $ ReadVar n
             
readField :: Parser Expr
readField = do n <- fieldName
               return $ ReadField n

-- TermLiteral Name [Expr]

selfCall :: Parser Expr
selfCall = do n <- name
              schar '('
              as <- args
              schar ')'
              return $ CallMethod Self n as

-- Match & Cases & Patterns
case' :: Parser Expr
case' = do symbol "case"
           e <- expr
           terminator
           c <- whens
           symbol "end"
           return $ Match e c

whens :: Parser Cases
whens = more when'

when' :: Parser Case
when' = do symbol "when"
           p <- pattern
           terminator
           es <- exprs
           terminator
           return (p,es)

pattern :: Parser Pattern
pattern = constInt <|> constString <|> anyValue

anyValue :: Parser Pattern
anyValue = do n <- name
              return $ AnyValue n
