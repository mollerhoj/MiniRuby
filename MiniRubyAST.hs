module MiniRubyAST
       ( ObjectReference
       , Value (..)
       , Name
       , Expr (..)
       , Exprs
       , Cases
       , Case
       , Pattern (..)
       , ClassDecl (..)
       , ConstructorDecl
       , NamedMethodDecl (..)
       , ReceiveDecl (..)
       , MethodDecl (..)
       , Prog
       )
       where

type Name = String

-- | An object reference is an integer uniquely identifying an object.
-- This does not appear in the grammar, but is used in the runtime
-- representation in the interpreter.
type ObjectReference = Int

-- | A value is either a term, an integer, or a string.  Expressions
-- are evaluated to values and methods return values.
data Value = IntValue Integer
           | StringValue String
           | SymbolValue String
           | BooleanValue Bool
           | ReferenceValue ObjectReference
           deriving (Eq, Show)

-- | An expression.
data Expr = IntConst Integer    
          | StringConst String  
          | SymbolConst String  
          | BooleanConst Bool
          | Self                
          | Plus Expr Expr     
          | Minus Expr Expr    
          | Times Expr Expr     
          | DividedBy Expr Expr
          | Return Expr        
          | SetField Name Expr 
          | SetVar Name Expr   
          | ReadVar Name       
          | ReadField Name     
          | Match Expr Cases   
          | CallMethod                   -- 
            Expr -- ^ Receiver           --
            Name -- ^ Method name        --
            [Expr] -- ^ Method arguments --
          | New Name [Expr]    -- 
          deriving (Eq, Show)

type Exprs = [Expr]

type Cases = [Case]

type Case = (Pattern,Exprs)

data Pattern = ConstInt Integer
             | ConstString String
             | AnyValue Name
             deriving (Eq, Show)

data ClassDecl = ClassDecl { className :: Name
                           , classConstructor :: Maybe ConstructorDecl
                           , classMethods :: [NamedMethodDecl]
                           , classReceive :: Maybe ReceiveDecl
                           }
               deriving (Eq, Show)

type ConstructorDecl = MethodDecl

data ReceiveDecl = ReceiveDecl { receiveParameters :: [Name]
                               , receiveBody :: Exprs
                               }
                 deriving (Eq, Show)

data NamedMethodDecl = NamedMethodDecl Name MethodDecl
                     deriving (Eq, Show)

data MethodDecl = MethodDecl { methodParameters :: [Name]
                             , methodBody :: Exprs
                             }
                deriving (Eq, Show)

-- | A program is just a list of class declarations.  The program
-- is executed by creating an instance of a class named Main.  If
-- there is no such class defined, interpretation must fail.
type Prog = [ClassDecl]
