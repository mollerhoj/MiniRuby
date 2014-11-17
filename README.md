MiniRuby
============
By Jens Dahl Møllerhøj, mollerhoj3@gmail.com

A parser and interpreter writen in haskell, with no external libraries (except the small `simpleParse` library)
MiniRuby is a homemade language that kinda feels like ruby, without being ruby.

Why?
----
You learn a lot about your language of choice when you try to build it from scratsh.

Why haskell?
------------
I find that haskell is the best tool for the job when implementing programming languages from scratsh.

Grammar
-------
```
Program          ::= ClassDecls
ClassDecls       ::= E
                   | ClassDecl ClassDecls
ClassDecl        ::= ’class’ Name ’{’ ConstructorDecl NamedMethodDecls RecvDecl ’}’
ConstructorDecl  ::= E
                   | ’new’ ’(’ Params ’)’ ’{’ Exprs ’}’
NamedMethodDecls ::= E
                   | NamedMethodDecl NamedMethodDecls
NamedMethodDecl  ::= ’def’ Name ’(’ Params ’)’ Terminator Exprs ’end’
RecvDecl         ::= E
                   | ’receive’ ’(’ Param ’)’ ’{’ Exprs ’}’
Params           ::= E
                   | Params'
Params'          ::= Param
                   | Param ’,’ Params'   /'
Param            ::= Name
Args             ::= E
                   | Args0
Args0            ::= Expr
                   | Expr ’,’ Args0
Exprs            ::= E
                   | Exprs1
Exprs1           ::= Expr
                   | ExprChained Exprs1
ExprEnd          ::= Expr
                     Expr ';'
ExprChained      ::= Expr Terminator
Terminator       ::= ';'
                   | '\n'
Factor           ::= integer
                   | string
                   | Name
                   | Name ’(’ Args’)’
                   | ’self’
                   | ’return’ Expr
                   | ’@’ Name ’=’ Expr
                   | Name ’=’ Expr
                   | ’match’ Expr ’{’ Cases ’}’
                   | ’send’ ’(’ Expr ’,’ Expr ’)’
                   | ’@’ Name
                   | ’new’ Name ’(’ Args ’)’
                   | ’(’ Expr ’)’
Term             ::= Term '*' FactorChain
                   | Term "/" FactorChain
                   | FactorChain.
Expr             ::= Expr '+' Term
                   | Expr "-" Term
                   | Term
FactorChain      ::= Factor Rest
Rest             ::= E
                     ’.’ Name ’(’ Args ’)’ Rest
Cases            ::= E
                   | Case Cases
Case             ::= Pattern ’->’ ’{’ Exprs ’}’
Pattern          ::= integer
                   | string
                   | Name ’(’ Params’)’
                   | Name
```
