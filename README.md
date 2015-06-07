MiniRuby
============
By Jens Dahl Møllerhøj, mollerhoj3@gmail.com

A parser and an interpreter writen in haskell, with no external libraries (except the for the tiny `simpleParse` library)
MiniRuby is a homemade language that kinda feels like ruby.

Motivation
----
You learn a lot about your language of choice when you try to build it from scratch.
I find that haskell is the best tool for the job when implementing programming languages from scratch.

Example
-----
An example program in MiniRuby
```
class Main
  def initialize()
    jens = new Person();

    y = 3

    @some_value = 4 + 2 - 20 / (7 + y) + 1 * 2 

    jens.send("say","1")
    jens.send("run","fast")

    switch("password")
    self.switch("invalid")

    x = test_return()
    puts(x)

    ivar()

    list = new Cons("A",new Cons("B", new Cons("C", new Cons(7,0))))

    puts(list.get(3))
    list.put(3,8)
    puts(list.get(3))

    puts(2 < 1 + 1 + 1);
    puts(2 > 3);
  end

  def switch(x)
    case x
      when "password"
        puts("3")
      when x
        puts("4")
    end
  end

  def test_return()
    return 5
    puts(0)
  end

  def ivar()
    puts(@some_value)
  end
end

class Cons
  def initialize(x,xs)
    @element = x
    @rest = xs
  end

  def get(i)
    case i
      when 0
        return @element
      when x
        return @rest.get(i-1)
    end
  end

  def put(i, value)
    case i
      when 0
        @rest = new Cons(@element, @rest)
        @element = value
      when x
        return @rest.put(i-1, value)
    end
  end
end

class Person
  def say(x)
    puts(x)
  end

  def method_missing(methodName,x)
    puts("2")
  end
end
```

Run
---
Here is how to run the program above:
`runhaskell MiniRuby examples/sending.rb`


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
