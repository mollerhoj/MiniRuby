MiniRuby
============
By Jens Dahl Møllerhøj, mollerhoj3@gmail.com

A parser and an interpreter writen in haskell, with no external libraries (except the for the tiny `simpleParse` library)
MiniRuby is a homemade language that kinda feels like ruby.

Motivation
----
You learn a lot about your language of choice when you try to build it from scratsh.
I find that haskell is the best tool for the job when implementing programming languages from scratsh.

Example
-----
An example program in MiniRuby
```
class Observable
  def initialize (value)
    @value = value;
    @observers = nil();
  end

  def addObserver (obj, cookie)
    @observers = cons(observer(obj, cookie), @observers);
  end

  def setValue (value)
    @value = value;
    self.notifyObservers(@observers);
  end

  def notifyObservers (xs)
    case xs
      when cons(x, xs)
        "Note - new xs shadows the old one";
        case x
          when observer(obj, cookie)
            obj.notify(cookie, @value);
        end
        self.notifyObservers(xs);
    end
  end 
end

class Observer
  def notify(cookie, initializeval)
    "Changed:".puts();
    cookie.puts();
    "New value".puts();
    initializeval.puts();
  end
end

class Main
  def initialize ()
    box = new Observable(0);
    obs1 = new Observer();
    obs2 = new Observer();
    box.setValue(1);
    box.addObserver(obs1, obs1());
    box.setValue(2);
    "".puts();
    box.addObserver(obs2, obs2());
    box.setValue(3);
  end
end
```

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
