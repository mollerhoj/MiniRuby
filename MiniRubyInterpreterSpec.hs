import MiniRubyAST
import MiniRubyInterpreter
import Data.String

-- Run the tests


-- Can evaluate simple expressions
test1 = runProg $ simpleExpr (DividedBy (Times (Plus (IntConst 3) (IntConst 4)) (IntConst 2)) (IntConst 3))
res1 = Right ("4\n")

-- Can print Self
test5 = runProg $ simpleExpr (Self)
res5 = Right ("#<object 0>\n")

-- Can set and read fields
test7 = runProg $ simpleMethod [(SetField "x" (IntConst 10)),(CallMethod (ReadField "x") "puts" [])]
res7 = Right ("10\n")

-- Can set and read vars
test8 = runProg $ simpleMethod [(SetVar "x" (IntConst 10)),(CallMethod (ReadVar "x") "puts" [])]
res8 = Right ("10\n")

-- Can match against strings
test10 = runProg $ simpleMethod [ Match (StringConst "x") [ (ConstString "x", [(CallMethod (IntConst 1) "puts" [])]) ] ]
res10 = Right ("1\n")

-- Can match against ints
test11 = runProg $ simpleMethod [ Match (IntConst 1) [ (ConstInt 1, [(CallMethod (IntConst 1) "puts" [])]) ] ]
res11 = Right ("1\n")

-- Cannot match against unequal ints
test14 = runProg $ simpleMethod [ Match (IntConst 2) [ (ConstInt 1, [(CallMethod (IntConst 1) "puts" [])]) ] ]
res14 = Right ("")

-- Can match against anyvalues, and they are bound
test12 = runProg $ simpleMethod [ Match (IntConst 1) [ (AnyValue "k", [(CallMethod (ReadVar "k") "puts" [])]) ] ]
res12 = Right ("1\n")

-- Can match against anyvalues, and they are bound
test21 = runProg $ simpleMethod [ Match (TermLiteral "foo" [(StringConst "hej"),(StringConst "Jens")]) [ (TermPattern "foo" ["a","b"], [(CallMethod (ReadVar "b") "puts" [])]) ] ]
res21 = Right ("Jens\n")

-- Cannot match strings against ints
test13 = runProg $ simpleMethod [ Match (StringConst "1") [ (ConstInt 1, [(CallMethod (IntConst 1) "puts" [])]) ] ]
res13 = Right ("")

-- Can call a method in the main class
test15 = runProg $ simpleMethod2 [(CallMethod (Self) "SayTwo" [(IntConst 1),(IntConst 2)])]
res15 = Right "1\n2\n"

-- Can create a new object
test16 = runProg $ simpleProgram [(New "Person" [(StringConst "Mr.")])]
res16 = Right "My title is:\nMr.\n"

-- Can create a new object and call its method
test17 = runProg $ simpleProgram [(CallMethod (New "Person" [(StringConst "Mr.")]) "dinner" [(StringConst "burger"),(StringConst"cola")])]
res17 = Right "My title is:\nMr.\neating:\nburger\ndrinkings:\ncola\n"

-- Unknown methods hits the receive method
test23 = runProg $ simpleProgram3
res23 = Right "hej(du)\n"

---- Testing: Errors

-- Cannot call methods of non-object values (only puts)
test24 = runProg $ simpleExpr (CallMethod (StringConst "What?") "noMethod" [])
res24 = Left (Error {unError = "Cannot call methods on non-objects. (only puts)" })

-- Cannot call an unknown method, when no receive method is defined.
test22 = runProg $ simpleProgram [(CallMethod (New "Person" [(StringConst "Mr.")]) "jump" [(StringConst "burger"),(StringConst"cola")])]
res22 = Left (Error {unError = "No receive method defined"})

-- Cannot read unset variable
test2 = runProg $ simpleExpr (ReadVar "hej")
res2 = Left (Error "Variable or parameter `hej` could not be found")

-- Cannot read unset field
test9 = runProg $ simpleExpr (ReadField "hej")
res9 = Left (Error "Field `hej` could not be found")

-- Cannot find the class
test3 = runProg $ simpleMethod [(New "Person" [])]
res3 = Left (Error "Could not find a `Person` class.")

-- Wrong number of args
test4 = runProg $ simpleMethod2 [(CallMethod (Self) "SayTwo" [])]
res4 = Left (Error "Wrong number of arguments!")

-- Cannot do arithmetic with non-integers
test6 = runProg $ simpleExpr (Plus (StringConst "x") (IntConst 4))
res6 = Left (Error "Cannot do arithmetic with non-integers")

-- There are doublicate classes
test18 = runProg $ dublicateClassesProgram
res18 = Left (Error "There are dublicate class definitions")

-- Integration test
test19 = runProg $ simpleProgram2
res19 = Right "4\n2\n"

-- Can return with Return
test20 = runProg $ simpleMethod [(Return (CallMethod (StringConst "1") "puts" [])),(CallMethod (StringConst "2") "puts" [])]
res20 = Right "1\n"

-- zend works
test25 = runProg $ simpleProgram4
res25 = Right "tack\nnoMethod(tuck)\n"

---- Main
main = do 
  putStrLn $ show (test1 == res1) ++ ": " ++ (show test1)
  putStrLn $ show (test2 == res2) ++ ": " ++ (show test2)
  putStrLn $ show (test3 == res3) ++ ": " ++ (show test3)
  putStrLn $ show (test4 == res4) ++ ": " ++ (show test4)
  putStrLn $ show (test5 == res5) ++ ": " ++ (show test5)
  putStrLn $ show (test6 == res6) ++ ": " ++ (show test6)
  putStrLn $ show (test7 == res7) ++ ": " ++ (show test7)
  putStrLn $ show (test8 == res8) ++ ": " ++ (show test8)
  putStrLn $ show (test9 == res9) ++ ": " ++ (show test9)
  putStrLn $ show (test10 == res10) ++ ": " ++ (show test10)
  putStrLn $ show (test11 == res11) ++ ": " ++ (show test11)
  putStrLn $ show (test12 == res12) ++ ": " ++ (show test12)
  putStrLn $ show (test13 == res13) ++ ": " ++ (show test13)
  putStrLn $ show (test14 == res14) ++ ": " ++ (show test14)
  putStrLn $ show (test15 == res15) ++ ": " ++ (show test15)
  putStrLn $ show (test16 == res16) ++ ": " ++ (show test16)
  putStrLn $ show (test17 == res17) ++ ": " ++ (show test17)
  putStrLn $ show (test18 == res18) ++ ": " ++ (show test18)
  putStrLn $ show (test19 == res19) ++ ": " ++ (show test19)
  putStrLn $ show (test20 == res20) ++ ": " ++ (show test20)
  putStrLn $ show (test21 == res21) ++ ": " ++ (show test21)
  putStrLn $ show (test22 == res22) ++ ": " ++ (show test22)
  putStrLn $ show (test23 == res23) ++ ": " ++ (show test23)
  putStrLn $ show (test24 == res24) ++ ": " ++ (show test24)
  putStrLn $ show (test25 == res25) ++ ": " ++ (show test25)

---- Utilities
simpleExpr :: Expr -> Prog
simpleExpr expr = [
    ClassDecl {
      className = "Main",
      classConstructor = Just (
        MethodDecl {
          methodParameters = [],
          methodBody = [(CallMethod expr "puts" [])]
        }
      ),
      classMethods = [],
      classReceive = Nothing
    }
  ]

simpleMethod :: Exprs -> Prog
simpleMethod exprs = [
    ClassDecl {
      className = "Main",
      classConstructor = Just (
        MethodDecl {
          methodParameters = [],
          methodBody = exprs 
        }
      ),
      classMethods = [],
      classReceive = Nothing
    }
  ]

----------------- Dublicate classes program

dublicateClassesProgram :: Prog
dublicateClassesProgram = [
    ClassDecl {
      className = "Main",
      classConstructor = Just (
        MethodDecl {
          methodParameters = [],
          methodBody = []
        }
      ),
      classMethods = [],
      classReceive = Nothing
    },

    ClassDecl {
      className = "Main",
      classConstructor = Just (
        MethodDecl {
          methodParameters = [],
          methodBody = []
        }
      ),
      classMethods = [],
      classReceive = Nothing
    }
  ]

----------------------------------------------------

simpleMethod2 :: Exprs -> Prog
simpleMethod2 exprs = [
    ClassDecl {
      className = "Main",
      classConstructor = Just (
        MethodDecl {
          methodParameters = [],
          methodBody = exprs 
        }
      ),
      classMethods = [
        NamedMethodDecl "SayTwo"
        (MethodDecl {
          methodParameters = ["x","y"],
          methodBody = [
            (CallMethod (ReadVar "x") "puts" []),
            (CallMethod (ReadVar "y") "puts" [])
          ]
        })
      ],
      classReceive = Nothing
    }
  ]

--------------------------------------------------

simpleProgram :: Exprs -> Prog
simpleProgram exprs = [
    ClassDecl {
      className = "Main",
      classConstructor = Just (
        MethodDecl {
          methodParameters = [],
          methodBody = exprs 
        }
      ),
      classMethods = [],
      classReceive = Nothing
    },

    ClassDecl {
      className = "Person",
      classConstructor = Just (
        MethodDecl {
          methodParameters = ["title"],
          methodBody = [
            (CallMethod (StringConst "My title is:") "puts" []),
            (CallMethod (ReadVar "title") "puts" [])
          ]
        }
      ),
      classMethods = [
        NamedMethodDecl "dinner"
        (MethodDecl {
          methodParameters = ["food","drinks"],
          methodBody = [
            (CallMethod (StringConst "eating:") "puts" []),
            (CallMethod (ReadVar "food") "puts" []),
            (CallMethod (StringConst "drinkings:") "puts" []),
            (CallMethod (ReadVar "drinks") "puts" [])
          ]
        })
      ],
      classReceive = Nothing
    }
  ]

-------------------------------------------------
--
--

simpleProgram4 = [
    ClassDecl {
      className = "Main",
      classConstructor = Just (
        MethodDecl {
          methodParameters = [],
          methodBody = [
            (CallMethod (New "Clock" []) "zend" [(StringConst "tick"),(StringConst "tack")]),
            (CallMethod (New "Clock" []) "zend" [(StringConst "noMethod"),(StringConst "tuck")])
          ]
        }
      ),
      classMethods = [],
      classReceive = Nothing
    },

    ClassDecl {
      className = "Clock",
      classConstructor = Just (
        MethodDecl {
          methodParameters = [],
          methodBody = [
            (SetField "counter" (IntConst 0))
          ]
        }
      ),
      classMethods = [
        (NamedMethodDecl "tick"
        (MethodDecl {
          methodParameters = ["sound"],
          methodBody = [
            (CallMethod (ReadVar "sound") "puts" [])
          ]
        }))
      ],
      classReceive = Just (
        ReceiveDecl {
        receiveParam = "msg",
        receiveBody = [
            (CallMethod (ReadVar "msg") "puts" [])
        ]
        }
      )
    }
  ]

----

simpleProgram3 = [
    ClassDecl {
      className = "Main",
      classConstructor = Just (
        MethodDecl {
          methodParameters = [],
          methodBody = [
            (CallMethod (New "Clock" []) "hej" [(StringConst "du")])
          ]
        }
      ),
      classMethods = [],
      classReceive = Nothing
    },

    ClassDecl {
      className = "Clock",
      classConstructor = Just (
        MethodDecl {
          methodParameters = [],
          methodBody = [
            (SetField "counter" (IntConst 0))
          ]
        }
      ),
      classMethods = [],
      classReceive = Just (
        ReceiveDecl {
        receiveParam = "msg",
        receiveBody = [
            (CallMethod (ReadVar "msg") "puts" [])
        ]
        }
      )
    }
  ]


--
simpleProgram2 = [
    ClassDecl {
      className = "Main",
      classConstructor = Just (
        MethodDecl {
          methodParameters = [],
          methodBody = [
            (SetVar "x" (New "Clock" [])),
            (SetVar "y" (New "Clock" [])),
            (CallMethod (ReadVar "x") "inc" [(IntConst 1)]),
            (CallMethod (ReadVar "y") "inc" [(IntConst 2)]),
            (CallMethod (ReadVar "x") "inc" [(IntConst 1)]),
            (CallMethod (ReadVar "y") "inc" [(IntConst 2)]),
            (CallMethod (ReadVar "y") "show" []),
            (CallMethod (ReadVar "x") "show" [])
          ]
        }
      ),
      classMethods = [],
      classReceive = Nothing
    },

    ClassDecl {
      className = "Clock",
      classConstructor = Just (
        MethodDecl {
          methodParameters = [],
          methodBody = [
            (SetField "counter" (IntConst 0))
          ]
        }
      ),
      classMethods = [
        (NamedMethodDecl "inc"
        (MethodDecl {
          methodParameters = ["x"],
          methodBody = [
            (SetField "counter" (Plus (ReadField "counter") (ReadVar "x")))
          ]
        })),

        (NamedMethodDecl "show"
        (MethodDecl {
          methodParameters = [],
          methodBody = [
            (CallMethod (ReadField "counter") "puts" [])
          ]
        }))
      ],
      classReceive = Just (
        ReceiveDecl {
        receiveParam = "msg",
        receiveBody = [
        Match (ReadField "log")
        [(TermPattern "true" [],
        [CallMethod
        (StringConst "Method call:")
        "puts" [],
        CallMethod (ReadVar "msg") "puts" []])],
        SendMessage (ReadField "receiver") (ReadVar "msg")
        ]
        }
      )
    }
  ]
