module MiniRubyInterpreter
       ( runProg
       , RuntimeError (..)
       )
       where

import MiniRubyAST

import Control.Applicative
import Control.Monad
import Data.List

import qualified Data.Map as Map

data RuntimeError = RuntimeError {
  unRuntimeError :: String 
} deriving (Show, Eq)

-- | Give the printed representation of a value.
printed :: Value -> String
printed (IntValue x) = show x
printed (StringValue s) = s
printed (SymbolValue s) = s
printed (BooleanValue b) = show b ++ "!"
printed (ReferenceValue ref) = "#<object " ++ show ref ++ ">"

type Store = Map.Map
type ObjectFields = Store Name Value

type ObjectStore = Store ObjectReference ObjectState

type MethodVariables = Store Name Value

-- | The global state of the program execution.
data GlobalState = GlobalState {
                     prog :: Prog,
                     output :: String,
                     store :: ObjectStore,
                     storeIndex :: Int
                   } deriving (Show)

-- | The state of a single object.
data ObjectState = ObjectState {
                     klassName :: Name,
                     fields :: ObjectFields
                   } deriving (Show)

-- | The state of a method execution.
data MethodState = MethodState {
                     globalState :: GlobalState, 
                     current :: ObjectReference,
                     vars :: MethodVariables
                   } deriving (Show)

-- | The basic monad in which execution of a MiniRuby program takes place.
-- Maintains the global state, the running output, and whether or not
-- an error has occurred.
data MiniRubyM a = MiniRubyM {
  runMiniRubyM :: GlobalState -> Either RuntimeError (a,GlobalState)
}

instance Functor MiniRubyM where
  fmap = liftM

instance Applicative MiniRubyM where
  pure = return
  (<*>) = ap

instance Monad MiniRubyM where
  return x = MiniRubyM (\s -> Right (x,s))
  (MiniRubyM h) >>= f = MiniRubyM $ \s -> case h s of 
                                          Left x -> Left x
                                          Right (a,s') -> runMiniRubyM (f a) s'
  fail str = MiniRubyM (\_ -> Left $ RuntimeError str)

getGlobalState :: MiniRubyM GlobalState
getGlobalState = MiniRubyM (\s -> Right (s,s))

putGlobalState :: GlobalState -> MiniRubyM ()
putGlobalState s = MiniRubyM (\_ -> Right ((),s))

getObjectStore :: MiniRubyM ObjectStore
getObjectStore = do s <- getGlobalState
                    return $ store s

putObjectStore :: ObjectStore -> MiniRubyM ()
putObjectStore gs = do s <- getGlobalState
                       putGlobalState (s {store = gs})
                    
-- Cannot get objects we have not created. (Not really an issus for the end users, since we cannot
-- remove objects either..)
lookupObject :: ObjectReference -> MiniRubyM ObjectState
lookupObject objRef = do st <- getObjectStore
                         case Map.lookup objRef st of
                           Nothing -> fail "Object not found."
                           Just x -> return x

setObject :: ObjectReference -> ObjectState -> MiniRubyM ()
setObject objRef os = do oldStore <- getObjectStore
                         putObjectStore $ Map.insert objRef os oldStore

getObjectState :: MiniRubyMethodM ObjectState
getObjectState = MiniRubyMethodM $ \s -> do os <- lookupObject (current s)
                                            return (os,s)

-- putObjectState :: ObjectState -> MiniRubyMethodM ()
-- putObjectState s' = MiniRubyMethodM $ \s -> liftMiniRubyM $ setObject (current s) s'

putObjectState :: ObjectState -> MiniRubyMethodM ()
putObjectState s' = MiniRubyMethodM $ \ms -> let gs = (globalState ms)
                                                 oldStore = (store gs)
                                                 store' = Map.insert (current ms) s' oldStore
                                                 gs' = gs {store = store'}
                                                 ms' = ms {globalState = gs'}
                                                 in return ((),ms')

                                           
allocUniqID :: MiniRubyM ObjectReference
allocUniqID = do s <- getGlobalState
                 putGlobalState (s {storeIndex = storeIndex s + 1})
                 return $ storeIndex s

-- | The monad in which methods (and constructors and receive actions)
-- execute.  Runs on top of 'MiniRubyM' - maintains the reference to self,
-- as well as the method variables.
data MiniRubyMethodM a = MiniRubyMethodM {
  runMiniRubyMethodM :: MethodState -> MiniRubyM (a,MethodState)
}

instance Functor MiniRubyMethodM where
  fmap = liftM

instance Applicative MiniRubyMethodM where
  pure = return
  (<*>) = ap

-- | The monad in which methods (and constructors and receive actions)
-- execute.  Runs on top of 'MiniRubyM' - maintains the reference to self,
-- as well as the method variables.
--
-- Note that since MiniRubyMethodM runs on top of MiniRubyM, a MiniRubyMethodM
-- action has access to the global state (through liftMiniRubyM).
instance Monad MiniRubyMethodM where
  return x = (liftMiniRubyM . return) x --MiniRubyMethodM (\s -> MiniRubyM (\z -> Right ((x,s),z)))
  fail x = (liftMiniRubyM . fail) x -- MiniRubyMethodM (\_ -> MiniRubyM (\_ -> Left $ RuntimeError x))

  (MiniRubyMethodM h) >>= f = MiniRubyMethodM $ \ms -> case runMiniRubyM (h ms) (globalState ms) of
                                                   Left str -> fail (unRuntimeError str)
                                                   Right ((x,s'),_) -> runMiniRubyMethodM (f x) s'

-- | Perform a 'MiniRubyM' operation inside a 'MiniRubyMethodM'.
liftMiniRubyM :: MiniRubyM a -> MiniRubyMethodM a
liftMiniRubyM monad = MiniRubyMethodM (\ms -> case (runMiniRubyM monad) (globalState ms) of
                                         Left x -> fail (unRuntimeError x)
                                         Right (v,gs) -> MiniRubyM (\gs' -> Right ((v,ms {globalState = gs} ),gs')))
  

-- | Who are we?
askSelf :: MiniRubyMethodM ObjectReference
askSelf = do s <- getMethodState
             return (current s)

-- bind variables
bindVars :: [Name] -> [Value] -> MiniRubyMethodM Value
bindVars [n]    [v]    = bindVar (n,v)
bindVars (n:ns) (v:vs) = do bindVar (n,v)
                            bindVars ns vs
bindVars _ _ = fail "Wrong number of arguments in `case`"


bindVar :: (Name,Value) -> MiniRubyMethodM Value
bindVar (name,value) = do s <- getMethodState
                          putMethodState (s {vars = Map.insert name value (vars s)})
                          return value

getVar :: Name -> MiniRubyMethodM Value
getVar name = do ms <- getMethodState
                 case Map.lookup name (vars ms) of
                   Nothing -> fail $ "Variable or parameter `" ++ name ++ "` could not be found"
                   Just x -> return x

getMethodState :: MiniRubyMethodM MethodState
--getMethodState = undefined
--getMethodState = MiniRubyMethodM (\s -> Right (s,s))
getMethodState = MiniRubyMethodM (\s -> MiniRubyM (\z -> Right ((s,s),z)))

putMethodState :: MethodState -> MiniRubyMethodM ()
putMethodState s = MiniRubyMethodM (\_ -> MiniRubyM (\z -> Right (((),s),z)))
--putMethodState s = MiniRubyMethodM (\_ -> Right((),s))

-- | Find the declaration of the class with the given name, or cause
-- an error if that name is not a class.
findClassDecl :: Name -> MiniRubyM ClassDecl
findClassDecl name = do s <- getGlobalState
                        case findClassDeclHelper name (prog s) of
                          Nothing -> fail $ "Could not find a `" ++ name ++ "` class."
                          Just x -> return x

findClassDeclHelper :: Name -> [ClassDecl] -> Maybe ClassDecl
findClassDeclHelper _ [] = Nothing
findClassDeclHelper name (e:es) = if (className e) == name
                                  then Just e
                                  else findClassDeclHelper name es

-- | Instantiate the class with the given name, passing the given
-- values to the constructor.
createObject :: Name -> MiniRubyM ObjectReference
createObject name = do i <- allocUniqID
                       setObject i (ObjectState {klassName = name, fields = Map.empty})
                       return i

getConstructor :: ClassDecl -> ConstructorDecl
getConstructor cld = case (classConstructor cld) of 
                          Nothing -> emptyContructorDecl
                          Just x -> x

emptyContructorDecl :: ConstructorDecl
emptyContructorDecl = MethodDecl { methodParameters = [],
                                   methodBody = []
                                 }

getMethod :: ClassDecl -> Name -> Maybe MethodDecl
getMethod cld methodName = getMethodHelper methodName (classMethods cld)

getMethodHelper :: Name -> [NamedMethodDecl] -> Maybe MethodDecl
getMethodHelper _ [] = Nothing
getMethodHelper name ((NamedMethodDecl name2 methodDecl):es) = if name == name2
                                                               then Just methodDecl
                                                               else getMethodHelper name es

----------------- Evaluate expressions ------------------

evalExprs :: [Expr] -> MiniRubyMethodM Value
evalExprs [] = return $ IntValue 0
evalExprs [e] = evalExpr e
evalExprs ((Return e):_) = evalExpr e 
evalExprs (e:es) = evalExpr e >> evalExprs es

evalExprsEach :: [Expr] -> MiniRubyMethodM [Value]
evalExprsEach [] = return []
evalExprsEach [e] = do v <- evalExpr e
                       return [v]
evalExprsEach (e:es) = do v <- evalExpr e 
                          vs <- evalExprsEach es
                          return (v:vs)

evalArit :: (Integer -> Integer -> Integer) -> Expr -> Expr -> MiniRubyMethodM Value
evalArit f e1 e2 = do v1 <- evalExpr e1
                      v2 <- evalExpr e2
                      case (v1,v2) of
                        (IntValue v1',IntValue v2') -> return $ IntValue $ f v1' v2' 
                        _ -> fail "Cannot do arithmetic with non-integers"

evalBool :: (Integer -> Integer -> Bool) -> Expr -> Expr -> MiniRubyMethodM Value
evalBool f e1 e2 = do v1 <- evalExpr e1
                      v2 <- evalExpr e2
                      case (v1,v2) of
                        (IntValue v1',IntValue v2') -> return $ BooleanValue $ f v1' v2' 
                        _ -> fail "Cannot do comparison with non-integers"

evalExpr :: Expr -> MiniRubyMethodM Value
evalExpr (Minus e1 e2) = evalArit (-) e1 e2
evalExpr (Plus e1 e2) = evalArit (+) e1 e2
evalExpr (Times e1 e2) = evalArit (*) e1 e2
evalExpr (DividedBy e1 e2) = evalArit div e1 e2
evalExpr (LessThan e1 e2) = evalBool (<) e1 e2
evalExpr (GreaterThan e1 e2) = evalBool (>) e1 e2
evalExpr (IntConst i) = return $ IntValue i
evalExpr (StringConst str) = return $ StringValue str
evalExpr (BooleanConst b) = return $ BooleanValue b

evalExpr (SetVar varName e1) = do varValue <- evalExpr e1
                                  bindVar (varName,varValue)

evalExpr (SetField fieldName e1) = do fieldValue <- evalExpr e1
                                      os <- getObjectState
                                      putObjectState (os {fields = Map.insert
                                                          fieldName fieldValue (fields os) })
                                      return fieldValue

evalExpr (ReadField fieldName) = do os <- getObjectState
                                    case Map.lookup fieldName (fields os) of
                                       Nothing -> fail $ "Field `" ++ fieldName ++ "` could not be found"
                                       Just x -> return x

evalExpr (ReadVar varName) = getVar varName

evalExpr (New className' exprParams) = do 
                        args <- evalExprsEach exprParams
                        i <- liftMiniRubyM $ createObject className'
                        cd <- liftMiniRubyM $ findClassDecl className'
                        params <- assignVars (methodParameters $ getConstructor cd) args
                        -- _ is the result of the constructor method. It is discarded.
                        _ <- liftMiniRubyM $ evalMethodBody i params (methodBody $ getConstructor cd)
                        return $ ReferenceValue i

evalExpr (CallMethod receiverExpr "send" (exprParam:exprParams)) = do (StringValue methodName) <- evalExpr exprParam
                                                                      args <- evalExprsEach exprParams
                                                                      objectRef <- evalExpr receiverExpr
                                                                      methodCall objectRef methodName args

evalExpr (CallMethod receiverExpr methodName exprParams) = do args <- evalExprsEach exprParams
                                                              objectRef <- evalExpr receiverExpr
                                                              methodCall objectRef methodName args

evalExpr (Self) = do v <- askSelf
                     return $ ReferenceValue v 

evalExpr (Match expr cases) = do v <- evalExpr expr
                                 patternMatch v cases

evalExpr (Return expr ) = evalExpr expr

evalExpr x = fail $ "Using unpermitted expression: " ++ show x

patternMatch :: Value -> Cases -> MiniRubyMethodM Value
patternMatch v ((p,exprs):cs) = case (v,p) of
                                  (IntValue i,ConstInt j) -> if i == j
                                                             then evalExprs exprs
                                                             else patternMatch v cs
                                  (StringValue i,ConstString j) -> if i == j
                                                                   then evalExprs exprs
                                                                   else patternMatch v cs
                                  (v',AnyValue k) -> do bindVar (k,v')
                                                        evalExprs exprs
                                  (_,_)          -> patternMatch v cs

methodCall :: Value -> Name -> [Value] -> MiniRubyMethodM Value
methodCall receiver "puts" [v] = puts v
methodCall (ReferenceValue objRef) methodName args = do
                className' <- lookupClassName objRef
                cd <- liftMiniRubyM $ findClassDecl className'
                case getMethod cd methodName of
                  Nothing -> 
                    case classReceive cd of
                      Nothing -> fail "No receive method defined"
                      Just x -> do params <- assignVars (receiveParameters x) (StringValue methodName:args)
                                   -- After a methodcall, we discard the methodstate.
                                   (methodReturn,_) <- liftMiniRubyM $ evalMethodBody objRef params (receiveBody x)
                                   return methodReturn
                  Just x ->
                    do params <- assignVars (methodParameters x) args
                       -- After a methodcall, we discard the methodstate.
                       (methodReturn,_) <- liftMiniRubyM $ evalMethodBody objRef params (methodBody x)
                       return methodReturn

methodCall _ _ _ = fail "Cannot call methods on non-objects. (only puts)"

assignVars :: [Name] -> [Value] -> MiniRubyMethodM MethodVariables
assignVars names values = case assignVarsHelper names values Map.empty of
                            Nothing -> fail "Wrong number of arguments!"
                            Just x -> return x

assignVarsHelper :: [Name] -> [Value] -> MethodVariables -> Maybe MethodVariables
assignVarsHelper [] [] assigned = Just assigned
assignVarsHelper _ [] _         = Nothing
assignVarsHelper [] _ _         = Nothing
assignVarsHelper (n:ns) (v:vs) assigned = assignVarsHelper ns vs (Map.insert n v assigned)

-- | Evaluate a method body - the passed arguments are the object in
-- which to run, the initial variable bindings (probably the
-- parameters of the method, constructor or receive action), and the
-- body.  Returns a value and the new state of the object.
evalMethodBody :: ObjectReference -> MethodVariables -> Exprs -> MiniRubyM (Value, MethodState)
evalMethodBody obj variables e = do 
                            st <- getGlobalState 
                            os <- lookupObject obj
                            (v,ms) <- runMiniRubyMethodM (evalExprs e) $ MethodState { current = obj,
                                                                                   globalState = st,
                                                                                   vars = variables }
                            putGlobalState (globalState ms)
                            return (v,ms)

-- function that goes from objRef to className 
lookupClassName :: ObjectReference -> MiniRubyMethodM Name
lookupClassName objRef = do objState <- liftMiniRubyM $ lookupObject objRef
                            return (klassName objState)

puts :: Value -> MiniRubyMethodM Value
puts v = do s <- liftMiniRubyM getGlobalState
            liftMiniRubyM $ putGlobalState (s {output = (output s) ++ printed v ++ "\n"})
            return v

-- runProg :: Prog -> Either RuntimeError String
runProg :: Prog -> Either RuntimeError String
runProg p = case starter p of
              Right ((_,_), gs) -> Right (output gs)
              Left x -> Left x

scanForDoubleClassNames :: Prog -> MiniRubyM ()
scanForDoubleClassNames p = if allClassNames p /= (nub (allClassNames p))
                            then fail "There are dublicate class definitions"
                            else return ()

allClassNames :: Prog -> [Name]
allClassNames p = fmap className p

starter :: Prog -> Either RuntimeError ((Value, MethodState), GlobalState)
starter p = let monad = do 
                 scanForDoubleClassNames p  
                 cd <- findClassDecl "Main" 
                 objRef <- createObject "Main" 
                 result <- evalMethodBody objRef Map.empty (methodBody $ getConstructor cd) 
                 return result 
            in runMiniRubyM monad $ initGlobalState p 

initGlobalState :: Prog -> GlobalState
initGlobalState p = GlobalState {
  prog = p,
  output = "",
  store = Map.empty,
  storeIndex = 0
}
