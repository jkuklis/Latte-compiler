module AnalyserErrors where

import Control.Monad.State

import AbsLatte

import AnalyserUtility
import AbstractTree

posInfo :: String -> Pos -> String

posInfo what pos@(Just (line, char)) =
    if pos == defaultPos
        then ""
        else
            what ++ " in line " ++ (show line)
            ++ " (at pos " ++ (show char) ++ ")\n"


msgRedefined :: String -> Ident -> Pos -> Pos -> AS ()

msgRedefined what (Ident ident) pos prevPos =
    addError $ what ++ " " ++ ident ++ " redefined!\n"
    ++ (posInfo "Redefined" pos)
    ++ (posInfo "Previously defined" prevPos)


msgFunDefined :: Ident -> Pos -> Pos -> AS ()

msgFunDefined ident pos prevPos =
    msgRedefined "Function" ident pos prevPos


msgClassDefined :: Ident -> Pos -> Pos -> AS ()

msgClassDefined ident pos prevPos =
    msgRedefined "Class" ident pos prevPos


msgNoBaseClass :: Pos -> Ident -> Ident -> AS ()

msgNoBaseClass pos (Ident this) (Ident extended) =
    addError $ "No base class " ++ extended ++ " for: " ++ this ++ "!\n"
    ++ (posInfo "Defined" pos)


msgExtendSelf :: Pos -> Ident -> AS ()

msgExtendSelf pos (Ident this) =
    addError $ "Class " ++ this ++ " extends itself!\n"
    ++ (posInfo "Defined" pos)


msgCyclicDep :: Pos -> Ident -> Ident -> AS ()

msgCyclicDep pos (Ident this) (Ident extended) =
    addError $ "Cyclic dependency for extending " ++ extended
    ++ " in " ++ this ++ "!\n"
    ++ (posInfo "Defined" pos)


msgPrevAttr :: Ident -> Pos -> Pos -> AS ()

msgPrevAttr ident pos prevPos =
    msgRedefined "Class attribute" ident pos prevPos


msgPrevMet :: Ident -> Pos -> Pos -> AS ()

msgPrevMet ident pos prevPos =
    msgRedefined "Class method" ident pos prevPos


msgSameArg :: Ident -> Pos -> Pos -> AS ()

msgSameArg ident pos prevPos =
    msgRedefined "Argument" ident pos prevPos


msgMainType :: Type Pos -> Pos -> AS ()

msgMainType type_ pos =
    addError $ "Incorrect main type: " ++ (showType type_) ++ "!\n"
    ++ (posInfo "Defined" pos)


msgMainArgs :: Pos -> AS ()

msgMainArgs pos =
    addError $ "Function main argument list not empty!\n"
    ++ (posInfo "Defined" pos)


msgNoMain :: AS ()

msgNoMain =
    addError $ "Function main not defined!\n"


msgNoReturn :: Ident -> Pos -> AS ()

msgNoReturn (Ident ident) pos =
    addError $ "Function " ++ ident ++ " might not return!\n"
    ++ (posInfo "Defined" pos)


msgVarUndeclared :: Ident -> Pos -> AS ()

msgVarUndeclared (Ident ident) pos =
    addError $ "Variable " ++ ident ++ " undeclared!\n"
    ++ (posInfo "Assigned" pos)


msgIncr :: Ident -> Pos -> Pos -> Type Pos -> AS ()

msgIncr (Ident ident) pos prevPos type_ =
    addError $ "Variable " ++ ident ++ " cannot be incremented/decremented\n"
    ++ (posInfo "Used" pos)
    ++ "It is of type " ++ (showType type_) ++ "\n"
    ++ (posInfo "Declared" prevPos)


msgAssign :: Ident -> Pos -> Type Pos -> Pos -> Type Pos -> AS ()

msgAssign (Ident ident) pos vType prevPos eType =
    addError $ "Incorrect type in " ++ ident ++ " assignment!\n"
    ++ (posInfo "Assignment" pos)
    ++ "Variable type: " ++ (showType vType)
    ++ ", expression type: " ++ (showType eType) ++ "\n"
    ++ (posInfo "Defined" prevPos)


msgReturn :: Pos -> Type Pos -> AS ()

msgReturn pos eType = do
    Ident ident <- gets curFun
    rType <- gets retType
    addError $ "Incorrect type in " ++ ident ++ " return!\n"
        ++ "Function return type: " ++ (showType rType)
        ++ ", expression type: " ++ (showType eType) ++ "\n"
        ++ (posInfo "Return" pos)


msgCond :: Pos -> Type Pos -> AS ()

msgCond pos eType =
    addError $ (showType eType) ++ " instead of a boolean in condition!\n"
    ++ (posInfo "Used" pos)


msgExpDecl :: Ident -> Pos -> Type Pos -> Type Pos -> AS ()

msgExpDecl (Ident ident) pos dType eType =
    addError $ "Incorrect expression type in " ++ ident ++ " declaration!\n"
    ++ "Expected: " ++ (showType dType)
    ++ ", got: " ++ (showType eType) ++ "\n"
    ++ (posInfo "Declared" pos)


msgVarDeclared :: Ident -> Pos -> Pos -> AS ()

msgVarDeclared ident pos prevPos =
    msgRedefined "Variable" ident pos prevPos


msgVarUndefined :: Ident -> Pos -> AS ()

msgVarUndefined (Ident ident) pos =
    addError $ "Variable " ++ ident ++ " not defined!!\n"
    ++ (posInfo "Used" pos)


msgClassUndefined :: Ident -> Pos -> AS ()

msgClassUndefined (Ident ident) pos =
    addError $ "Class " ++ ident ++ " not defined!\n"
    ++ (posInfo "Used" pos)


msgNotClass :: Ident -> Type Pos -> Pos -> Pos -> AS ()

msgNotClass (Ident ident) vType pos prevPos =
    addError $ ident ++ " is not an object, it is of type " ++ (showType vType) ++ "!\n"
    ++ (posInfo "Used" pos)
    ++ (posInfo (ident ++ " declared") prevPos)


msgAttributeUndefined :: Pos -> Ident -> Ident -> AS ()

msgAttributeUndefined pos (Ident object) (Ident attr) =
    addError $ "No attribute " ++ attr
    ++ " in class " ++ object
    ++ (posInfo "Used" pos)


msgAttrType :: Pos -> Ident -> Ident -> Type Pos -> Type Pos -> AS ()

msgAttrType pos (Ident classIdent) (Ident attr) attrType eType =
    addError $ "Incompatible types for class " ++ classIdent
    ++ " attribute " ++ attr ++ "!\n"
    ++ "Attribute of type: " ++ (showType attrType)
    ++ " , expression of type: " ++ (showType eType) ++ "\n"
    ++ (posInfo "Used" pos)

msgNotVoidReturn :: Pos -> AS ()

msgNotVoidReturn pos = do
    Ident ident <- gets curFun
    addError $ "Non-void return in a void function " ++ ident ++ "!\n"
        ++ (posInfo "Return" pos)


msgVoidReturn :: Pos -> AS ()

msgVoidReturn pos = do
    Ident ident <- gets curFun
    addError $ "Void return in a non-void function " ++ ident ++ "\n"
        ++ (posInfo "Return" pos)


msgFunUndefined :: Ident -> Pos -> AS ()

msgFunUndefined (Ident ident) pos =
    addError $ "Function " ++ ident ++ " not defined!\n"
    ++ (posInfo "Invoked" pos)


msgTooFewArgs :: Ident -> Pos -> [Arg Pos] -> AS ()

msgTooFewArgs (Ident ident) pos args =
    addError $ "Too few arguments in function " ++ ident ++ " invocation!\n"
    ++ "Expected " ++ (show (length args)) ++ " more\n"
    ++ (posInfo "Invoked" pos)


msgTooManyArgs :: Ident -> Pos -> [Expr Pos] -> AS ()

msgTooManyArgs (Ident ident) pos exprs =
    addError $ "Too many arguments in function " ++ ident ++ " invocation!\n"
    ++ "Expected " ++ (show (length exprs)) ++ " less\n"
    ++ (posInfo "Invoked" pos)


msgArgType :: Pos -> Ident -> Ident -> Type Pos -> Type Pos -> AS ()

msgArgType pos (Ident ident) (Ident aIdent) aType eType =
    addError $ "Mismatched type for paramater " ++ aIdent
    ++ " in function " ++ ident ++ " invocation!\n"
    ++ "Expected: " ++ (showType aType)
    ++ ", got: " ++ (showType eType) ++ "\n"
    ++ (posInfo "Invoked" pos)


msgNeg :: Pos -> Type Pos -> AS ()

msgNeg pos eType =
    addError $ "Not an int to negate"
    ++ ", got: " ++ (showType eType) ++ "\n"
    ++ (posInfo "Used" pos)


msgNot :: Pos -> Type Pos -> AS ()

msgNot pos eType =
    addError $ "Not a bool for not operation argument"
    ++ ", got: " ++ (showType eType) ++ "\n"
    ++ (posInfo "Used" pos)


whichStr :: Int -> String

whichStr which =
    if which == 1
        then "first"
        else "second"

msgTwoOperands :: Pos -> Int -> Type Pos -> String -> String -> AS ()

msgTwoOperands pos which eType expType oper =
    addError $ "Not " ++ expType
    ++ " for " ++ oper ++ " operation "
    ++ (whichStr which) ++ " argument"
    ++ ", got: " ++ (showType eType) ++ "\n"
    ++ (posInfo "Used" pos)


msgMul :: Pos -> Int -> Type Pos -> AS ()

msgMul pos which eType =
    msgTwoOperands pos which eType "int" "mul"


msgAnd :: Pos -> Int -> Type Pos -> AS ()

msgAnd pos which eType =
    msgTwoOperands pos which eType "bool" "and"


msgOr :: Pos -> Int -> Type Pos -> AS ()

msgOr pos which eType =
    msgTwoOperands pos which eType "bool" "or"


msgMinus :: Pos -> Int -> Type Pos -> AS ()

msgMinus pos which eType =
    msgTwoOperands pos which eType "int" "subtraction"


msgAddType :: Pos -> AS ()

msgAddType pos =
    addError $ "Can't deduce addition return type!\n"
    ++ (posInfo "Addition" pos)


msgAddInt :: Pos -> Int -> Type Pos -> AS ()

msgAddInt pos which eType =
    msgTwoOperands pos which eType "int" "addition"


msgAddStr :: Pos -> Int -> Type Pos -> AS ()

msgAddStr pos which eType =
    msgTwoOperands pos which eType "string" "concatenation"


msgRelInt :: Pos -> Int -> Type Pos -> AS ()

msgRelInt pos which eType =
    msgTwoOperands pos which eType "int" "integer comparison"


msgEqType :: Pos -> Type Pos -> Type Pos -> AS ()

msgEqType pos eType1 eType2 =
    addError $ "Mismatched types in equality comparison!\n"
    ++ "Expected: " ++ (showType eType1)
    ++ ", got: " ++ (showType eType2) ++ "\n"
    ++ (posInfo "Compared" pos)


msgIntTooSmall :: Pos -> Integer -> AS ()

msgIntTooSmall pos int =
    addError $ "Integer smaller than minimal int: " ++ (show int) ++ "\n"
    ++ (posInfo "Used" pos)


msgIntTooBig :: Pos -> Integer -> AS ()

msgIntTooBig pos int =
    addError $ "Integer bigger than maximum int: " ++ (show int) ++ "\n"
    ++ (posInfo "Used" pos)


msgVoidArg :: Pos -> Ident -> AS ()

msgVoidArg pos (Ident ident) = do
    Ident fun <- gets curFun
    addError $ "Void argument " ++ ident
        ++ " in function " ++ fun ++ " declaration!\n"
        ++ (posInfo "Declared" pos)


msgVoidVar :: Pos -> AS ()

msgVoidVar pos =
    addError $ "Void variable declaration!\n"
    ++ (posInfo "Declaration" pos)


msgDivZero :: Pos -> AS ()

msgDivZero pos =
    addError $ "Dividing by zero!\n"
    ++ (posInfo "Division" pos)


msgModZero :: Pos -> AS ()

msgModZero pos =
    addError $ "Modulo zero!\n"
    ++ (posInfo "Modulo" pos)


msgQuote :: Pos -> Ident -> AS ()

msgQuote pos (Ident ident) =
    addError $ "Single quote in name: " ++ ident ++ "\n"
    ++ (posInfo "Used" pos)

msgVoidComp :: Pos -> AS ()

msgVoidComp pos =
    addError $ "Void comparison!\n"
    ++ (posInfo "Compared" pos)
