module CompilerUtility where

import Control.Monad.State

import qualified Data.Map as M

import AbstractTree


type RegMap = M.Map String Bool

type VarMap = M.Map String String

data CompilerState = CompilerState {
    code :: [String],
    regs :: RegMap,
    locVars :: VarMap,
    outVars :: VarMap
    } deriving Show

type CS a = State CompilerState a

-- registers =
--     ["rax", "rcx", "rdx",
--         "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10", "r11"]

registers = ["eax", "ecx", "edx"] -- , "ebx", "esi", "edi]
registersMap = foldr (\reg m -> M.insert reg True m) M.empty registers
stack = "ebp"
frame = "esp"

startState = CompilerState {
    code = [],
    regs = registersMap,
    locVars = M.empty,
    outVars = M.empty
    }


addFun :: Ident_ -> CS ()

addFun (Ident_ ident) =
    let
        globl = "\n.globl " ++ ident
        fun = ident ++ ":"
        stackPtr = "\tpushl    %" ++ stack
        framePtr = "\tmovl     %" ++ frame ++ ", %" ++ stack
        stackOff = "..stack_holder"
        prolog = reverse [globl, fun, stackPtr, framePtr, stackOff]

    in modify $ \s -> s { code = concat [prolog, (code s)] }


addArgs :: [Arg_] -> CS ()

addArgs args = foldM_ addArg 8 args


addArg :: Integer -> Arg_ -> CS Integer

addArg pos (Arg_ type_ (Ident_ ident)) = do
    let reg = (show pos) ++ "(%" ++ stack ++ ")"
    modify $ \s -> s { outVars = M.insert ident reg (outVars s)}
    return $ pos + 4


clearArgs :: CS ()

clearArgs = modify $ \s -> s {
    outVars = M.empty,
    locVars = M.empty,
    regs = registersMap }


addBlock :: Block_ -> CS ()

addBlock (Block_ stmts) = addStmts stmts


addStmts :: [Stmt_] -> CS ()

addStmts [] = return ()

addStmts (stmt:stmts) =
    case stmt of
        _ -> return ()
