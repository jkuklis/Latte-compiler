module CompilerUtility where

import Control.Monad.State

import qualified Data.Map as M

import AbstractTree


type RegMap = M.Map String Bool

type VarMap = M.Map String String

data CompilerState = CompilerState {
    code :: [String],
    regs :: RegMap,
    vars :: VarMap
    } deriving Show

type CS a = State CompilerState a

registers =
    ["rax", "rcx", "rdx",
        "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10", "r11"]

startState = CompilerState {
    code = [],
    regs = foldr (\reg m -> M.insert reg True m) M.empty registers,
    vars = M.empty
    }


addFun :: Ident_ -> CS ()

addFun (Ident_ ident) =
    let
        globl = "\n.globl " ++ ident
        fun = ident ++ ":"
        rbp = "\tpushq    %rbp"
        rsp = "\tmovq     %rsp, %rbp"
        stack = "..stack_holder"
        prolog = reverse [globl, fun, rbp, rsp, stack]

    in modify $ \s -> s { code = concat [prolog, (code s)] }


addArgs :: [Arg_] -> CS ()

addArgs args = forM_ args addArg


addArg :: Arg_ -> CS ()

addArg arg = return ()
