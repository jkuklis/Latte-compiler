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

-- registers =
--     ["rax", "rcx", "rdx",
--         "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10", "r11"]

registers = ["eax", "ecx", "edx"] -- , "ebx", "esi", "edi]


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
        ebp = "\tpushl    %ebp"
        esp = "\tmovl     %esp, %ebp"
        stack = "..stack_holder"
        prolog = reverse [globl, fun, ebp, esp, stack]

    in modify $ \s -> s { code = concat [prolog, (code s)] }


addArgs :: [Arg_] -> CS ()

addArgs args = do
    foldM addArg 8 args
    return ()


addArg :: Integer -> Arg_ -> CS Integer

addArg pos arg = return $ pos + 4
