module CodeGen (emitProgram, runEmitter) where

import Ast
import Control.Monad.State
import Control.Monad.Writer
import Data.Map (Map)

type Asm = String
type Offset = Integer

newtype Env = Env {
  labelCount :: Int,
  locals :: Map Name Offset
}

type CodeGen = StateT Env (Writer Asm)

emit :: String -> CodeGen ()
emit line = tell (line <> "\n")

label :: CodeGen String
label = do
  env <- get
  let count = labelCount env
  put env {labelCount = count + 1}
  return $ "L" <> show count

emitExpr :: Expr -> CodeGen ()
emitExpr (Number n) = emit $ "  ldr r0, =" <> show n
emitExpr (Assert condition) = do
  emitExpr condition -- result in r0
  emit "  cmp r0, #1"
  emit "  moveq r0, #'.'"
  emit "  movne r0, #'F'"
  emit "  bl putchar"
emitExpr (Not expr) = do
  emitExpr expr -- result in r0
  emit "  cmp r0, #0"
  emit "  moveq r0, #1"
  emit "  movne r0, #0"
emitExpr (Add left right) =
  emitBinop "add" left right
emitExpr (Subtract left right) =
  emitBinop "sub" left right
emitExpr (Multiply left right) =
  emitBinop "mul" left right
emitExpr (Divide left right) =
  emitBinop "udiv" left right
emitExpr (Equal left right) = do
  emitExpr left -- result in r0
  emit "  push {r0, ip}" -- ip is dummy register
  emitExpr right -- result in r0
  emit "  pop {r1, ip}"
  emit "  cmp r0, r1"
  emit "  moveq r0, #1"
  emit "  movne r0, #0"
emitExpr (NotEqual left right) = do
  emitExpr left -- result in r0
  emit "  push {r0, ip}" -- ip is dummy register
  emitExpr right -- result in r0
  emit "  pop {r1, ip}" -- move
  emit "  cmp r0, r1"
  emit "  moveq r0, #0"
  emit "  movne r0, #1"
emitExpr (Block exprs) = do
  mapM_ emitExpr exprs
emitExpr (Call calee []) = do
  emit $ "  bl " <> calee
emitExpr (Call calee [arg]) = do
  emitExpr arg
  emit $ "  bl " <> calee
emitExpr (Call calee args) | length args <= 4 = do
  emit "  sub sp, sp, #16"
  forM_ (zip [0, 4 ..] args) $ \(offset, arg) -> do
    emitExpr arg
    emit $ "  str r0, [sp, #" <> show offset <> "]"
  emit "  pop {r0, r1, r3, r4}"
  emit $ "  bl " <> calee
emitExpr (If condition consequent alternative) = do
  elseLabel <- label
  endLabel <- label

  emitExpr condition
  emit "  cmp r0, #0"
  emit $ "  beq " <> elseLabel
  emitExpr consequent
  emit $ "  b " <> endLabel
  emit $ elseLabel <> ":"
  emitExpr alternative
  emit $ endLabel <> ":"
emitExpr (While condition body) = do
  startLabel <- label
  endLabel <- label

  emit $ startLabel <> ":"
  emitExpr condition
  emit "  cmp r0, #0"
  emit $ "  beq " <> endLabel
  emitExpr body
  emit $ "  b " <> startLabel
  emit $ endLabel <> ":"
emitExpr (Function name params body) = do
  emit ""
  emit $ ".global " <> name
  emit $ name <> ":"
  -- Prologue
  emit "  push {fp, lr}"
  emit "  mov fp, sp" -- Update fp
  emit "  push {r0, r1, r2, r3}"
  -- Body
  emitExpr body
  -- Epilogue
  emit "  mov sp, fp" -- Deallocate stack
  emit "  r0, #0"
  emit "  pop {fp, pc}" 

emitBinop :: String -> Expr -> Expr -> CodeGen ()
emitBinop op left right = do
  emitExpr left
  emit "  push {r0, ip}"
  emitExpr right
  emit "  pop {r1, ip}"
  emit $ "  " <> op <> " r0, r1, r0"

emitProgram :: Program -> CodeGen ()
emitProgram (Program statements) = do
  emit ".global main"
  emit "main:"
  emit "  push {fp, lr}"
  mapM_ emitExpr statements
  emit "  mov r0, #0"
  emit "  pop {fp, pc}"

initEnv :: Env
initEnv = Env {labelCount = 0}

runEmitter :: CodeGen a -> String
runEmitter cg = execWriter $ runStateT cg initEnv