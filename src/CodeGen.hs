module CodeGen (emitProgram, runEmitter) where

import Ast
import Control.Monad.Identity (Identity)
import Control.Monad.Reader
import Control.Monad.Writer

type Asm = String

newtype Env = Env {labelCount :: Int}

type CodeGen = WriterT Asm Identity

-- type CodeGen = Writer String ()

emit :: String -> CodeGen ()
emit line = tell (line <> "\n")

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

emitBinop :: String -> Expr -> Expr -> CodeGen ()
emitBinop op left right = do
  emitExpr left -- result in r0
  emit "  push {r0, ip}" -- ip is dummy register
  emitExpr right -- result in r0
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

runEmitter :: CodeGen () -> String
runEmitter = execWriter