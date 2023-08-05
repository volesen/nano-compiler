module CodeGen (emitProgram, runEmitter) where

import Ast
import Control.Monad.State
import Control.Monad.Writer
import Data.Map (Map, empty, insert, lookup)
import qualified Data.Map as Map

type Asm = String

type Offset = Integer

data Env = Env
  { labelCount :: Int,
    locals :: Map Name Offset,
    nextLocal :: Offset
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

getLocal :: Name -> CodeGen Offset
getLocal name = do
  env <- get
  case Data.Map.lookup name (locals env) of
    Just offset -> return offset
    Nothing -> do
      let offset = 4 * fromIntegral (length (locals env))
      put env {locals = insert name offset (locals env)}
      return offset

setLocal :: Name -> CodeGen ()
setLocal name =
  modify
    ( \env ->
        let offset = nextLocal env
         in env
              { locals = insert name (offset - 4) (locals env),
                nextLocal = offset - 8
              }
    )

emitExpr :: Expr -> CodeGen ()
emitExpr (Number n) = emit $ "  ldr r0, =" <> show n
emitExpr (Not expr) = do
  emitExpr expr
  emit "  cmp r0, #0"
  emit "  moveq r0, #1"
  emit "  movne r0, #0"
emitExpr (Add left right) = do
  emitBinop left right
  emit "  add r0, r1, r0"
emitExpr (Subtract left right) = do
  emitBinop left right
  emit "  sub r0, r1, r0"
emitExpr (Multiply left right) = do
  emitBinop left right
  emit "  mul r0, r1, r0"
emitExpr (Divide left right) = do
  emitBinop left right
  emit "  udiv r0, r1, r0"
emitExpr (Equal left right) = do
  emitBinop left right
  emit "  cmp r0, r1"
  emit "  moveq r0, #1"
  emit "  movne r0, #0"
emitExpr (NotEqual left right) = do
  emitBinop left right
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
  emit "  pop {r0, r1, r2, r3}"
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
  emit "  mov fp, sp"
  emit "  push {r0, r1, r2, r3}"

  -- Body
  withParams params $ emitExpr body

  -- Epilogue
  emit "  mov sp, fp" -- Deallocate stack
  emit "  mov r0, #0"
  emit "  pop {fp, pc}"
emitExpr (Id name) = do
  offset <- getLocal name
  emit $ "  ldr r0, [fp, #" <> show offset <> "]"
emitExpr (Return expr) = do
  emitExpr expr
  emit "  mov sp, fp"
  emit "  pop {fp, pc}"
emitExpr (Var name expr) = do
  emitExpr expr
  emit "  push {r0, ip}"
  setLocal name
emitExpr (Assign name expr) = do
  emitExpr expr
  offset <- getLocal name
  emit $ "  str r0, [fp, #" <> show offset <> "]"

withParams :: [Name] -> CodeGen a -> CodeGen a
withParams params codegen = do
  env <- get
  let locals = Map.fromList (zip params [-16, -12 ..])
  put $ env {locals = locals, nextLocal = -20}
  codegen

emitBinop :: Expr -> Expr -> CodeGen ()
emitBinop left right = do
  emitExpr left
  emit "  push {r0, ip}"
  emitExpr right
  emit "  pop {r1, ip}"

emitProgram :: Program -> CodeGen ()
emitProgram (Program statements) = do
  mapM_ emitExpr statements

initEnv :: Env
initEnv = Env {labelCount = 0, locals = empty, nextLocal = 0}

runEmitter :: CodeGen a -> String
runEmitter cg = execWriter $ runStateT cg initEnv