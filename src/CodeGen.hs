module CodeGen (runCodeGen, execCodeGen, emitProgram) where

import Ast
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map

type Asm = String

type Offset = Integer

-- * Code generation context

data Ctx = Ctx
  { labelCount :: Int,
    locals :: Map.Map Name Offset,
    nextLocal :: Offset
  }

initCtx :: Ctx
initCtx = Ctx {labelCount = 0, locals = Map.empty, nextLocal = 0}

-- * Code generation monad

type CodeGen a = StateT Ctx (Writer Asm) a

runCodeGen :: CodeGen a -> (a, Asm)
runCodeGen = runWriter . flip evalStateT initCtx

execCodeGen :: CodeGen a -> Asm
execCodeGen = snd . runCodeGen

emit :: String -> CodeGen ()
emit line = tell (line <> "\n")

label :: CodeGen String
label = do
  ctx <- get
  let count = labelCount ctx
  put ctx {labelCount = count + 1}
  return $ "L" <> show count

setupLocals :: [Name] -> CodeGen ()
setupLocals params =
  modify $ \ctx -> ctx {nextLocal = -4 * fromIntegral (length params)}

getLocal :: Name -> CodeGen Offset
getLocal name = do
  ctx <- get
  case Map.lookup name (locals ctx) of
    Just offset -> return offset
    Nothing -> error $ "Local variable " <> name <> " not found"

setLocal :: Name -> CodeGen ()
setLocal name =
  modify $ \ctx ->
    let offset = nextLocal ctx
     in ctx
          { locals = Map.insert name (offset - 4) (locals ctx),
            nextLocal = offset - 8
          }

-- * Emitting

emitLit :: Literal -> CodeGen ()
emitLit (Number n) = emit $ "  ldr r0, =" <> show n

emitExpr :: Expr -> CodeGen ()
emitExpr (Lit lit) = emitLit lit
emitExpr (UnOp op expr) = emitUnOp op expr
emitExpr (BinOp op left right) = emitBinOp op left right
emitExpr (Id name) = do
  offset <- getLocal name
  emit $ "  ldr r0, [fp, #" <> show offset <> "]"
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

emitUnOp :: UnOp -> Expr -> CodeGen ()
emitUnOp Not expr = do
  emitExpr expr
  emit "  cmp r0, #0"
  emit "  moveq r0, #1"
  emit "  movne r0, #0"

emitBinOp :: BinOp -> Expr -> Expr -> CodeGen ()
emitBinOp op left right = do
  emitExpr left
  emit "  push {r0, ip}"
  emitExpr right
  emit "  pop {r1, ip}"
  case op of
    Add ->
      emit "  add r0, r1, r0"
    Subtract ->
      emit "  sub r0, r1, r0"
    Multiply ->
      emit "  mul r0, r1, r0"
    Divide ->
      emit "  udiv r0, r1, r0"
    Equal -> do
      emit "  cmp r0, r1"
      emit "  moveq r0, #1"
      emit "  movne r0, #0"
    NotEqual -> do
      emit "  cmp r0, r1"
      emit "  moveq r0, #0"
      emit "  movne r0, #1"

emitStmt :: Stmt -> CodeGen ()
emitStmt (Expr expr) = emitExpr expr
emitStmt (Block stmts) = mapM_ emitStmt stmts
emitStmt (If condition consequent alternative) = do
  elseLabel <- label
  endLabel <- label

  emitExpr condition
  emit "  cmp r0, #0"
  emit $ "  beq " <> elseLabel
  emitStmt consequent
  emit $ "  b " <> endLabel
  emit $ elseLabel <> ":"
  emitStmt alternative
  emit $ endLabel <> ":"
emitStmt (While condition body) = do
  startLabel <- label
  endLabel <- label

  emit $ startLabel <> ":"
  emitExpr condition
  emit "  cmp r0, #0"
  emit $ "  beq " <> endLabel
  emitStmt body
  emit $ "  b " <> startLabel
  emit $ endLabel <> ":"
emitStmt (Function name params body) = do
  emit ""
  emit $ ".global " <> name
  emit $ name <> ":"

  -- Prologue
  emit "  push {fp, lr}"
  emit "  mov fp, sp"
  emit "  push {r0, r1, r2, r3}"

  -- Body
  setupLocals params
  emitStmt body

  -- Epilogue
  emit "  mov sp, fp"
  emit "  mov r0, #0"
  emit "  pop {fp, pc}"
emitStmt (Return expr) = do
  emitExpr expr
  emit "  mov sp, fp"
  emit "  pop {fp, pc}"
emitStmt (Var name expr) = do
  emitExpr expr
  emit "  push {r0, ip}"
  setLocal name
emitStmt (Assign name expr) = do
  emitExpr expr
  offset <- getLocal name
  emit $ "  str r0, [fp, #" <> show offset <> "]"

emitProgram :: Program -> CodeGen ()
emitProgram (Program statements) =
  mapM_ emitStmt statements
