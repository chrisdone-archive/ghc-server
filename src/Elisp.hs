{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Monad.Reader
import           Data.AttoLisp
import           Data.AttoLisp.Easy
import           Data.Attoparsec.Number
import qualified Data.ByteString as L
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           JSBeautify
import           Language.ECMAScript3.Syntax
import           Node

compileAndRun = compile >=> node

compileAndPrint x = do
  L.putStrLn x
  compile x >>= putStr

compile source = beautifyJS $
  case fromLispString ("(progn " <> source <> ")") of
    Left e -> error e
    Right (List (_progn:stmts)) ->
      [ExprStmt ()
                (CallExpr ()
                          (FuncExpr ()
                                    Nothing
                                    [ident "e"]
                                    (concat [[ffi "message" "console.log"]
                                            ,map translateStmt stmts]))
                          [(create [])])]

  where declare name val = VarDecl ()
                                   (ident name)
                                   (return val)

ffi name fname =
  FunctionStmt ()
               (ident name)
               []
               [ReturnStmt ()
                           (return (CallExpr ()
                                             (ref (ident "console.log.apply"))
                                             [ref (ident "console")
                                             ,CallExpr ()
                                                       (ref (ident "Array.prototype.slice.call"))
                                                       [ref (ident "arguments")
                                                       ,IntLit () 1
                                                       ]]))]

translateStmt (List (Symbol "progn":xs)) =
  BlockStmt () (map translateStmt xs)
translateStmt (List (Symbol "defun":name:params:body)) =
  let (FuncExpr () _ ps b) =
        runReader (translate (List (concat [[Symbol "lambda",params]
                                            ,body])))
                  []
  in FunctionStmt () (intern (sym name)) ps b
translateStmt call@(List (op:args)) =
  ExprStmt () (translate call [])

translate (List (Symbol "progn":xs)) =
  fmap (ListExpr ()) (mapM translate xs)
translate (List (Symbol "lambda":List (map sym -> bindings):body)) =
  local (const bindings) $ do
    args <- mapM (fmap (ExprStmt ()) . translate)
                 (take (length body - 1) body)
    retstmt <- fmap (ReturnStmt () . listToMaybe)
                    (mapM translate (take 1 (reverse body)))
    return (FuncExpr ()
                     Nothing
                     (env : map intern bindings)
                     (concat [if null bindings then [] else [localb bindings]
                             ,args
                             ,[retstmt]]))
translate (List (Symbol "let":List (map pair -> args):body)) = do
  binds <- translate (List (concat [[Symbol "lambda"
                                    ,List (map fst args)]
                                   ,body]))
  bodystmts <- mapM (translate . snd) args
  return (CallExpr () binds (ref env : bodystmts))
translate (List ((operator -> Just (op,zero)):args)) =
  case args of
    [_] -> return (IntLit () zero)
    _ -> fmap (foldr1 (InfixExpr () op))
              (mapM translate args)
translate (List (op:args)) = do
  ope <- case op of
    Symbol o -> return (ref (intern o))
    _ -> translate op
  argexprs <- mapM translate args
  return (CallExpr ()
                   ope
                   (ref env : argexprs))
translate (Symbol x) = do
  env <- ask
  if elem x env
     then return (ref (intern x))
     else return (envRef x)
translate (Number (I i)) =
  return (IntLit () (fromIntegral i))
translate (Number (D d)) =
  return (NumLit () d)
translate (String t) =
  return (StringLit () (T.unpack t))

localb bindings =
  ExprStmt ()
           (ListExpr ()
                     (AssignExpr ()
                                 OpAssign
                                 (LVar () envname)
                                 (create [envref])
                     : assigns))

  where assigns = map (\sym ->
                        AssignExpr ()
                                   OpAssign
                                   (LDot ()
                                         (ref env)
                                         (T.unpack sym))
                                   (ref (intern sym)))
                      bindings

create x = CallExpr ()
                    (DotRef () (ref (ident "Object")) (ident "create"))
                    (if null x then [NullLit ()] else x)

envRef x =
  DotRef ()
         envref
         (intern x)

envref = VarRef () env
envname = "e"
env = intern (T.pack envname)

intern = Id () . T.unpack
ident = Id ()
ref = VarRef ()

sym (Symbol x) = x
sym e = error (show e)

pair (List [x,y]) = (x,y)

operator s@(Symbol{}) =
  case sym s of
    "+" -> return (OpAdd,0)
    "-" -> return (OpSub,0)
    "*" -> return (OpMul,1)
    _   -> Nothing
operator _ = Nothing
