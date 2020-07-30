module Ch11.Hutton where

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add x1 x2) = (eval x1) + (eval x2)

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add expr1 expr2) = printExpr expr1 ++ " + " ++ printExpr expr2
