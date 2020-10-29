module Ch1To5.Expr where

data Expr
  = Const Integer
  | Sum Expr Expr
  | Neg Expr
  | Mult Expr Expr
  | Div Expr Expr

instance Show Expr where
  show (Const i) = show i
  show (Sum e e') =
    concat
      [ "(",
        (show e),
        ") + (",
        (show e'),
        ")"
      ]
  show (Neg e) =
    concat
      [ "-(",
        (show e),
        ")"
      ]
  show (Mult e e') =
    concat
      [ "(",
        (show e),
        ") * (",
        (show e'),
        ")"
      ]
  show (Div e e') =
    concat
      [ "(",
        (show e),
        ") / (",
        (show e'),
        ")"
      ]

eval :: Fractional a => Expr -> a
eval (Const i) = fromIntegral i
eval (Sum e e') = (eval e) + (eval e')
eval (Neg e) = - (eval e)
eval (Mult e e') = (eval e) * (eval e')
eval (Div e e') = (eval e) / (eval e')

-- 5 * (-6 + 1)
testExpr :: Expr
testExpr = Mult (Const 5) (Sum (Neg (Const 6)) (Const 1))
