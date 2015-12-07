{-# LANGUAGE UnicodeSyntax #-}
module LCInterpreter where

data Expression = Variable String
                | Function String Expression
                | Call Expression Expression

instance Show Expression where
    show (Variable n)        = n
    show (Function arg body) = "λ" ++ arg ++ "." ++ show body
    show (Call left right)   = "(" ++ show left ++ " " ++ show right ++ ")"

type Env = [(String, Expression)]

reduce :: Env -> Expression -> Expression
reduce env e@(Variable n)      = maybe e id (lookup n env)
reduce env (Function arg body) = Function arg (reduce env body)
reduce env (Call left right)   = apply env (reduce env left) (reduce env right)

apply :: Env -> Expression -> Expression -> Expression
apply env (Function arg body) e2 = reduce ((arg, e2):env) body
apply env e1 e2 = Call e1 e2

