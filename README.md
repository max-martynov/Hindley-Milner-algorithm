# Hindley-Milner-algorithm
Implementation of the famous type inference algorithm.
Main concepts and ideas are taken from "Functional programming" college course and [this article](https://en.wikipedia.org/wiki/Hindley–Milner_type_system).

## Usage
```haskell
GHCi> let Right pp = principlePair (Var "x") in pp
(Env [("x",TVar "a")],TVar "a")
GHCi> let Right pp = principlePair (Var "f" :@ Var "x") in pp
(Env [("f",TVar "a" :-> TVar "b"),("x",TVar "a")],TVar "b")
GHCi> let Right pp = principlePair (Lam "x" $ Lam "y" $ Var "y") in pp
(Env [],TVar "a" :-> (TVar "b" :-> TVar "b"))
GHCi> let Left err = principlePair (Var "x" :@ Var "x") in err
"Can't unify (TVar \"a\") with (TVar \"a\" :-> TVar \"b\")!"
```

