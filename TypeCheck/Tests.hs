module TypeCheck.Tests where

import Parse (parseExprString)

import TypeCheck.Types
import TypeCheck.TI
import TypeCheck.Unify
import TypeCheck.Core
import TypeCheck.Infer

test str =
  case parseExprString "test" str of
    Left err -> print err
    Right expr -> 
      case runTI (inferExpr (TypeEnv builtins) expr) of
        (Left err, _) -> putStrLn err
        (Right t, s) -> putStrLn $ str ++ "\n" ++ show (substitute (tiCurrent s) t) ++ "\n"


main = do
  test "fn (a) { return a.b + a.c }"
  test "fn () { var add = fn (a) { var blah = a.b + a.c return a } var obj = {a:1, b:3, c:4} return add(obj) }"
  test "fn () { return console.log(1) }"
  test "fn (a) { var x = fn (b) { var y = fn (c) { return a } return y(b) } return x(x) }"
  test "fn (a) { var x = fn (b) { var y = fn (c) { return a() } return y(b) } return x(x) }"
  test "fn (a) { var x = fn (b) { var y = fn (c) { var z = fn (d) { var h = a() return h() } return z(1) } return y(b) } return x(x) }"
  test "fn (a) { return a(a) }"
  test "fn () { var x = {hello: fn (a) { return a.bye(a) }, bye: fn (a) { return a }} return x }"
  test "fn () { var x = {hello: fn () { return this.bye(this) }, bye: fn (a) { return a }} return x }"
