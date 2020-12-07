-- Wymagamy, by moduł zawierał tylko bezpieczne funkcje
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający rozwiązanie.
-- Należy zmienić nazwę modułu na {Imie}{Nazwisko} gdzie za {Imie}
-- i {Nazwisko} należy podstawić odpowiednio swoje imię i nazwisko
-- zaczynające się wielką literą oraz bez znaków diakrytycznych.
module MarekKwasny (typecheck, eval) where

-- Importujemy moduły z definicją języka oraz typami potrzebnymi w zadaniu
import AST
import DataTypes

data CheckType = BoolVal | IntVal deriving (Eq, Show)

data Error p = WrongType p String deriving (Eq, Show)

data EvalType = BoolV Bool | IntV Integer deriving (Eq, Show)

data RuntimeError = RunErr deriving (Eq, Show)

-- Funkcja sprawdzająca typy
-- Dla wywołania typecheck vars e zakładamy, że zmienne występujące
-- w vars są już zdefiniowane i mają typ int, i oczekujemy by wyrażenia e
-- miało typ int
-- UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jej definicję.
typecheck :: [Var] -> Expr p -> TypeCheckResult p
typecheck vars expr = case infer_type (convert vars) expr of Right IntVal -> Ok
                                                             Right BoolVal -> Error (getData expr) "Expected int value as an output"
                                                             Left (WrongType p error) -> Error p error


convert :: [Var] -> [(Var, CheckType)]
convert [] = []
convert [x] = [(x, IntVal)]
convert (x:xs) = convert [x] ++ convert xs


tuplesearch :: Var -> [(Var, CheckType)] -> Bool
tuplesearch var [] = False
tuplesearch var ((fst, _):tail) = if var == fst then True else tuplesearch var tail


tuplefind :: Var -> [(Var, CheckType)] -> CheckType
tuplefind var ((fst, checkType):tail) = if var == fst then checkType else tuplefind var tail


infer_type :: [(Var, CheckType)] -> Expr p -> Either (Error p) CheckType
infer_type env (EVar p x) = if tuplesearch x env then Right (tuplefind x env) else Left (WrongType p "Undefined variable")
infer_type env (ENum _ _) = Right IntVal
infer_type env (EBool _ _) = Right BoolVal
infer_type env (EUnary p UNot exp) = case infer_type env exp of Right BoolVal -> Right BoolVal
                                                                _ -> Left (WrongType p "Expected bool expression")

infer_type env (EUnary p UNeg exp) = case infer_type env exp of Right IntVal -> Right IntVal
                                                                _ -> Left (WrongType p "Expected int expression")

infer_type env (EBinary p BAnd exp1 exp2) = case (infer_type env exp1, infer_type env exp2) of (Right BoolVal, Right BoolVal) -> Right BoolVal
                                                                                               (_, _) -> Left (WrongType p "Operation is exclusive for bool variables only")

infer_type env (EBinary p BOr exp1 exp2) = case (infer_type env exp1, infer_type env exp2) of (Right BoolVal, Right BoolVal) -> Right BoolVal
                                                                                              (_, _) -> Left (WrongType p "Operation is exclusive for bool variables only")

infer_type env (EBinary p BEq exp1 exp2) = case (infer_type env exp1, infer_type env exp2) of (Right IntVal, Right IntVal) -> Right BoolVal
                                                                                              (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (EBinary p BNeq exp1 exp2) = case (infer_type env exp1, infer_type env exp2) of (Right IntVal, Right IntVal) -> Right BoolVal
                                                                                               (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (EBinary p BLt exp1 exp2) = case (infer_type env exp1, infer_type env exp2) of (Right IntVal, Right IntVal) -> Right BoolVal
                                                                                              (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (EBinary p BGt exp1 exp2) = case (infer_type env exp1, infer_type env exp2) of (Right IntVal, Right IntVal) -> Right BoolVal
                                                                                              (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (EBinary p BLe exp1 exp2) = case (infer_type env exp1, infer_type env exp2) of (Right IntVal, Right IntVal) -> Right BoolVal
                                                                                              (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (EBinary p BGe exp1 exp2) = case (infer_type env exp1, infer_type env exp2) of (Right IntVal, Right IntVal) -> Right BoolVal
                                                                                              (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (EBinary p BAdd exp1 exp2) = case (infer_type env exp1, infer_type env exp2) of (Right IntVal, Right IntVal) -> Right IntVal
                                                                                               (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (EBinary p BSub exp1 exp2) = case (infer_type env exp1, infer_type env exp2) of (Right IntVal, Right IntVal) -> Right IntVal
                                                                                               (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (EBinary p BMul exp1 exp2) = case (infer_type env exp1, infer_type env exp2) of (Right IntVal, Right IntVal) -> Right IntVal
                                                                                               (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (EBinary p BDiv exp1 exp2) = case (infer_type env exp1, infer_type env exp2) of (Right IntVal, Right IntVal) -> Right IntVal
                                                                                               (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (EBinary p BMod exp1 exp2) = case (infer_type env exp1, infer_type env exp2) of (Right IntVal, Right IntVal) -> Right IntVal
                                                                                               (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (ELet p var exp1 exp2) = case infer_type env exp1 of Left (WrongType p error) -> Left (WrongType p error)
                                                                    Right IntVal -> infer_type ((var, IntVal):env) exp2
                                                                    Right BoolVal -> infer_type ((var, BoolVal):env) exp2

infer_type env (EIf p exp1 exp2 exp3) = case (infer_type env exp1, infer_type env exp2, infer_type env exp3) of (Right BoolVal, Right BoolVal, Right BoolVal) -> Right BoolVal
                                                                                                                (Right BoolVal, Right IntVal, Right IntVal) -> Right IntVal
                                                                                                                (Right IntVal, _, _) -> Left (WrongType p "Expected bool value in if statement")
                                                                                                                (Right BoolVal, Right IntVal, Right BoolVal) -> Left (WrongType p "Different types of expression")
                                                                                                                (Right BoolVal, Right BoolVal, Right IntVal) -> Left (WrongType p "Different types of expression")
                                                                                                                (_, _, _) -> Left (WrongType p "Undefined error")



-- Funkcja obliczająca wyrażenia
-- Dla wywołania eval input e przyjmujemy, że dla każdej pary (x, v)
-- znajdującej się w input, wartość zmiennej x wynosi v.
-- Możemy założyć, że wyrażenie e jest dobrze typowane, tzn.
-- typecheck (map fst input) e = Ok
-- UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jej definicję.
eval :: [(Var,Integer)] -> Expr p -> EvalResult
eval vars expr = case value (prepare vars) expr of Right (IntV value) -> Value value
                                                   Left RunErr -> RuntimeError

prepare :: [(Var, Integer)] -> [(Var, EvalType)]
prepare [] = []
prepare [(x, y)] = [(x, IntV y)]
prepare (x:xs) = prepare [x] ++ prepare xs


tuplefindVal :: Var -> [(Var, EvalType)] -> EvalType
tuplefindVal var ((fst, evalType):tail) = if var == fst then evalType else tuplefindVal var tail


value :: [(Var, EvalType)] -> Expr p -> Either RuntimeError EvalType
value env (EVar p x) = Right (tuplefindVal x env)
value env (ENum _ x) = Right (IntV x)
value env (EBool _ x) = Right (BoolV x)
value env (EUnary p UNot exp) = case value env exp of Right (BoolV True) -> Right (BoolV False)
                                                      Right (BoolV False) -> Right (BoolV True)
                                                      otherwise -> Left RunErr

value env (EUnary p UNeg exp) = case value env exp of Right (IntV x) -> Right (IntV (-x))
                                                      otherwise -> Left RunErr

value env (EBinary p BAnd exp1 exp2) = case (value env exp1, value env exp2) of (Right (BoolV True), Right (BoolV True)) -> Right (BoolV True)
                                                                                (_, _) -> Right (BoolV False)

value env (EBinary p BOr exp1 exp2) = case (value env exp1, value env exp2) of (Right (BoolV True), Right (BoolV _)) -> Right (BoolV True)
                                                                               (Right (BoolV _), Right (BoolV True)) -> Right (BoolV True)
                                                                               (_, _) -> Right (BoolV False)

value env (EBinary p BEq exp1 exp2) = case (value env exp1, value env exp2) of (Right (IntV x), Right (IntV y)) -> if x == y then Right (BoolV True) else Right (BoolV False)

value env (EBinary p BNeq exp1 exp2) = case (value env exp1, value env exp2) of (Right (IntV x), Right (IntV y)) -> if x == y then Right (BoolV False) else Right (BoolV True)

value env (EBinary p BLt exp1 exp2) = case (value env exp1, value env exp2) of (Right (IntV x), Right (IntV y)) -> if x < y then Right (BoolV True) else Right (BoolV False)

value env (EBinary p BGt exp1 exp2) = case (value env exp1, value env exp2) of (Right (IntV x), Right (IntV y)) -> if x > y then Right (BoolV True) else Right (BoolV False)

value env (EBinary p BLe exp1 exp2) = case (value env exp1, value env exp2) of (Right (IntV x), Right (IntV y)) -> if x <= y then Right (BoolV True) else Right (BoolV False)

value env (EBinary p BGe exp1 exp2) = case (value env exp1, value env exp2) of (Right (IntV x), Right (IntV y)) -> if x >= y then Right (BoolV True) else Right (BoolV False)

value env (EBinary p BAdd exp1 exp2) = case (value env exp1, value env exp2) of (Right (IntV x), Right (IntV y)) -> Right (IntV (x + y))

value env (EBinary p BSub exp1 exp2) = case (value env exp1, value env exp2) of (Right (IntV x), Right (IntV y)) -> Right (IntV (x - y))

value env (EBinary p BMul exp1 exp2) = case (value env exp1, value env exp2) of (Right (IntV x), Right (IntV y)) -> Right (IntV (x * y))

value env (EBinary p BDiv exp1 exp2) = case (value env exp1, value env exp2) of (Right (IntV _), Right (IntV 0)) -> Left RunErr
                                                                                (Right (IntV x), Right (IntV y)) -> Right (IntV (quot x y))

value env (EBinary p BMod exp1 exp2) = case (value env exp1, value env exp2) of (Right (IntV _), Right (IntV 0)) -> Left RunErr
                                                                                (Right (IntV x), Right (IntV y)) -> Right (IntV (mod x y))

value env (ELet p var exp1 exp2) = case value env exp1 of Right (IntV x) -> value ((var, IntV x):env) exp2
                                                          Right (BoolV x) -> value ((var, BoolV x):env) exp2

value env (EIf p exp1 exp2 exp3) = case value env exp1 of Right (BoolV True) -> value env exp2
                                                          Right (BoolV False) -> value env exp3







