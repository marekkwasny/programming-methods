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
import Data.Map (Map)
import qualified Data.Map as Map

-- data Type = TBool | TInt | TUnit | TPair Type Type | TList Type deriving (Eq, Show)

data Error p = WrongType p String deriving (Eq, Show)

data EvalType
    = BoolV Bool 
    | IntV Integer 
    | PairV (EvalType, EvalType) 
    | ListV [EvalType]
    | EmptyV
    | UnitV 
    deriving (Eq, Show)

data RuntimeError = RunErr deriving (Eq, Show)

-- Funkcja sprawdzająca typy
-- Dla wywołania typecheck vars e zakładamy, że zmienne występujące
-- w vars są już zdefiniowane i mają typ int, i oczekujemy by wyrażenia e
-- miało typ int
-- UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jej definicję.
typecheck :: [FunctionDef p] -> [Var] -> Expr p -> TypeCheckResult p
typecheck defs vars expr = 
    case (infer_def defs (prep defs), infer_type (prep defs) (convert vars) expr) of
        (False, _) -> Error (getData expr) "Wrong function definition"
        (True, Right TInt) -> Ok
        (True, Right TBool) -> Error (getData expr) "Expected int value as an output"
        (True, Left (WrongType p error)) -> Error p error


convert :: [Var] -> [(Var, Type)]
convert [] = []
convert [x] = [(x, TInt)]
convert (x:xs) = convert [x] ++ convert xs


tuplesearch :: Var -> [(Var, Type)] -> Bool
tuplesearch var [] = False
tuplesearch var ((fst, _):tail) = if var == fst then True else tuplesearch var tail


tuplefind :: Var -> [(Var, Type)] -> Type
tuplefind var ((fst, x):tail) = if var == fst then x else tuplefind var tail


prep :: [FunctionDef p] -> Map FSym (FunctionDef p)
prep defs = Map.fromList (zip (map funcName (defs)) defs)


infer_def :: [FunctionDef p] -> Map FSym (FunctionDef p) -> Bool
infer_def [] map = True
infer_def (x:xs) map =
    case infer_type map [(funcArg x, funcArgType x)] (funcBody x) of
        Left _ -> False
        Right y -> if y == funcResType x then infer_def xs map else False


infer_type :: Map FSym (FunctionDef p) -> [(Var, Type)] -> Expr p -> Either (Error p) Type
infer_type defs env (EVar p x) =
    if tuplesearch x env then Right (tuplefind x env)
    else Left (WrongType p "Undefined variable")

infer_type defs env (ENum _ _) = Right TInt
infer_type defs env (EBool _ _) = Right TBool
infer_type defs env (EUnit p) = Right TUnit
infer_type defs env (ENil p x) = Right x

infer_type defs env (EPair p exp1 exp2) = 
    case (infer_type defs env exp1, infer_type defs env exp2) of
        (Right x, Right y) -> Right (TPair x y)
        (_, _) -> Left (WrongType p "Something is wrong in the pair")

infer_type defs env (EFst p exp) = 
    case infer_type defs env exp of
        Right (TPair x y) -> Right x
        _ -> Left (WrongType p "Expected pair") 

infer_type defs env (ESnd p exp) = 
    case infer_type defs env exp of
        Right (TPair x y) -> Right y
        _ -> Left (WrongType p "Expected pair")

infer_type defs env (EUnary p UNot exp) = 
    case infer_type defs env exp of
        Right TBool -> Right TBool
        _ -> Left (WrongType p "Expected bool expression")

infer_type defs env (EUnary p UNeg exp) = 
    case infer_type defs env exp of
        Right TInt -> Right TInt
        _ -> Left (WrongType p "Expected int expression")

infer_type defs env (EBinary p BAnd exp1 exp2) = 
    case (infer_type defs env exp1, infer_type defs env exp2) of
        (Right TBool, Right TBool) -> Right TBool
        (_, _) -> Left (WrongType p "Operation is exclusive for bool variables only")

infer_type defs env (EBinary p BOr exp1 exp2) = 
    case (infer_type defs env exp1, infer_type defs env exp2) of
        (Right TBool, Right TBool) -> Right TBool
        (_, _) -> Left (WrongType p "Operation is exclusive for bool variables only")

infer_type defs env (EBinary p BEq exp1 exp2) = 
    case (infer_type defs env exp1, infer_type defs env exp2) of
        (Right TInt, Right TInt) -> Right TBool
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type defs env (EBinary p BNeq exp1 exp2) = 
    case (infer_type defs env exp1, infer_type defs env exp2) of
        (Right TInt, Right TInt) -> Right TBool
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type defs env (EBinary p BLt exp1 exp2) = 
    case (infer_type defs env exp1, infer_type defs env exp2) of
        (Right TInt, Right TInt) -> Right TBool
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type defs env (EBinary p BGt exp1 exp2) = 
    case (infer_type defs env exp1, infer_type defs env exp2) of
        (Right TInt, Right TInt) -> Right TBool
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type defs env (EBinary p BLe exp1 exp2) = 
    case (infer_type defs env exp1, infer_type defs env exp2) of
        (Right TInt, Right TInt) -> Right TBool
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type defs env (EBinary p BGe exp1 exp2) = 
    case (infer_type defs env exp1, infer_type defs env exp2) of 
        (Right TInt, Right TInt) -> Right TBool
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type defs env (EBinary p BAdd exp1 exp2) = 
    case (infer_type defs env exp1, infer_type defs env exp2) of 
        (Right TInt, Right TInt) -> Right TInt
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type defs env (EBinary p BSub exp1 exp2) = 
    case (infer_type defs env exp1, infer_type defs env exp2) of 
        (Right TInt, Right TInt) -> Right TInt
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type defs env (EBinary p BMul exp1 exp2) = 
    case (infer_type defs env exp1, infer_type defs env exp2) of 
        (Right TInt, Right TInt) -> Right TInt
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type defs env (EBinary p BDiv exp1 exp2) = 
    case (infer_type defs env exp1, infer_type defs env exp2) of 
        (Right TInt, Right TInt) -> Right TInt
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type defs env (EBinary p BMod exp1 exp2) = 
    case (infer_type defs env exp1, infer_type defs env exp2) of 
        (Right TInt, Right TInt) -> Right TInt
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type defs env (ELet p var exp1 exp2) = 
    case infer_type defs env exp1 of 
        Left (WrongType p error) -> Left (WrongType p error)
        Right TInt -> infer_type defs ((var, TInt):env) exp2
        Right TBool -> infer_type defs ((var, TBool):env) exp2
        Right TUnit -> infer_type defs ((var, TUnit):env) exp2
        Right (TList x) -> infer_type defs ((var, TList x):env) exp2
        Right (TPair x y) -> infer_type defs ((var, TPair x y):env) exp2

infer_type defs env (EIf p exp1 exp2 exp3) = 
    case (infer_type defs env exp1, infer_type defs env exp2, infer_type defs env exp3) of
        (Right TBool, Right x, Right y) -> if x == y then Right x
            else Left (WrongType p "Different types of expression")
        (Right TInt, _, _) -> Left (WrongType p "Expected bool value in if statement")
        (_, _, _) -> Left (WrongType p "Wrong types in if statement")

infer_type defs env (EApp p name exp) = 
    case Map.lookup name defs of
        Nothing -> Left (WrongType p "Called undefined function")
        Just x -> Right (funcResType x)

infer_type defs env (ECons p exp1 exp2) = 
    case (infer_type defs env exp1, infer_type defs env exp2) of
        (Right x, Right (TList y)) -> if x == y then Right (TList x)
                                      else Left (WrongType p "Wrong list type")
        (Right x, Right y) -> Left (WrongType p "Wrong list type")
        (Left _, _) -> Left (WrongType p "Wrong list type")
        (_, Left _) -> Left (WrongType p "Wrong list type")

infer_type defs env (EMatchL p exp1 exp2 (var1, var2, exp3)) =
    case (infer_type defs env exp1) of
        Left _ -> Left (WrongType p "No list to match")
        Right (TList x) -> case (infer_type defs env exp2, infer_type defs ((var1, x):(var2, TList x):env) exp3) of
            (Right z, Right y) -> if z == y then Right z else Left (WrongType p "Wrong types to match")
            (_, _) -> Left (WrongType p "Wrong types to match")
        Right x -> Left (WrongType p "No list to match")

-- Funkcja obliczająca wyrażenia
-- Dla wywołania eval input e przyjmujemy, że dla każdej pary (x, v)
-- znajdującej się w input, wartość zmiennej x wynosi v.
-- Możemy założyć, że wyrażenie e jest dobrze typowane, tzn.
-- typecheck (map fst input) e = Ok
-- UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jej definicję.
eval :: [FunctionDef p] -> [(Var,Integer)] -> Expr p -> EvalResult
eval defs vars expr = case value (prep defs) (prepare vars) expr of Right (IntV value) -> Value value
                                                                    Left RunErr -> RuntimeError

prepare :: [(Var, Integer)] -> [(Var, EvalType)]
prepare [] = []
prepare [(x, y)] = [(x, IntV y)]
prepare (x:xs) = prepare [x] ++ prepare xs


tuplefindVal :: Var -> [(Var, EvalType)] -> EvalType
tuplefindVal var ((fst, evalType):tail) = if var == fst then evalType else tuplefindVal var tail


value :: Map FSym (FunctionDef p) -> [(Var, EvalType)] -> Expr p -> Either RuntimeError EvalType
value map env (EVar p x) = Right (tuplefindVal x env)
value map env (ENum _ x) = Right (IntV x)
value map env (EBool _ x) = Right (BoolV x)
value map env (ENil _ _) = Right EmptyV
value map env (EUnit _) = Right UnitV

value map env (EUnary p UNot exp) =
    case value map env exp of 
        Right (BoolV True) -> Right (BoolV False)
        Right (BoolV False) -> Right (BoolV True)
        otherwise -> Left RunErr

value map env (EUnary p UNeg exp) = 
    case value map env exp of 
        Right (IntV x) -> Right (IntV (-x))
        otherwise -> Left RunErr

value map env (EBinary p BAnd exp1 exp2) = 
    case (value map env exp1, value map env exp2) of 
        (Right (BoolV True), Right (BoolV True)) -> Right (BoolV True)
        (_, _) -> Right (BoolV False)

value map env (EBinary p BOr exp1 exp2) =
    case (value map env exp1, value map env exp2) of 
        (Right (BoolV True), Right (BoolV _)) -> Right (BoolV True)
        (Right (BoolV _), Right (BoolV True)) -> Right (BoolV True)
        (_, _) -> Right (BoolV False)

value map env (EBinary p BEq exp1 exp2) = 
    case (value map env exp1, value map env exp2) of 
        (Right (IntV x), Right (IntV y)) -> if x == y then Right (BoolV True) 
            else Right (BoolV False)

value map env (EBinary p BNeq exp1 exp2) =
    case (value map env exp1, value map env exp2) of
        (Right (IntV x), Right (IntV y)) -> if x == y then Right (BoolV False)
            else Right (BoolV True)

value map env (EBinary p BLt exp1 exp2) =
    case (value map env exp1, value map env exp2) of 
        (Right (IntV x), Right (IntV y)) -> if x < y then Right (BoolV True) 
            else Right (BoolV False)

value map env (EBinary p BGt exp1 exp2) = 
    case (value map env exp1, value map env exp2) of 
        (Right (IntV x), Right (IntV y)) -> if x > y then Right (BoolV True) 
            else Right (BoolV False)

value map env (EBinary p BLe exp1 exp2) = 
    case (value map env exp1, value map env exp2) of 
        (Right (IntV x), Right (IntV y)) -> if x <= y then Right (BoolV True) 
            else Right (BoolV False)

value map env (EBinary p BGe exp1 exp2) = 
    case (value map env exp1, value map env exp2) of 
        (Right (IntV x), Right (IntV y)) -> if x >= y then Right (BoolV True) 
            else Right (BoolV False)

value map env (EBinary p BAdd exp1 exp2) = 
    case (value map env exp1, value map env exp2) of 
        (Right (IntV x), Right (IntV y)) -> Right (IntV (x + y))

value map env (EBinary p BSub exp1 exp2) = 
    case (value map env exp1, value map env exp2) of 
        (Right (IntV x), Right (IntV y)) -> Right (IntV (x - y))

value map env (EBinary p BMul exp1 exp2) = 
    case (value map env exp1, value map env exp2) of 
        (Right (IntV x), Right (IntV y)) -> Right (IntV (x * y))

value map env (EBinary p BDiv exp1 exp2) = 
    case (value map env exp1, value map env exp2) of 
        (Right (IntV _), Right (IntV 0)) -> Left RunErr
        (Right (IntV x), Right (IntV y)) -> Right (IntV (quot x y))

value map env (EBinary p BMod exp1 exp2) = 
    case (value map env exp1, value map env exp2) of 
        (Right (IntV _), Right (IntV 0)) -> Left RunErr
        (Right (IntV x), Right (IntV y)) -> Right (IntV (mod x y))

value map env (ELet p var exp1 exp2) = 
    case value map env exp1 of 
        Right x -> value map ((var, x):env) exp2
        Left RunErr -> Left RunErr

value map env (EIf p exp1 exp2 exp3) = 
    case value map env exp1 of 
        Right (BoolV True) -> value map env exp2
        Right (BoolV False) -> value map env exp3
        Left RunErr -> Left RunErr

value map env (EApp p name exp) =
    case (Map.lookup name map, value map env exp) of
        (Just x, Left y) -> Left RunErr
        (Just x, Right y) -> value map ((funcArg x, y):env) (funcBody x)
        (Nothing, _) -> Left RunErr

value map env (EPair p exp1 exp2) =
    case (value map env exp1, value map env exp2) of
        (Right x, Right y) -> Right (PairV (x, y))
        otherwise -> Left RunErr

value map env (EFst p exp) =
    case value map env exp of
        Right (PairV (x, y)) -> Right x
        Left RunErr -> Left RunErr

value map env (ESnd p exp) =
    case value map env exp of
        Right (PairV (x, y)) -> Right y
        Left RunErr -> Left RunErr
    
value map env (ECons p exp1 exp2) =
    case (value map env exp1, value map env exp2) of
        (Right x, Right y) -> Right (ListV [x, y])
        (Left RunErr, _) -> Left RunErr
        (_, Left RunErr) -> Left RunErr

value map env (EMatchL p exp1 exp2 (var1, var2, exp3)) =
    case (value map env exp1) of
        Right EmptyV -> value map env exp2
        Right (ListV (x:y:[])) -> value map ((var1, x):(var2, y):env) exp3
        Left RunErr -> Left RunErr

