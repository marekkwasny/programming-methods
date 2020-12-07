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

-- data Type = TBool | TInt | TUnit | TPair Type Type | TList Type deriving (Eq, Show)

data Error p = WrongType p String deriving (Eq, Show)

data EvalType p
    = BoolV Bool 
    | IntV Integer 
    | PairV (EvalType p, EvalType p) 
    | ListV [EvalType p]
    | EmptyV
    | UnitV
    | ArrowV Var (Expr p)
    deriving (Eq, Show)

data RuntimeError = RunErr deriving (Eq, Show)

-- Funkcja sprawdzająca typy
-- Dla wywołania typecheck vars e zakładamy, że zmienne występujące
-- w vars są już zdefiniowane i mają typ int, i oczekujemy by wyrażenia e
-- miało typ int
-- UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jej definicję.
typecheck :: [FunctionDef p] -> [Var] -> Expr p -> TypeCheckResult p
typecheck defs vars expr = 
    case (infer_def (funconv defs) defs, infer_type ((funconv defs)++(convert vars)) expr) of
        (True, Right TInt) -> Ok
        (True, Right _) -> Error (getData expr) "Expected int value as an output"
        (True, Left (WrongType p error)) -> Error p error
        (False, _) -> Error (getData expr) "Wrong function definition"


convert :: [Var] -> [(Var, Type)]
convert [] = []
convert [x] = [(x, TInt)]
convert (x:xs) = convert [x] ++ convert xs


tuplesearch :: Var -> [(Var, Type)] -> Bool
tuplesearch var [] = False
tuplesearch var ((fst, _):tail) = if var == fst then True else tuplesearch var tail


tuplefind :: Var -> [(Var, Type)] -> Type
tuplefind var ((fst, x):tail) = if var == fst then x else tuplefind var tail


funconv :: [FunctionDef p] -> [(Var, Type)]
funconv [] = []
funconv (x:xs) = [(funcName x, TArrow (funcArgType x) (funcResType x))] ++ funconv xs


infer_def :: [(Var, Type)] -> [FunctionDef p] -> Bool
infer_def _ [] = True
infer_def fn (x:xs) =
    case infer_type ([(funcArg x, funcArgType x)]++fn) (funcBody x) of
        Left _ -> False
        Right y -> if y == funcResType x then infer_def fn xs else False


infer_type :: [(Var, Type)] -> Expr p -> Either (Error p) Type
infer_type env (EVar p x) =
    if tuplesearch x env then Right (tuplefind x env)
    else Left (WrongType p "Undefined variable")

infer_type env (ENum _ _) = Right TInt
infer_type env (EBool _ _) = Right TBool
infer_type env (EUnit p) = Right TUnit
infer_type env (ENil p x) = Right x

infer_type env (EPair p exp1 exp2) = 
    case (infer_type env exp1, infer_type env exp2) of
        (Right x, Right y) -> Right (TPair x y)
        (_, _) -> Left (WrongType p "Something is wrong in the pair")

infer_type env (EFst p exp) = 
    case infer_type env exp of
        Right (TPair x y) -> Right x
        _ -> Left (WrongType p "Expected pair") 

infer_type env (ESnd p exp) = 
    case infer_type env exp of
        Right (TPair x y) -> Right y
        _ -> Left (WrongType p "Expected pair")

infer_type env (EUnary p UNot exp) = 
    case infer_type env exp of
        Right TBool -> Right TBool
        _ -> Left (WrongType p "Expected bool expression")

infer_type env (EUnary p UNeg exp) = 
    case infer_type env exp of
        Right TInt -> Right TInt
        _ -> Left (WrongType p "Expected int expression")

infer_type env (EBinary p BAnd exp1 exp2) = 
    case (infer_type env exp1, infer_type env exp2) of
        (Right TBool, Right TBool) -> Right TBool
        (_, _) -> Left (WrongType p "Operation is exclusive for bool variables only")

infer_type env (EBinary p BOr exp1 exp2) = 
    case (infer_type env exp1, infer_type env exp2) of
        (Right TBool, Right TBool) -> Right TBool
        (_, _) -> Left (WrongType p "Operation is exclusive for bool variables only")

infer_type env (EBinary p BEq exp1 exp2) = 
    case (infer_type env exp1, infer_type env exp2) of
        (Right TInt, Right TInt) -> Right TBool
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (EBinary p BNeq exp1 exp2) = 
    case (infer_type env exp1, infer_type env exp2) of
        (Right TInt, Right TInt) -> Right TBool
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (EBinary p BLt exp1 exp2) = 
    case (infer_type env exp1, infer_type env exp2) of
        (Right TInt, Right TInt) -> Right TBool
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (EBinary p BGt exp1 exp2) = 
    case (infer_type env exp1, infer_type env exp2) of
        (Right TInt, Right TInt) -> Right TBool
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (EBinary p BLe exp1 exp2) = 
    case (infer_type env exp1, infer_type env exp2) of
        (Right TInt, Right TInt) -> Right TBool
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (EBinary p BGe exp1 exp2) = 
    case (infer_type env exp1, infer_type env exp2) of 
        (Right TInt, Right TInt) -> Right TBool
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (EBinary p BAdd exp1 exp2) = 
    case (infer_type env exp1, infer_type env exp2) of 
        (Right TInt, Right TInt) -> Right TInt
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (EBinary p BSub exp1 exp2) = 
    case (infer_type env exp1, infer_type env exp2) of 
        (Right TInt, Right TInt) -> Right TInt
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (EBinary p BMul exp1 exp2) = 
    case (infer_type env exp1, infer_type env exp2) of 
        (Right TInt, Right TInt) -> Right TInt
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (EBinary p BDiv exp1 exp2) = 
    case (infer_type env exp1, infer_type env exp2) of 
        (Right TInt, Right TInt) -> Right TInt
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (EBinary p BMod exp1 exp2) = 
    case (infer_type env exp1, infer_type env exp2) of 
        (Right TInt, Right TInt) -> Right TInt
        (_, _) -> Left (WrongType p "Operation is exclusive for int variables only")

infer_type env (ELet p var exp1 exp2) = 
    case infer_type env exp1 of 
        Left (WrongType p error) -> Left (WrongType p error)
        Right TInt -> infer_type ((var, TInt):env) exp2
        Right TBool -> infer_type ((var, TBool):env) exp2
        Right TUnit -> infer_type ((var, TUnit):env) exp2
        Right (TList x) -> infer_type ((var, TList x):env) exp2
        Right (TPair x y) -> infer_type ((var, TPair x y):env) exp2
        Right x -> infer_type ((var, x):env) exp2

infer_type env (EIf p exp1 exp2 exp3) = 
    case (infer_type env exp1, infer_type env exp2, infer_type env exp3) of
        (Right TBool, Right x, Right y) -> if x == y then Right x
            else Left (WrongType p "Different types of expressions")
        (Right TInt, _, _) -> Left (WrongType p "Expected bool value in if statement")
        (_, _, _) -> Left (WrongType p "Wrong types in if statement")

infer_type env (EApp p exp1 exp2) =
    case infer_type env exp1 of
        Right (TArrow t1 t2) -> case infer_type env exp2 of
            Right t3 -> if t1 == t3 then Right t2
                        else Left (WrongType p "Wrong types in application")
            Left _ -> Left (WrongType p "Error during function application")
        Right (_) -> Left (WrongType p "Called undefined function")
        Left (_) -> Left (WrongType p "Error during function application")

infer_type env (EFn p var vtype exp) =
    case (vtype, infer_type ((var, vtype) : env) exp) of
        (x, Right y) -> Right (TArrow x y)
        (_, Left x) -> Left (WrongType p "Error in lambda function")

infer_type env (ECons p exp1 exp2) = 
    case (infer_type env exp1, infer_type env exp2) of
        (Right x, Right (TList y)) -> if x == y then Right (TList x)
                                      else Left (WrongType p "Wrong list type")
        (Right x, Right y) -> Left (WrongType p "Wrong list type")
        (Left _, _) -> Left (WrongType p "Wrong list type")
        (_, Left _) -> Left (WrongType p "Wrong list type")

infer_type env (EMatchL p exp1 exp2 (var1, var2, exp3)) =
    case (infer_type env exp1) of
        Left _ -> Left (WrongType p "No list to match")
        Right (TList x) -> case (infer_type env exp2, infer_type ((var1, x):(var2, TList x):env) exp3) of
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
eval defs vars expr = case value ((prepare vars)++(prep defs)) expr of Right (IntV value) -> Value value
                                                                       Left RunErr -> RuntimeError

prepare :: [(Var, Integer)] -> [(Var, EvalType p)]
prepare [] = []
prepare [(x, y)] = [(x, IntV y)]
prepare (x:xs) = prepare [x] ++ prepare xs


prep :: [FunctionDef p] -> [(Var, EvalType p)]
prep [] = []
prep (x:xs) = [(funcName x, ArrowV (funcArg x) (funcBody x))] ++ prep xs


tuplefindVal :: Var -> [(Var, EvalType p)] -> EvalType p
tuplefindVal var ((fst, evalType):tail) = if var == fst then evalType
                                          else tuplefindVal var tail


value :: [(Var, EvalType p)] -> Expr p -> Either RuntimeError (EvalType p)
value env (EVar p x) = Right (tuplefindVal x env)
value env (ENum _ x) = Right (IntV x)
value env (EBool _ x) = Right (BoolV x)
value env (ENil _ _) = Right EmptyV
value env (EUnit _) = Right UnitV

value env (EUnary p UNot exp) =
    case value env exp of 
        Right (BoolV True) -> Right (BoolV False)
        Right (BoolV False) -> Right (BoolV True)
        otherwise -> Left RunErr

value env (EUnary p UNeg exp) = 
    case value env exp of 
        Right (IntV x) -> Right (IntV (-x))
        otherwise -> Left RunErr

value env (EBinary p BAnd exp1 exp2) = 
    case (value env exp1, value env exp2) of 
        (Right (BoolV True), Right (BoolV True)) -> Right (BoolV True)
        (_, _) -> Right (BoolV False)

value env (EBinary p BOr exp1 exp2) =
    case (value env exp1, value env exp2) of 
        (Right (BoolV True), Right (BoolV _)) -> Right (BoolV True)
        (Right (BoolV _), Right (BoolV True)) -> Right (BoolV True)
        (_, _) -> Right (BoolV False)

value env (EBinary p BEq exp1 exp2) = 
    case (value env exp1, value env exp2) of 
        (Right (IntV x), Right (IntV y)) -> if x == y then Right (BoolV True) 
            else Right (BoolV False)
        otherwise -> Left RunErr

value env (EBinary p BNeq exp1 exp2) =
    case (value env exp1, value env exp2) of
        (Right (IntV x), Right (IntV y)) -> if x == y then Right (BoolV False)
            else Right (BoolV True)
        otherwise -> Left RunErr

value env (EBinary p BLt exp1 exp2) =
    case (value env exp1, value env exp2) of 
        (Right (IntV x), Right (IntV y)) -> if x < y then Right (BoolV True) 
            else Right (BoolV False)
        otherwise -> Left RunErr

value env (EBinary p BGt exp1 exp2) = 
    case (value env exp1, value env exp2) of 
        (Right (IntV x), Right (IntV y)) -> if x > y then Right (BoolV True) 
            else Right (BoolV False)
        otherwise -> Left RunErr

value env (EBinary p BLe exp1 exp2) = 
    case (value env exp1, value env exp2) of 
        (Right (IntV x), Right (IntV y)) -> if x <= y then Right (BoolV True) 
            else Right (BoolV False)
        otherwise -> Left RunErr

value env (EBinary p BGe exp1 exp2) = 
    case (value env exp1, value env exp2) of 
        (Right (IntV x), Right (IntV y)) -> if x >= y then Right (BoolV True) 
            else Right (BoolV False)
        otherwise -> Left RunErr

value env (EBinary p BAdd exp1 exp2) = 
    case (value env exp1, value env exp2) of 
        (Right (IntV x), Right (IntV y)) -> Right (IntV (x + y))
        otherwise -> Left RunErr

value env (EBinary p BSub exp1 exp2) = 
    case (value env exp1, value env exp2) of 
        (Right (IntV x), Right (IntV y)) -> Right (IntV (x - y))
        otherwise -> Left RunErr

value env (EBinary p BMul exp1 exp2) = 
    case (value env exp1, value env exp2) of 
        (Right (IntV x), Right (IntV y)) -> Right (IntV (x * y))
        otherwise -> Left RunErr

value env (EBinary p BDiv exp1 exp2) = 
    case (value env exp1, value env exp2) of 
        (Right (IntV _), Right (IntV 0)) -> Left RunErr
        (Right (IntV x), Right (IntV y)) -> Right (IntV (quot x y))
        otherwise -> Left RunErr

value env (EBinary p BMod exp1 exp2) = 
    case (value env exp1, value env exp2) of 
        (Right (IntV _), Right (IntV 0)) -> Left RunErr
        (Right (IntV x), Right (IntV y)) -> Right (IntV (mod x y))
        otherwise -> Left RunErr

value env (ELet p var exp1 exp2) = 
    case value env exp1 of 
        Right x -> value ((var, x):env) exp2
        Left RunErr -> Left RunErr
        
value env (EIf p exp1 exp2 exp3) = 
    case value env exp1 of 
        Right (BoolV True) -> value env exp2
        Right (BoolV False) -> value env exp3
        Left RunErr -> Left RunErr


value env (EApp p exp1 exp2) =
    case (value env exp1, value env exp2) of
        (Right (ArrowV var exp), Right x) -> value ((var, x):env) exp
        otherwise -> Left RunErr


value env (EFn p var vtype exp) =
    Right (ArrowV var exp)


value env (EPair p exp1 exp2) =
    case (value env exp1, value env exp2) of
        (Right x, Right y) -> Right (PairV (x, y))
        otherwise -> Left RunErr

value env (EFst p exp) =
    case value env exp of
        Right (PairV (x, y)) -> Right x
        Left RunErr -> Left RunErr

value env (ESnd p exp) =
    case value env exp of
        Right (PairV (x, y)) -> Right y
        Left RunErr -> Left RunErr
    
value env (ECons p exp1 exp2) =
    case (value env exp1, value env exp2) of
        (Right x, Right y) -> Right (ListV [x, y])
        (Left RunErr, _) -> Left RunErr
        (_, Left RunErr) -> Left RunErr

value env (EMatchL p exp1 exp2 (var1, var2, exp3)) =
    case (value env exp1) of
        Right EmptyV -> value env exp2
        Right (ListV (x:y:[])) -> value ((var1, x):(var2, y):env) exp3
        Left RunErr -> Left RunErr
