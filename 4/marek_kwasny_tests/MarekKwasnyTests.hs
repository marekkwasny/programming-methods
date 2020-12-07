-- Wymagamy, by moduł zawierał tylko bezpieczne funkcje
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający testy.
-- Należy zmienić nazwę modułu na {Imie}{Nazwisko}Tests gdzie za {Imie}
-- i {Nazwisko} należy podstawić odpowiednio swoje imię i nazwisko
-- zaczynające się wielką literą oraz bez znaków diakrytycznych.
module MarekKwasnyTests(tests) where

-- Importujemy moduł zawierający typy danych potrzebne w zadaniu
import DataTypes

-- Lista testów do zadania
-- Należy uzupełnić jej definicję swoimi testami
tests :: [Test]
tests =
  [ Test "inc"          (SrcString "input x in x + 1")                                         (Eval [42] (Value 43))
  , Test "undefVar"     (SrcString "x")                                                        TypeError
  , Test "adding"       (SrcString "1 + 3 + 2")                                                (Eval [] (Value 6))
  , Test "ifneg"        (SrcString "input x y in if x > y then 1 else 0")                      (Eval [1, 2] (Value 0))
  , Test "iftrue"       (SrcString "input x y in if x > y then 1 else 0")                      (Eval [2, 1] (Value 1))
  , Test "let"          (SrcString "input y in let x = 10 in if y < x then 1 else 0")          (Eval [5] (Value 1))
  , Test "boolint"      (SrcString "let x = true in if x > 2 then 1 else 0")                   TypeError
  , Test "nonint"       (SrcString "2 + true")                                                 TypeError
  , Test "nonbool"      (SrcString "input x in if 5 then false else x")                        TypeError
  , Test "undef"        (SrcString "input x in x + y")                                         TypeError
  , Test "ifinifelse"   (SrcString "input x y in if x > 2 then if y > 2 then 1 else 0 else 2") (Eval [3, 1] (Value 0))
  , Test "ifinifskip"   (SrcString "input x y in if x > 2 then if y > 2 then 1 else 0 else 2") (Eval [1, 0] (Value 2))
  , Test "ifinifthen"   (SrcString "input x y in if x > 2 then if y > 2 then 1 else 0 else 2") (Eval [3, 3] (Value 1))
  , Test "ifinelseif1"  (SrcString "input x y in if x > 2 then 0 else if y > 2 then 1 else 2") (Eval [3, 5] (Value 0))
  , Test "ifinelseif2"  (SrcString "input x y in if x > 2 then 0 else if y > 2 then 1 else 2") (Eval [1, 5] (Value 1))
  , Test "ifinelseif3"  (SrcString "input x y in if x > 2 then 0 else if y > 2 then 1 else 2") (Eval [1, 1] (Value 2))
  , Test "modinlet"     (SrcString "input y in let x = y mod 2 in if x > 0 then 0 else 1")     (Eval [4] (Value 1))
  ]