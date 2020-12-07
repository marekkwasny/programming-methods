% Definiujemy moduł zawierający testy.
% Należy zmienić nazwę modułu na {imie}_{nazwisko}_tests gdzie za
% {imie} i {nazwisko} należy podstawić odpowiednio swoje imię
% i nazwisko bez wielkich liter oraz znaków diakrytycznych
:- module(marek_kwasny_tests, [tests/3]).

% Zbiór faktów definiujących testy
% Należy zdefiniować swoje testy
tests(empty_program, input(""), program([])).
tests(invalid, input("def main()"), no).
tests(adder, file('adder.hdml'), yes).
tests(srcpos, input("def main(_) = 1"),
  program([def(main, wildcard(file(test, 1, 10, 9, 1)), num(no, 1))])).

tests(empty_from_file, file('empty.hdml'), yes).
tests(bad_function_name, file('bad_function_name.hdml'), no).
tests(hdml_feature, file('hdml_feature.hdml'), no).
tests(bad_if, file('bad_if.hdml'), no).
tests(example, input("def half_adder(A, B) = A & B, A ^ B"), 
	program([def(half_adder, pair(no, var(no, 'A'), var(no, 'B')), pair(no, op(no, '&', var(no, 'A'), var(no, 'B')), op(no, '^', var(no, 'A'), var(no, 'B'))))])).

tests(test1, file('test1.hdml'), yes).
tests(test2, file('test2.hdml'), yes).
tests(test3, file('test3.hdml'), yes).

tests(test4, input("def f(X) = X + 2"),
	program([def(f, var(no, 'X'), op(no, '+', var(no, 'X'), num(no, 2)))])). 

tests(test5, input("def f(_) = []"), 
	program([def(f, wildcard(no), empty(no))])).

tests(test6, input("def f(X, Y) = let _ = f(0, 0) in []"),
	program([def(f, pair(no, var(no, 'X'), var(no, 'Y')), let(no, wildcard(no), call(no, 'f', pair(no, num(no, 0), num(no, 0))), empty(no)))])).

tests(test7, file('test7.hdml'), no).
tests(test8, file('test8.hdml'), yes).
tests(test9, input("def f((((_)))) = []"), yes).
tests(test10, file('test10.hdml'), yes).



