% Definiujemy moduł zawierający testy.
% Należy zmienić nazwę modułu na {imie}_{nazwisko}_tests gdzie za
% {imie} i {nazwisko} należy podstawić odpowiednio swoje imię
% i nazwisko bez znaków diakrytycznych
:- module(marek_kwasny_tests, [tests/5]).

% definiujemy operatory ~/1 oraz v/2
:- op(200, fx, ~).
:- op(500, xfy, v).

% Zbiór faktów definiujących testy
% Należy zdefiniować swoje testy
tests(excluded_middle, validity, [p v ~p], 500, solution([(p,t)])).
tests(niezalezna_count, validity, [p v ~p], 500, count(2)).

tests(puste, validity, [], 500, count(0)).

tests(literal, validity, [p], 500, solution([(p,t)])).
tests(literal_count, validity, [p], 500, count(1)).

tests(zanegowany_literal, validity, [~p], 500, solution([(p,f)])).
tests(zanegowany_literal_count, validity, [~p], 500, count(1)).

tests(falsz, validity, [p, ~p], 500, count(0)).

tests(dwa_literaly, validity, [p, q], 500, solution([(p,t), (q,t)])).
tests(zanegowane_dwa_literaly, validity, [~p, ~q], 500, solution([(p,f), (q,f)])).

tests(literaly, validity, [p, q, r], 1000, solution([(p,t), (q,t), (r,t)])).
tests(literaly_count, validity, [p, q, r], 1000, count(1)).

tests(negacje, validity, [~p, ~q, ~r], 1000, solution([(p,f), (q,f), (r,f)])).
tests(negacje_count, validity, [~p, ~q, ~r], 1000, count(1)).

tests(wiele_rozwiazan, validity, [p v q v z v s v t v h], 10000, count(63)). 

tests(niespelnialna1, validity, [~p v q, ~p v ~r v s, ~q v r, p, ~s], 1000, count(0)).
tests(niespelnialna2, validity, [~q v p v ~r, r, p, q, ~q, q v ~p v r], 1000, count(0)).
tests(niespelnialna3, validity, [p v q, ~p v q, p v ~q, ~p v ~q], 1000, count(0)).
tests(niespelnialna4, validity, [q v p, ~p, p], 500, count(0)).
tests(niespelnialna5, validity, [p, ~p v q, ~q], 500, count(0)).
tests(niespelnialna6, validity, [p v p, ~p], 500, count(0)).

tests(spelnialna1, validity, [p v q v r, ~r v ~q v ~p, ~q v r, ~r v p], 1000, solution([(p,t), (q,f), (r,t)])).
tests(spelnialna2, validity, [p v ~p v ~q v q, q v ~q, ~p v p], 1000, solution([(p,f), (q,f)])).
tests(spelnialna3, validity, [p, ~q], 500, solution([(p,t), (q,f)])).
tests(spelnialna4, validity, [q v ~q, p v ~q, ~p], 500, solution([(p,f), (q,f)])).
tests(spelnialna5, validity, [p v q, ~q], 500, solution([(p,t), (q,f)])).

tests(tautologia, validity, [p v q v ~q v ~p], 500, count(4)).

tests(wybor_wartosciowania1, validity, [p v ~p, p], 500, solution([(p,t)])).
tests(wybor_wartosciowania1_count, validity, [p v ~p, p], 500, count(1)).

tests(wybor_wartosciowania2, validity, [p v ~p, ~p], 500, solution([(p,f)])).
tests(wybor_wartosciowania2_count, validity, [p v ~p, ~p], 500, count(1)).

tests(wydajnosciowy1, performance, [p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p, p v ~p], 1000, count(2)).

tests(wydajnosciowy2, performance, [a1 v a2 v a3 v a4 v a5 v a6 v a7 v a8 v a9 v a10], 1000, count(1023)).

