% Definiujemy moduł zawierający rozwiązanie.
% Należy zmienić nazwę modułu na {imie}_{nazwisko} gdzie za
% {imie} i {nazwisko} należy podstawić odpowiednio swoje imię
% i nazwisko bez znaków diakrytycznych
:- module(marek_kwasny, [solve/2]).

% definiujemy operatory ~/1 oraz v/2
:- op(200, fx, ~).
:- op(500, xfy, v).

% Główny predykat rozwiązujący zadanie.
% UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jego
% definicję.
%solve(Clauses, Solution) :-
%  Clauses  = [p v ~p],
%  Solution = [(p,x)].

solve(Clauses, Solution) :-
	convert(Clauses, BetterClauses),
	variables(BetterClauses, Variables),
	tuple(Variables, TupleList),
	checking(TupleList, BetterClauses),
	Solution = TupleList.


converting(H v T, [H|X]) :-
	!,
	converting(T, X).

converting(H, [H]).


convert([], []).
convert([H|T], [H1|T1]) :-
	converting(H, H1),
	convert(T, T1).



variables(ClauseList, VarList) :-
	flatten(ClauseList, X),
	bettervar(X, WithoutNeg),
	sort(WithoutNeg, VarList).


varex(~X, X).
varex(X, X).


bettervar([], []).

bettervar([H|T], [X|BetterVar]) :-
	varex(H, X), !,
	bettervar(T, BetterVar).
	

values(X, (X, t)).
values(X, (X, f)).


tuple([], []).

tuple([H|T], [H1|T1]) :-
	values(H, H1),
	tuple(T, T1).


checkclauses(_, []) :- fail.

checkclauses((X, t), [H|_]) :-
	X = H.

checkclauses((X, f), [H|_]) :-
	~X = H.

checkclauses((X, L), [_|T]) :-
	checkclauses((X, L), T).



check([], [_]) :- fail.

check([H|_], Clauses) :-
	checkclauses(H, Clauses), !.

check([_|T], Clauses) :-
	check(T, Clauses).


checking(_, []).

checking(TupleList, [H|T]) :-
	check(TupleList, H),
	checking(TupleList, T), !.

checking(_, [_]) :- fail.

	
	