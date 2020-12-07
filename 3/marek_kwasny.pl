% Definiujemy moduł zawierający rozwiązanie.
% Należy zmienić nazwę modułu na {imie}_{nazwisko} gdzie za
% {imie} i {nazwisko} należy podstawić odpowiednio swoje imię
% i nazwisko bez wielkich liter oraz znaków diakrytycznych
:- module(marek_kwasny, [parse/3]).

% Główny predykat rozwiązujący zadanie.
% UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jego
% definicję.

% [ ] .. , ^ | & / % @ # ~

lexer(Tokens) -->
   white_space,
   (  (  ":=",      !, { Token = tokAssgn }
      ;  ";",       !, { Token = tokSColon }
      ;  "(",       !, { Token = tokLParen }
      ;  ")",       !, { Token = tokRParen }
      ;  "+",       !, { Token = tokPlus }
      ;  "-",       !, { Token = tokMinus }
      ;  "*",       !, { Token = tokTimes }
      ;  "=",       !, { Token = tokEq }
      ;  "<>",      !, { Token = tokNeq }
      ;  "<=",      !, { Token = tokLeq }
      ;  "<",       !, { Token = tokLt }
      ;  ">=",      !, { Token = tokGeq }
      ;  ">",       !, { Token = tokGt }
      ;  "[",	    !, { Token = tokLSq }
      ;  "]",       !, { Token = tokRSq }
      ;  "..", 	    !, { Token = tokDots }
      ;  ",",	    !, { Token = tokComma }
      ;  "^", 	    !, { Token = tokPower }
      ;  "|", 	    !, { Token = tokOr }
      ;  "&", 	    !, { Token = tokAmp }
      ;  "/",  	    !, { Token = tokDiv }
      ;  "%", 	    !, { Token = tokMod }
      ;  "@", 	    !, { Token = tokAt }
      ;  "#",	    !, { Token = tokHash }
      ;  "~",       !, { Token = tokTilde }
%      ;  "_", 	    !, { Token = tokUnderline }
      ;  digit(D),  !,
            number(D, N),
            { Token = tokNumber(N) }
      ;  letter(L), !, identifier(L, Id),
            {  member((Id, Token), [ (def, tokDef),
                                     (else, tokElse),
                                     (if, tokIf),
                                     (in, tokIn),
                                     (let, tokLet),
                                     (then, tokThen),
				     ('_', tokUnderline)]),
               !
            ;  Token = tokVar(Id)
            }
      ;  [_],
            { Token = tokUnknown }
      ),
      !,
         { Tokens = [Token | TokList] },
      lexer(TokList)
   ;  [],
         { Tokens = [] }
   ).

white_space -->
   [Char], { code_type(Char, space) }, !, white_space.
white_space -->
    "(*", !, comment.
white_space -->
   [].

comment -->
      "*)", !, white_space.
comment -->
      [_], !, comment.
   
digit(D) -->
   [D],
      { code_type(D, digit) }.

digits([D|T]) -->
   digit(D),
   !,
   digits(T).
digits([]) -->
   [].

number(D, N) -->
   digits(Ds),
      { number_chars(N, [D|Ds]) }.

letter(L) -->
   [L], { code_type(L, csym) }.

alphanum([A|T]) -->
   [A], { code_type(A, csym); A == 39 }, !, alphanum(T).
alphanum([]) -->
   [].

identifier(L, Id) -->
   alphanum(As),
      { atom_codes(Id, [L|As]) }.



% -------------------------------------------------------------------------



%parse(_Path, Codes, Program) :-
%  Codes = [], Program = [].


parse(_Path, Codes, Program) :-
	phrase(lexer(X), Codes),
	phrase(program(Program), X).

program(Prog) -->
	definicje(Prog).

definicje(Prog) -->
	puste(Prog).

definicje(Prog) --> 
	definicja(P1), !, 
	definicje(P2), 
	{ Prog = [P1|P2] }.

definicje([]) -->
	[].

definicja(Def) -->
	[tokDef, tokVar(Name), tokLParen],
 	wzorzec(P),
	[tokRParen, tokEq], 
	wyrazenie(E),
	{ Def = def(Name, P, E) }.

%wzorzec(W) -->
%	[tokUnderline],
%	{ W = wildcard(no) }.

%wzorzec(W) -->
%	[tokVar(X)],
%	{ W = var(no, X) }.

%wzorzec(W) -->
%	wzorzec(P1),
%	[tokComma],
%	wzorzec(P2),
%	{ W = pair(no, P1, P2) }.
  
%wzorzec(W) -->
%	[tokLParen],
%	wzorzec(W),
%	[tokRParen].

wzorzec(W) -->
	wzor(P1),
	[tokComma], !,
	wzorzec(P2),
	{ W = pair(no, P1, P2) }.

wzorzec(W) -->
	wzor(W).

wzor(W) -->
	[tokUnderline],
	{ W = wildcard(no) }.

wzor(W) -->
	[tokVar(X)],
	{ W = var(no, X) }.

wzor(W) -->
	[tokLParen], !,
	wzorzec(W),
	[tokRParen].

wyrazenie(W) -->
	[tokIf], !,
	wyrazenie(E1),
	[tokThen],
	wyrazenie(E2),
	[tokElse],
	wyrazenie(E3),
	{ W = if(no, E1, E2, E3) }.

wyrazenie(W) -->
	[tokLet], !,
	wzorzec(P),
	[tokEq],
	wyrazenie(E1),
	[tokIn],
	wyrazenie(E2),
	{ W = let(no, P, E1, E2) }.

wyrazenie(W) -->
	wyrazenie_op(W).

%wyrazenie_op(W) -->
%	wyrazenie_proste(W).

wyrazenie_op(W) -->
	wyrazenie_op1(W).

wyrazenie_op1(W) -->
	op_unarny(Op), !,
	wyrazenie_op1(E),
	{ W = op(no, Op, E) }.

wyrazenie_op1(W) -->
	wyrazenie_op2(E1),
	op1(_),
	wyrazenie_op1(E2),
	{ W = pair(no, E1, E2) }.

wyrazenie_op1(W) -->
	wyrazenie_op2(W).

wyrazenie_op2(W) -->
	op_unarny(Op), !,
	wyrazenie_op2(E),
	{ W = op(no, Op, E) }.

wyrazenie_op2(W) -->
	wyrazenie_op3(E1),
	op2(Op),
	wyrazenie_op3(E2),
	{ W = op(no, Op, E1, E2) }.

wyrazenie_op2(W) -->
	wyrazenie_op3(W).

wyrazenie_op3(W) -->
	op_unarny(Op), !,
	wyrazenie_op3(E),
	{ W = op(no, Op, E) }.

wyrazenie_op3(W) -->
	wyrazenie_op4(E1),
	op3(Op),
	wyrazenie_op3(E2),
	{ W = op(no, Op, E1, E2) }.

wyrazenie_op3(W) -->
	wyrazenie_op4(W).

wyrazenie_op4(W) -->
	op_unarny(Op), !,
	wyrazenie_op4(E),
	{ W = op(no, Op, E) }.

wyrazenie_op4(W) -->
	wyrazenie_op5(E),
	wyrazenie_op4(E,W).

wyrazenie_op4(W) -->
	wyrazenie_op5(W).

wyrazenie_op4(Acc, W) -->
	op4(Op), !,
	wyrazenie_op5(X),
	{ Nacc = op(no, Op, Acc, X) },
	wyrazenie_op4(Nacc, W).

wyrazenie_op4(W, W) -->
	[].

wyrazenie_op5(W) -->
	op_unarny(Op), !,
	wyrazenie_op5(E),
	{ W = op(no, Op, E) }.

wyrazenie_op5(W) -->
	wyrazenie_proste(E),
	wyrazenie_op5(E, W).

%wyrazenie_op5(W) -->
%	wyrazenie_proste(W).
	
wyrazenie_op5(Acc, W) -->
	op5(Op), !,
	wyrazenie_proste(X),
	{ Nacc = op(no, Op, Acc, X) },
	wyrazenie_op5(Nacc, W).

wyrazenie_op5(W, W) -->
	[].


op1(,) -->
	[tokComma], !.

op2(=) -->
	[tokEq], !.

op2(<>) -->
	[tokNeq], !.

op2(<) -->
	[tokLt], !.

op2(>) -->
	[tokGt], !.

op2(<=) -->
	[tokLeq], !.

op2(>=) -->
	[tokGeq], !.

op3(@) -->
	[tokAt], !.

op4('|') -->
	[tokOr], !.

op4(^) -->
	[tokPower], !.

op4(+) -->
	[tokPlus], !.

op4(-) -->
	[tokMinus], !.

op5(&) -->
	[tokAmp], !.

op5(*) -->
	[tokTimes], !.

op5(/) -->
	[tokDiv], !.

op5('%') -->
	[tokMod], !. 
	
op_unarny(-) -->
	[tokMinus], !.

op_unarny(#) -->
	[tokHash], !.

op_unarny(~) -->
	[tokTilde], !.

wyrazenie_proste(W) -->
	wyrazenie_atomowe(W).

wyrazenie_proste(W) -->
	wybor_bitu(W).

wyrazenie_proste(W) -->
	wybor_bitow(W).

wyrazenie_proste(W) -->
	[tokLParen],
	wyrazenie(W),
	[tokRParen].

%wybor_bitu(W) -->
%	[tokLSq],
%	wyrazenie(E),
%	[tokRSq],
%	wybor_bitu(E, W).

wybor_bitu(W) -->
	[tokLSq],
	wyrazenie(E),
	[tokRSq],
	wybor_bitu(E, W).

wybor_bitu(W) -->
	wyrazenie_atomowe(E),
	wybor_bitu(E, W).

wybor_bitu(Acc, W) -->
	[tokLSq],
	wyrazenie(E),
	[tokRSq],
	{ Nacc = bitsel(no, Acc, E) },
	wybor_bitu(Nacc, W).

wybor_bitu(W, W) -->
	[].

%wybor_bitow(W) -->
%	wyrazenie_proste(E1),
%	[tokLSq],
%	wyrazenie(E2),
%	[tokDots],
%	wyrazenie(E3),
%	[tokRSq],
%	{ W = bitsel(no, E1, E2, E3) }.

wybor_bitow(W) -->
	[tokLSq],
	wyrazenie(E),
	[tokRSq],
	wybor_bitow(E, W).

wybor_bitow(W) -->
	wyrazenie_atomowe(E),
	wybor_bitow(E, W).

wybor_bitow(Acc, W) -->
	[tokLSq],
	wyrazenie(E1),
	[tokDots],
	wyrazenie(E2),
	[tokRSq],
	{ Nacc = bitsel(no, Acc, E1, E2) },
	wybor_bitow(Nacc, W).

wybor_bitow(W, W) -->
	[].


wyrazenie_atomowe(W) -->
	wyw_funkcji(W).

wyrazenie_atomowe(W) -->
	zmienna(W).

wyrazenie_atomowe(W) -->
	num(W).

wyrazenie_atomowe(W) -->
	pusty_w(W).

wyrazenie_atomowe(W) -->
	bit(W).

zmienna(X) -->
	[tokVar(N)],
	{ X = var(no, N) }.

wyw_funkcji(F) -->
	[tokVar(Name), tokLParen], !,
	wyrazenie(E),
	[tokRParen],
	{ F = call(no, Name, E) }.

pusty_w(W) -->
	[tokLSq, tokRSq], !,
	{ W = empty(no) }.

num(X) -->
	[tokNumber(N)],
	{ X = num(no, N) }.

bit(B) -->
	[tokLSq],
	wyrazenie(E), !,
	[tokRSq],
	{ B = bit(no, E) }.

puste([]) -->
	[].



























