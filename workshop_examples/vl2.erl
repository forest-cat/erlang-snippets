-module('vl2').
-author(tmj).
%-export(square/[1]).
-compile(export_all).
-import(vltest, [fact/1]).

square(X) -> XQ = X * X,
	     %XB = fact(X),
	 %io:format("~p! = ~p\n", [X,XB]),
	 io:format("~p^2 = ~p\n", [X,XQ]).


mymember(_EL, []) -> false;
% mymember(El, [H|T]) when H =:= El -> true;
mymember(El, [El|_T]) -> true;
mymember(El, [_H|T]) -> mymember(El, T).


add(Inc, [Head | Tail]) -> [Head+Inc | add(Inc, Tail)];
add(_Inc, []) -> [].

%add2(Liste) -> add(Liste, []).
%add2([H|T], Acc) -> add2(

% removelast([]) -> [];
% removelast([H]) -> [];
% removelast([H|T]) -> [H|removelast(T)].


invertList(List) -> invertList(List, []).
invertList([H|T], InvertedList) -> invertList(T, [H|InvertedList]);
invertList([], InvertedList) -> InvertedList.

% Removes the last element from a list removelasttail(<List>)
removelasttail(List) -> removelasttail(List, []).
removelasttail([_], Acc) -> invertList(Acc);
removelasttail([H|T], Acc) -> removelasttail(T, [H|Acc]).

% zip(<List>, <List>)
zip(L1, L2) -> lists:reverse(zip(L1, L2, [])).

zip([], _, Acc) -> Acc;
zip(_, [], Acc) -> Acc;
zip([H1|T1], [H2|T2], Acc) -> zip(T1, T2, [{H1, H2}|Acc]).



% substitute(_A, _B, [], _P) -> [].
% substitute(A, B, [E|Tail],e) -> [B|T];
% substitute(A, B, [E|Tail],e) -> [B|substitute(A,B,T,a];
% substitute(A, B, [H|T],P) -> [A,B,T,P].
