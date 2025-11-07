-module(adtest).
-author(tmj).
-compile(export_all).
%-export([diagram/1, diff_list/2])
 
% diagram(N) returns a list of the length N following the Test Ablaufdiagram 5
diagram(N) when N < 0 -> io:format("Fehlerhafte Eingabe: ~p~n", [N]);
diagram(N) when is_integer(N) =/= true -> io:format("Fehlerhafte Eingabe: ~p~n", [N]);
diagram(0) -> [];
diagram(1) -> [1];
diagram(N) -> diagram(N+1, 1, 0, [1|[]]).
 
diagram(N, _Onb, _Twb, Result) when N < 3 -> Result;
diagram(N, Onb, Twb, Result) -> diagram(N-1, Onb+Twb, Onb, [Onb+Twb|Result]).
 
 
% is_member(Element, List) checks whether Element is a member in the given List
is_member(_El, []) -> false;
is_member(El, [El|_T]) -> true;
is_member(El, [_|T]) -> is_member(El, T).
 
% reverse_list(List) returns a copy of the given List reversed
reverse_list(List) -> reverse_list(List, []).
 
reverse_list([], Acc) -> Acc;
reverse_list([H|T], Acc) -> reverse_list(T, [H|Acc]).
 
 
% diff_list(L1, L2) returns a list of all Elements which exist in L1, but not in L2
diff_list(L1, L2) -> reverse_list(diff_list(L1, L2, [])).
 
diff_list([], _L2, Acc) -> Acc;
diff_list([H|T], L2, Acc) -> 
	case is_member(H, L2) of
		true -> diff_list(T, L2, Acc);
		false -> diff_list(T, L2, [H|Acc])
	end.
 
