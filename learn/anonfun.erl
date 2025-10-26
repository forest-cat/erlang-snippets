-module('anonfun').
-author(tmj).
%-export(square/[1]).
-compile(export_all).

% maps all items of the given list through predicate
map(_, []) -> [];
map(F, [H|T]) -> [F(H) | map(F, T)].


% sums, finds min/max and more
fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(H,Start), T).

% Example call for fold with a predicate anonymous function
%fold(fun(A,B) -> A + B end, 0, lists:seq(1,6)).
