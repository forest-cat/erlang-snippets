-module(eulerggt).  % module attribute
-export([ggt/2]).   % module attribute

% ggt(<Integer>, <Integer>) calculates the greatest common divider of two Integers using Eulers Algorithm
ggt(A, B) when A =< 0; B =< 0; is_integer(A) =/= true; is_integer(B) =/= true -> io:format("Fehler in ggt: Parameter ~p,~p sind nicht beide eine positive natürliche Zahl!\n", [A,B]),
												 0;
ggt(A, B) when A =:= B -> A;
ggt(A, B) when A > B -> ggt(A-B, B);
ggt(A, B) -> ggt(B - A, A).
