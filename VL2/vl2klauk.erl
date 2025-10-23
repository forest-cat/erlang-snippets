-module('vlAD').
-author(klc).
%-export([square/1]).
-compile(export_all).

square(X) -> XQ = X * X,
			io:format("~p^2 = ~p\n",[X,XQ]),
			XQ.

cube(X) -> square(X) * X.


squareB(X)  when (X =< 5) -> XQ = X + X,
				io:format("~p*2 = ~p\n",[X,XQ]),
				XQ;
squareB(X) when (X > 5) -> XQ = X * X,
				io:format("~p^2 = ~p\n",[X,XQ]),
				XQ.

sum([]) -> 0;
sum([Head|Tail]) -> Head + sum(Tail).

sumr([]) -> 0;
sumr([H|T]) -> Sum = H + sumr(T),
				io:format("aktuelles Element: ~p\n",[H]),
				 Sum.

sumi(Liste) -> sumi(Liste,0).
sumi([],Akku) -> Akku;				 
sumi([Head|Tail],Akku) -> io:format("aktuelles Element: ~p\n",[Head]),
						  sumi(Tail,Head+Akku).				 

test_if(A, B) ->
    if 
%test_if(A, B) when A == 5 ->	
        A == 5 ->
            io:format("A == ~p~n", [A]),
            a_equals_5;
%test_if(A, B) when B == 6 ->	
        B == 6 ->
            io:format("B == 6~n", []),
            b_equals_6;
        A == 2, B == 3 ->                      %That is A equals 2 and B equals 3
            io:format("A == 2, B == 3~n", []),
            a_equals_2_b_equals_3;
        A == 1 ; B == 7 ->                     %That is A equals 1 or B equals 7
            io:format("A == 1 ; B == 7~n", []),
            a_equals_1_or_b_equals_7
    end.
	
test_case(A) when is_integer(A) ->
		case (A rem 2) of
			_Y when (A rem 2) == 0 -> gerade;
			1 -> ungerade;
			X when X > 1 -> io:format("falsche Eingabe ~p\n",[X])
		end.

% mymember(<Element>,<Liste>)		
mymember(_El,[]) -> false;		
mymember(El,[El|_T]) -> true;	
mymember(El,[_H|T]) -> mymember(El,T).	

memberCase(El,Li) ->
		case Li of
			[] -> false;
			[El|_Rest] -> true;
			[_H|Rest] -> memberCase(El,Rest)
		end.		

add1([Head|Tail]) -> [Head+1 | add1(Tail)];
add1([]) -> [].

add2(Liste) -> add2(Liste,[]).
%add2([Head|Tail],Akku) -> add2(Tail,[Head+1|Akku]);
add2([Head|Tail],Akku) -> add2(Tail,Akku++[Head+1]);
add2([],Akku) -> Akku.

deleteLast([]) -> [];
deleteLast([_H]) -> [];
deleteLast([H|T]) -> [H|deleteLast(T)].
 
deleteFirst([]) -> [];
deleteFirst([_H|T]) -> T.

% substitute(<Element>, <Substitut>, <Liste>, <Position: eRSTES,lETZTES,aLLE>)
substitute(_E,_S,[],_P) -> [];
substitute(E,S,[E|T],e) -> [S|T];
substitute(E,S,[E|T],a) -> [S|substitute(E,S,T,a)];
substitute(E,S,[H|T],P) -> [H|substitute(E,S,T,P)].

 