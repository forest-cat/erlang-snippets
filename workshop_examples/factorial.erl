-module(vltest).          % module attribute
-export([fact/1]).   % module attribute

% Recursive
fact(N) when N>0 ->  % beginning of function declaration
    N * fact(N-1);   %  |
fact(0) ->           %  |
    1.               % end of function declaration

% Tail Recursive
factt(N) -> factt(N, 1).

factt(N, Acc) when N>0 ->  factt(N-1, N * Acc);
factt(0, Acc) -> Acc.
