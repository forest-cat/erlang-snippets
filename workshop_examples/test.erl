-module(test).
-author(hi).
-compile(export_all).

myproc() ->
	timer:sleep(25000),
	exit(reason).

montest(TargetPid) ->
% Process A
{monitor, Pid} = erlang:monitor(process, TargetPid),

% Receiving monitor messages
receive
	{'DOWN', Ref, process, Pid, Reason} ->
		% Handle process termination
		io:format("~p -> ~p~n", [Pid, Reason]);
	_ -> miau
end.
