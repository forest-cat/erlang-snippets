-module('dijkstra').
-author(tmj).
%-export(square/[1]).
-compile(export_all).
%-import(digraph).

% init_graph(List) -> init_graph(List, digraph:new()).
% init_graph([], G) -> G;
% init_graph([H|T], G) -> digraph:add_vertex(G, H),
%
%
% dijkstra:init_graph([{'A','C',20}, {'A','B',10}, {'B','E',10}, {'B','D',50}, {'C','D',20}, {'C','E',33}, {'D','F',2}, {'E','F',1}]).


test_dijkstra() -> init_dijkstra(digraph:vertices(init_graph()), 'A').

init_graph() -> init_graph([{'A','C',20}, 
			    {'A','B',10}, 
			    {'B','E',10}, 
			    {'B','D',50}, 
			    {'C','D',20}, 
			    {'C','E',33}, 
			    {'D','F',2}, 
			    {'E','F',1}]).
init_graph(List) -> init_graph(List, digraph:new()).
init_graph([], G) -> G;
init_graph([{V1}|T], G) -> digraph:add_vertex(G, V1),
				init_graph(T, G);
init_graph([{V1, V2, Weight}|T], G) -> digraph:add_vertex(G, V1),
				digraph:add_vertex(G, V2),
				digraph:add_edge(G, V1, V2, Weight),
				init_graph(T, G).

% Initialise the dijkstra algorithm
init_dijkstra(Vertices, StartVertex) -> init_dijkstra(Vertices, [], StartVertex).
init_dijkstra([], UnvisitedSet, _) -> UnvisitedSet;
init_dijkstra([VertHead|VertTail], UnvisitedSet, StartVertex) when VertHead =:= StartVertex -> init_dijkstra(
								 VertTail, 
								 [{VertHead, 0, StartVertex, false}|UnvisitedSet],
								 StartVertex); 
init_dijkstra([VertHead|VertTail], UnvisitedSet, StartVertex) -> init_dijkstra(VertTail, 
								[{VertHead, infinite, none, false}|UnvisitedSet], 
								StartVertex). 

% Retrieve the Vertex with the lowest score in the UnvisitedSet
get_lowest_vertex([H|T]) -> {_,Acc,_,_} = H,
			get_lowest_vertex(T, Acc).
get_lowest_vertex([], Acc) -> Acc;
get_lowest_vertex([H|T], Acc) -> {_,HDist,_,_} = H, 
	if 
		HDist == infinite -> get_lowest_vertex(T, Acc);
		HDist < Acc -> get_lowest_vertex(T, HDist);
		HDist >= Acc -> get_lowest_vertex(T, Acc)
	end.

% Remove Vertex from the UnvisitedSet
remove_vertex(UnvisitedSet, Vertex) -> remove_vertex(UnvisitedSet, Vertex, []).
remove_vertex([], _, NewSet) -> NewSet;
remove_vertex(UnvisitedSet, Vertex, NewSet) -> {Vert,_,_,_}= H,
	if
		Vertex == Vert -> remove_vertex(UnvisitedSet, 
						Vertex, 
						NewSet);
		true -> remove_vertex(UnvisitedSet, 
				      Vertex, 
				      [H|NewSet])
	end.

% Check whether Vertex exists in the given UnvisitedSet
is_unvisited([H|T], Vertex) -> {SetVertex, _, _, _} = H,
        if
	       Vertex == SetVertex -> true;
	       true -> is_unvisited(T, Vertex)
	end.

% Returns all unvisited Neighbours from the Neighbours
unvisited_out_neighbours(UnvisitedSet, Neighbours) -> unvisited_out_neighbours(UnvisitedSet, 
									       Neighbours,
									       []).
unvisited_out_neighbours(_, [], UnvisitedNeighbours) -> UnvisitedNeighbours;
unvisited_out_neighbours(UnvisitedSet, [H|T], UnvisitedNeighbours) -> 
	case is_unvisited(UnvisitedSet, H) of
		true -> unvisited_out_neighbours(UnvisitedSet, T, [H|UnvisitedNeighbours]);
		false -> unvisited_out_neighbours(UnvisitedSet, T, UnvisitedNeighbours)
	end.

% Checks whether all the vertices in the given List are infinite
all_infinite([]) -> true.
all_infinite([H|T]) -> {_, HDist, _, _} = H, 
	case HDist == infinite of
		true -> all_infinite(T);
		false -> false
	end.

% Runs the dijkstra main algorithm loop
dijkstra(UnvisitedSet, DijkstraTable, StartVertex, TargetVertex) -> dijkstra(, 
								     remove_vertex(UnvisitedSet, CurrentVertex), 
								     DijkstraTable,
								     get_lowest_vertex(UnvisitedSet), 
								     TargetVertex).
dijkstra(


examine_vertex(UnvisitedSet, Neighbours, CurrentVertex, TargetVertex) -> examine_vertex(
								   unvisited_out_neighbours(UnvisitedSet, Neighbours),
								   Neighbours,
								   UnvisitedSet,
								   CurrentVertex,
								   TargetVertex).
examine_vertex([H|T], UnvisitedSet, Neighbours, CurrentVertex, TargetVertex) -> {_, HDist, _, _} = H,
								    {_, CDist, _, _} = CurrentVertex,
	case of HDist < CDist  
examine_vertex([], UnvisitedSet, CurrentVertex, TargetVertex) -> 
	case CurrentVertex == TargetVertex of
		true -> io:format("Path found");
		false -> 
			case all_infinite(UnvisitedSet) of
				true -> io:format("No Path exists");
				false ->  

	end.
% unvisited_out_neighbours(UnvisitedSet, Vertex, Graph) -> unvisited_out_neighbours(UnvisitedSet, [], Vertex, Graph).
% unvisited_out_neighbours([], Neighbours, _, _) -> Neighbours;
% unvisited_out_neighbours([H|T], Neighbours, Vertex, Graph) -> {UnvisitedVertext,_,_,_} = H,
% 	if
% 		Vertex == UnvisitedVertext -> [ListVertex|Neighbours]
% 

%dijkstra() -> 
