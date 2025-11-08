-module('dijkstranew').
-author(tmj).
%-export(square/[1]).
-compile(export_all).

%====================================================================%
%			Dijkstra Test Functions	
%====================================================================%

% Test the dijkstra easy and quick
% test_dijkstra().
test_dijkstra() -> test_dijkstra('A', 'E').
test_dijkstra(StartVertex, TargetVertex) ->
				Graph = init_test_graph(),
				DijkstraTable = dijkstra_init_table(digraph:vertices(init_test_graph()), StartVertex),
				StartVertexTuple = get_vertex_tuple(DijkstraTable, StartVertex),
				dijkstra_run(
					Graph,
					DijkstraTable,
					StartVertexTuple, 
					TargetVertex).

% Initialise and return a test graph 
% init_test_graph() -> G.
init_test_graph() -> init_test_graph([
			    {'A','C',20}, 
			    {'A','B',10}, 
			    {'B','E',11}, 
			    {'B','D',50}, 
			    {'C','D',20}, 
			    {'C','E',33}, 
			    {'D','F',2}, 
			    {'E','F',1}]).

% Initialise a new graph from the given List
% init_test_graph(List) -> G. 
init_test_graph(List) -> init_test_graph(List, digraph:new()).
init_test_graph([], G) -> G;
init_test_graph([{V1}|T], G) -> digraph:add_vertex(G, V1),
				init_test_graph(T, G);
init_test_graph([{V1, V2, Weight}|T], G) -> digraph:add_vertex(G, V1),
				digraph:add_vertex(G, V2),
				digraph:add_edge(G, V1, V2, Weight),
				init_test_graph(T, G).

%====================================================================%
%			Helper Functions
%====================================================================%

% Return the VertexTuple from for the given Vertex from DijkstraTable
% get_vertex_tuple(DijkstraTable, Vertex) -> {Name, Distance, Parent, Visited}.
get_vertex_tuple([H|T], Vertex) -> {HVertexname, _HDistance, _HParent, _HVisited} = H,
	case HVertexname =:= Vertex of
		true -> H;
		false -> get_vertex_tuple(T, Vertex)
	end.

% Retrieve the Vertex with the lowest score in the DijkstraTable
% get_lowest_vertex(DijkstraTable) -> {Name, Distance, Parent, Visited}.
get_lowest_vertex([H|T]) -> {_, _, _, HDone} = H, 
	case HDone =:= true of
		true -> get_lowest_vertex(T);
		false -> get_lowest_vertex(T, H)
	end.
get_lowest_vertex([], Acc) -> Acc;
get_lowest_vertex([H|T], Acc) -> {_,HDist,_,HDone} = H,
				 {_,AccDist,_,_} = Acc,
	if
		HDone =:= true -> get_lowest_vertex(T, Acc);
		HDist =:= infinite -> get_lowest_vertex(T, Acc);
		HDist < AccDist -> get_lowest_vertex(T, H);
		HDist >= AccDist -> get_lowest_vertex(T, Acc)
	end.

%====================================================================%
%			Dijkstra Functions	
%====================================================================%

% Create the DijkstraTable with inital values
% dijkstra_init_table(Vertices, StartVertex) -> DijkstraTable.
dijkstra_init_table(Vertices, StartVertex) -> dijkstra_init_table(Vertices, [], StartVertex).
dijkstra_init_table([], DijkstraTable, _) -> DijkstraTable;
dijkstra_init_table([VertHead|VertTail], DijkstraTable, StartVertex) when VertHead =:= StartVertex -> dijkstra_init_table(
								 VertTail, 
								 [{VertHead, 0, StartVertex, false}|DijkstraTable],
								 StartVertex); 
dijkstra_init_table([VertHead|VertTail], DijkstraTable, StartVertex) -> dijkstra_init_table(VertTail, 
								[{VertHead, infinite, none, false}|DijkstraTable], 
								StartVertex). 

% Updates the DijkstraTable from the CurrentVertex
% update_dijkstra_table(Graph, DijkstraTable, Neighbours, CurrentVertex) -> DijkstraTable.
update_dijkstra_table(Graph, DijkstraTable, CurrentVertex) -> lists:reverse(update_dijkstra_table(Graph, DijkstraTable, [], CurrentVertex)).
update_dijkstra_table(Graph, [DTH|DTT], NewTable, CurrentVertex) ->
			{CuVeName, CuVeDist, CuVeParent, _CuVeVisited} = CurrentVertex,
			{DTHName, _DTHDist, _DTHParent, _DTHVisited} = DTH,
			NeighbourEdges = digraph:out_edges(Graph, CuVeName),
			case DTHName =:= CuVeName of
				true -> UpdatedVertex = {CuVeName, CuVeDist, CuVeParent, true};
				false -> UpdatedVertex = update_table_entry(Graph, DTH, CurrentVertex, NeighbourEdges)
			end,
			update_dijkstra_table(Graph, DTT, [UpdatedVertex|NewTable], CurrentVertex);
update_dijkstra_table(_Graph, [], NewTable, _CurrentVertex) -> NewTable.

% Update the table entry (CurrentVertex) with all new connections from it
% update_table_entry(Graph, ExamineVertex, CurrentVertex, NeighbourEdgesFromCurrentVertex)
update_table_entry(_Graph, ExamineVertex, _CurrentVertex, []) -> ExamineVertex;
update_table_entry(Graph, ExamineVertex, CurrentVertex, [H|T]) ->
		{Name, Dist, _Parent, _Visited} = ExamineVertex,
		{CuVeName, CuVeDist, CuVeParent, _CuVeVisited} = CurrentVertex,
		{_Edge, _VOut, VIn, EWeight} = digraph:edge(Graph, H),
		if 
			VIn =:= Name ->
				if
					Name =:= CuVeName -> {CuVeName, CuVeDist, CuVeParent, true};
					Dist =:= infinite -> {Name, EWeight + CuVeDist, CuVeName, false};
					Dist > EWeight + CuVeDist -> {Name, EWeight + CuVeDist, CuVeName, false};
					Dist =< EWeight + CuVeDist -> ExamineVertex
				end;
			true -> update_table_entry(Graph, ExamineVertex, CurrentVertex, T)
		end.

% Run the Dijkstra main loop
% dijkstra_run(Graph, DijkstraTable, StartVertex, TargetVertex) -> DijkstraTable.
dijkstra_run(G, DijkstraTable, StartVertex, TargetVertex) -> 
	io:format("StartVertex: ~p~n", [StartVertex]),
	dijkstra_run(G, DijkstraTable, TargetVertex).
dijkstra_run(G, DijkstraTable, TargetVertex) ->
	CurrentVertex = get_lowest_vertex(DijkstraTable),
	io:format("Examining: ~p~n",[CurrentVertex]),
	{CuVeName, _CuVeDist, _CuVeParent, _CuVeVisited} = CurrentVertex,
	if 
		CuVeName =:= TargetVertex -> io:format("~n=====> Done <=====~n"),
					     DijkstraTable; % Target reached
		true -> 
			NewTable = update_dijkstra_table(G, DijkstraTable, CurrentVertex),
			dijkstra_run(G, NewTable, TargetVertex)
	end.

