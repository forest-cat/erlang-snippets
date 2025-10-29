-module('dijkstra').
-author(tmj).
%-export(square/[1]).
-compile(export_all).
%-import(digraph).

% init_graph(List) -> init_graph(List, digraph:new()).
% init_graph([], G) -> G;
% init_graph([H|T], G) -> digraph:add_vertex(G, H),
	
init_graph(List) -> init_graph(List, digraph:new()).
init_graph([], G) -> G;
init_graph([{V1}|T], G) -> digraph:add_vertex(G, V1),
				init_graph(T, G);
init_graph([{V1, V2}|T], G) -> digraph:add_vertex(G, V1),
				digraph:add_vertex(G, V2),
				digraph:add_edge(G, V1, V2, miau),
				init_graph(T, G).

