-module(testdijkstra).
-author(tmj).
-compile(export_all).

% init_test_graph() -> G. Initialise and return a test graph 
init_test_graph() -> init_test_graph([
			    {'A','C',20}, 
			    {'A','B',10}, 
			    {'B','E',10}, 
			    {'B','D',50}, 
			    {'C','D',20}, 
			    {'C','E',33}, 
			    {'D','F',2}, 
			    {'E','F',1}]).
% init_test_graph(List) -> G. Initialise a new graph from the given List
init_test_graph(List) -> init_test_graph(List, digraph:new()).
init_test_graph([], G) -> G;
init_test_graph([{V1}|T], G) -> digraph:add_vertex(G, V1),
				init_test_graph(T, G);
init_test_graph([{V1, V2, Weight}|T], G) -> digraph:add_vertex(G, V1),
				digraph:add_vertex(G, V2),
				digraph:add_edge(G, V1, V2, Weight),
				init_test_graph(T, G).
