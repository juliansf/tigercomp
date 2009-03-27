signature TigerLiveness =
sig
	datatype igraph =
		IGRAPH of {graph: TigerGraph.graph,
							 tnode: TigerTemp.temp -> TigerGraph.node,
							 gtemp: TigerGraph.node -> TigerTemp.temp,
							 moves: (TigerGraph.node * TigerGraph.node) list}
	
	val interferenceGraph : 
				TigerFlow.flowgraph -> 
					igraph * (TigerFlow.Graph.node -> TigerTemp.temp list)

	val show : igraph -> unit
end
