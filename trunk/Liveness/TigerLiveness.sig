signature TigerLiveness =
sig
	datatype igraph =
		IGRAPH of {graph: TigerGraph.graph,
							 tnode: TigerTemp.temp -> TigerGraph.node,
							 gtemp: TigerGraph.node -> TigerTemp.temp,
							 moves: (TigerGraph.node * TigerGraph.node) list}
	
	val interferenceGraph : 
				TigerFlow.flowgraph -> 
					igraph * (TigerGraph.node -> int)

	val show : igraph -> unit
end
