structure TigerFlow =
struct
	structure Graph = TigerGraph
	datatype flowgraph =
		FGRAPH of {control: Graph.graph,
							 def: (TigerTemp.temp list) Graph.table,
							 use: (TigerTemp.temp list) Graph.table,
							 ismove: (Graph.node, bool) tigertab.Tabla}
end