signature TigerMakeGraph =
sig
	val instrs2graph : TigerAssem.instr list ->
				TigerFlow.flowgraph * TigerFlow.Graph.node list
end
