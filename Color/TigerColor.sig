signature TigerColor =
sig
	type allocation = (TigerTemp.temp, TigerFrame.register) tigertab.Tabla
	
	val color : {interference: TigerLiveness.igraph,
							 initial: allocation,
							 spillCost: TigerGraph.node -> int,
							 registers: TigerFrame.register list}
					-> allocation * TigerTemp.temp list
end