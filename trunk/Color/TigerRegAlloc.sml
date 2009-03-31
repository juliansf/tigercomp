structure TigerRegAlloc :> TigerRegAlloc =
struct
	open TigerMakeGraph
	open TigerLiveness
	open TigerColor
	
	type allocation = (TigerTemp.temp, TigerTemp.temp) tigertab.Tabla
	
	fun alloc (li, frame:TigerFrame.frame) =
		let
			(* Build *)
			val (flowgraph, fg_nodes) = instrs2graph li
			val (intergraph, live_temps) = interferenceGraph flowgraph
			
			(* Color *)
			val (regalloc, spills) = 
				color {interference=intergraph,
							 initial=tigertab.tabNueva(),
							 spillCost=spillCost,
							 registers=TigerFrame.registers}
		in
			print ":::: RegAlloc ::::\n";
			List.app (fn (t,r) => print (TigerTemp.tempname t ^ "->" ^ TigerTemp.tempname r ^ "\n") ) (tigertab.tabAList regalloc);
			(*if [] = spills 
			then (li, regalloc)
			else alloc (rewriteCode(li, spills), frame)*)
			(li, regalloc)
		end
	
	and rewriteCode (li, spills) = li (* implementar! =b *)
	
	and spillCost n = 1 (* mejorar! =D *)
		
end