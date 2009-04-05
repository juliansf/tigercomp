structure TigerRegAlloc :> TigerRegAlloc =
struct
	open TigerMakeGraph
	open TigerLiveness
	open TigerColor
	open tigertab
	open TigerError
	open TigerUtils
	
	structure A = TigerAssem
	structure Frame = TigerFrame
	
	type allocation = (TigerTemp.temp, Frame.register) tigertab.Tabla
	
	fun member xs x = List.exists (fn y => x = y) xs
	
	fun alloc (li, frame: Frame.frame) =
		let
			(* Build *)
			val (flowgraph, fg_nodes) = instrs2graph li
			val (intergraph, spillCost) = interferenceGraph (flowgraph)
			
			(*DEBUG*)(*
			val (igraph, gtemp) = case intergraph of IGRAPH {graph,gtemp,...} => (graph,gtemp)
			val _ = print (TigerTemp.labelname (Frame.getFrameLabel frame) ^ ":\n")
			val _ = List.app (fn n => print (TigerTemp.tempname (gtemp n) ^ " = " ^ int (spillCost n) ^ "\n")) (TigerGraph.nodes igraph)
			(*val _ = TigerLiveness.show(intergraph)*)
			*)(*DEBUG*)
			
			(* Color *)
			val (regalloc, spills) = 
				color {interference=intergraph,
							 initial=Frame.tempMap,
							 spillCost=spillCost,
							 registers=TigerFrame.registers}
		in
			(*DEBUG*)(*
			print ":::: RegAlloc ::::\n";
			List.app (fn (t,r) => print (TigerTemp.tempname t ^ "->" ^ TigerTemp.tempname r ^ "\n") ) (tigertab.tabAList regalloc);
			*)(*DEBUG*)
			if [] = spills 
			then (cleanSillyMoves li regalloc, regalloc)
			else ((*List.app (print o TigerTemp.tempname) spills;*) alloc (rewriteCode(li, frame, spills), frame))
		end
	
	and rewriteCode (li, frame, spills) = 
		let
			(* Reescribe una instruccion *)
			fun single_rw is_src t offset i =
				let 
					val t' = TigerTemp.newtemp()
					val repl = List.map (fn x => if x=t then t' else x)
					val id = fn x => x
					val (rs, rd, tsl, tdl, ts, td, asm, flip) =
						if is_src then 
							(repl, id, [], [t'], fn _ => t', id, "lwz `d0, ", fn (x,y) => [x,y]) 
						else 
							(id, repl, [t'], [], id, fn _ => t', "stw `s0, ", fn (x,y) => [y,x])
				in
					flip(
						A.OPER {assem=asm ^ int offset ^ "(r1)\n", dst=tdl, src=tsl, jump=NONE},
						case i of
							A.OPER {assem, dst, src, jump} => 
				 							A.OPER {assem=assem, 
				 											dst=rd dst, 
				 											src=rs src, 
				 											jump=jump}
				 		| A.MOVE {assem, dst, src} => A.MOVE {assem=assem, dst=td dst, src=ts src}
				 		| A.LABEL _  => 
				 				Error (ErrorInternalError "problemas con RegAlloc.rewriteCode.single_rw!", 0)
				 	)
				 end
			
			(* Procesa los temporarios fuente *)
			fun rewrite_src t offset [] = []
			 |	rewrite_src t offset (i::li) =
			 	let
			 		val src = case i of
			 				A.OPER {src,...} => src
			 			| A.MOVE {src,...} => [src]
			 			| A.LABEL _ => []
			 	in
			 		(if member src t then single_rw true t offset i
			 		 else [i]) @ rewrite_src t offset li
			 	end
			
			(* Procesa los temporarios destino *)
			and rewrite_dst t offset [] = []
			 |	rewrite_dst t offset (i::li) =
			 	let
			 		val dst = case i of
			 				A.OPER {dst,...} => dst
			 			| A.MOVE {dst,...} => [dst]
			 			| A.LABEL _ => []
			 	in
			 		(if member dst t then single_rw false t offset i
			 		 else [i]) @ rewrite_dst t offset li
			 	end
			
			fun procSpills (t,li') =
				let
					val offset = Frame.getTempOffset frame t
				in
					rewrite_dst t offset (rewrite_src t offset li')
				end
		in
			List.foldl procSpills li spills
		end
	
	and cleanSillyMoves li alloc = 
		List.filter (fn i =>
			case i of
				A.MOVE{dst=d, src=s,...} => not ( valOf(tabSearch alloc d) = valOf(tabSearch alloc s))
			|	_ => true) li
end