structure TigerLiveness :> TigerLiveness =
struct
	open tigertab
	open TigerError
	
	structure L = TigerUtils.Lst
	
	datatype igraph =
		IGRAPH of {graph: TigerGraph.graph,
							 tnode: TigerTemp.temp -> TigerGraph.node,
							 gtemp: TigerGraph.node -> TigerTemp.temp,
							 moves: (TigerGraph.node * TigerGraph.node) list}
	
	type liveSet = (TigerTemp.temp, unit) Tabla
	type liveMap = liveSet TigerGraph.table
	
	fun mapFind (t : ('a,'b) Tabla) n = 
				case tabSearch t n of
					SOME v => v
				| NONE => Error (ErrorInternalError "problemas con TigerLivennes.mapFind!", 0)
	
	fun makeLiveMap (TigerFlow.FGRAPH {control, def, use, ismove}) =
		let
			val out : liveMap = TigerGraph.newTable ()
			val _ = List.app (fn (n,_) => (tabRInsert out (n, tabNueva());())) (tabAList def)
			
			fun live (n,tl) =
				let
					fun liveOut temp node =
						let
							val ltmap = valOf (tabSearch out node)
						in
							case tabSearch ltmap temp of
								SOME _ => ()
							| NONE =>
									let val defl = valOf (tabSearch def node)
									in 
										if List.exists (fn x => temp=x) defl then ()
										else (tabRInsert ltmap (temp, ());
													List.app (liveOut temp) (TigerGraph.pred node)) 		
									end
						end
				in
					(*DEBUG*)(*
					print (TigerGraph.nodename n ^ " -> "); 
					List.app (fn t => print (TigerGraph.nodename t ^ " : ")) (TigerGraph.pred n);
					print "\n";
					*)(*DEBUG*)
					
					List.app (fn t => List.app (liveOut t) (TigerGraph.pred n)) tl
				end

		in
			List.app live (tabAList use);
			(*DEBUG*)(*print "\n----\n"; *)(*DEBUG*)
			out
		end
	
	fun interferenceGraph (fgraph as TigerFlow.FGRAPH {control, def, use, ismove}) =
		let
			val ig = TigerGraph.newGraph ()
			val tnodeMap = tabNueva ()
			val ntempMap = TigerGraph.newTable ()
			val liveOutMap = makeLiveMap fgraph
			
			(* Mapping de nodos del fgraph a listas de temporarios vivos *)
			fun livetemps n = List.map (fn (t,_) => t) (tabAList (mapFind liveOutMap n))
			
			fun tnode t =
				case tabSearch tnodeMap t of
					SOME n => n
				| NONE => 
						let val n = TigerGraph.newNode ig 
						in 
							tabInsert tnodeMap (t,n); 
							tabInsert ntempMap (n,t);
							n 
						end
			
			fun makeIGraph () =
				let
					fun proc_nodes ([],_,_) = []
					 |	proc_nodes ((n,dl)::defl', (_,ul)::usel',(_,ismv)::ismovel') =
					 			let
					 				val ltemps = livetemps n
					 				
					 				(*DEBUG*)(*
					 				val _ = (print (TigerGraph.nodename n ^ " -> "); 
					 								 List.app (fn t => print (TigerTemp.tempname t ^ " : ")) ltemps;
					 								 print "\n")
					 				*)(*DEBUG*)
					 				
					 				fun mkEdge d t = 
					 					if d = t then ()
					 					else if ismv andalso List.exists (fn x => x=t) ul then () 
					 					else if (TigerGraph.is_adj (tnode d, tnode t)) then ()
					 					else TigerGraph.mk_edge {from=tnode d, to=tnode t}
					 				
					 				val lmv = proc_nodes (defl', usel', ismovel')
					 			in
					 				List.app (fn d => List.app (mkEdge d) ltemps) dl;
					 				if ismv then 
					 					let 
					 						val a = tnode (List.hd dl)
					 						val b = tnode (List.hd ul)	
					 					in (a,b) :: lmv end
					 				else lmv
					 			end
					  | proc_nodes _ = 
					  		Error (ErrorInternalError "problemas con TigerLiveness.interferenceGraph.makeIGraph!", 0)
				in
					List.app (fn n => (tnode n;())) TigerFrame.registers;
					proc_nodes(tabAList def, tabAList use, tabAList ismove)
				end
			
			val mvl = makeIGraph ();
		in
			
			(IGRAPH {graph=ig, tnode=mapFind tnodeMap, gtemp=mapFind ntempMap, moves=mvl}, livetemps)
		end

	(*fun show (IGRAPH {graph, tnode, gtemp, moves}) = TigerGraph.printGraph false graph*)
	fun show (IGRAPH {graph, tnode, gtemp, moves}) =
  	let
  		fun print_adj [] = print ""
  		 |	print_adj (n::adj') = (print (TigerTemp.tempname (gtemp n) ^ ":"); print_adj adj')
  		 
  		fun print_node n = (
  					print (TigerTemp.tempname (gtemp n) ^ " -> "); 
  					print_adj (TigerGraph.adj n); print " *\n")
  	in
  		print ":::: iGraph :::::::::::::::::::\n";
  		List.app print_node (TigerGraph.nodes graph);
  		
  		print "\n:::: Moves ::::::::::::::::::::\n";
  		List.app (fn (x,y) => print (TigerTemp.tempname (gtemp x) ^ " <- " ^ TigerTemp.tempname (gtemp y) ^ "\n")) moves
  	end
end