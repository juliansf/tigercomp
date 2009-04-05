structure TigerMakeGraph :> TigerMakeGraph =
struct
	open TigerError
	open tigertab
	
	structure A = TigerAssem
	structure Set = TigerSet
	
	fun instrs2graph linstr =
		let
			val control = TigerGraph.newGraph ()
			val def = TigerGraph.newTable ()
			val use = TigerGraph.newTable ()
			val ismove = TigerGraph.newTable ()
		
			fun setup instr =
				let
					val node = TigerGraph.newNode control
					
					fun f (A.OPER {dst=dst, src=src,...}) = (dst, src, false)
					 |	f (A.MOVE {dst=dst, src=src,...}) = ([dst], [src], true)
					 |	f (A.LABEL _) = ([], [], false)
					 
					val result = f instr
				in
					tigertab.insert def (node, (#1 result));
					tigertab.insert use (node, (#2 result));
					tigertab.insert ismove (node, (#3 result));
					node
				end
			
			val lnodes = List.map setup linstr
			val lNodesIns = ListPair.zip (lnodes, linstr)
			
			fun mkFlow [] = ()
			 |	mkFlow [_] = ()
			 |	mkFlow ((node,ins)::(node', ins')::instrs) =
				let
					fun addLabel nd k lab =
						let 
							fun p (x,i) = case i of A.LABEL {lab=l,...} => lab = l | _ => false
						in
							case List.find p lNodesIns of
								SOME (n, A.LABEL {lab=l,...}) => {from=nd, to=n}
							|	_ => Error (ErrorInternalError "problemas con TigerMakeGraph.mkFlow.addLabel!", 0)
						end	
				in
					List.app TigerGraph.mk_edge (
						case ins of
							A.OPER {jump=NONE,...} => [{from=node, to=node'}]
						| A.OPER {jump=SOME llst,...} => List.map (addLabel node 0) llst
						| A.MOVE _ => [{from=node, to=node'}]
						| A.LABEL _ => [{from=node, to=node'}]);
					mkFlow ((node', ins')::instrs)
				end
			
			val _ = mkFlow lNodesIns
		in
			(TigerFlow.FGRAPH {control=control, def=def, use=use, ismove=ismove}, lnodes)
		end
end
