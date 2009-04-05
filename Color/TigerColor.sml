structure TigerColor :> TigerColor =
struct
	open tigertab
	open TigerError
	
	structure Utils = TigerUtils
	structure Stack = TigerStack
	structure Graph = TigerGraph
	structure Set = TigerSet
	structure Stack = TigerStack

	type allocation = (TigerTemp.temp, TigerFrame.register) Tabla
	
	(* Comparacion entre pares de nodos - usado para los sets de pares de nodos*)
	fun p_comp ((a,b),(c,d)) = 
		case Graph.comp (a,c) of
			EQUAL => Graph.comp (b,d)
		| ord => ord
	
	fun color {interference: TigerLiveness.igraph, 
						 initial: allocation, 
						 spillCost: Graph.node -> int, 
						 registers: TigerFrame.register list} =
		let
			val TigerLiveness.IGRAPH {graph, tnode, gtemp, moves} = interference
			val K = List.length TigerFrame.registers
			
			(*DEBUG*)(*
			fun pp_pair (n,m) =
					"(" ^ TigerTemp.tempname(gtemp n) ^ ", " ^ TigerTemp.tempname(gtemp m) ^ ")"
			
			(*val _ = TigerLiveness.show interference*)
			*)(*DEBUG*)
			
			fun mkDegreeMap () =
				let
					val dg = TigerGraph.newTable ()
				in
					List.app (fn n => (tabRInsert dg (n, List.length (Graph.adj n));())) (Graph.nodes graph);
					List.app (fn t => (tabRInsert dg (tnode t, valOf(Int.maxInt));())) registers;
					dg
				end
		
			val degreeMap = mkDegreeMap ()
			fun degree n =
				case tabSearch degreeMap n of
					SOME i => i
				| NONE => Error (ErrorInternalError "problemas con TigerColor.color.degree!", 0)
			
			fun setDegree n v = (tabRInsert degreeMap (n, v);())
			
			(* Worklists *)
			val init = Set.empty(Graph.comp)
			val precolored = Set.empty(Graph.comp)
			val simplifyWorklist = Set.empty(Graph.comp)
			val freezeWorklist = Set.empty(Graph.comp)
			val spillWorklist = Set.empty(Graph.comp)
			val spilledNodes = Set.empty(Graph.comp)
			val coalescedNodes = Set.empty(Graph.comp)
			val coloredNodes = Set.empty(Graph.comp)
			val selectStack = Stack.new()
			
			(* Move Sets *)
			val coalescedMoves = Set.empty(p_comp)
			val constrainedMoves = Set.empty(p_comp)
			val frozenMoves = Set.empty(p_comp)
			val worklistMoves = Set.empty(p_comp)
			val activeMoves = Set.empty(p_comp)
			
			(* Other *)
			val adjSet = Set.empty(p_comp)
			val adjList : (Graph.node Set.set) Graph.table = Graph.newTable()
			val moveList : (Graph.node Set.set) Graph.table = Graph.newTable()
			val alias : Graph.node Graph.table = Graph.newTable()
			val color : TigerTemp.temp Graph.table = Graph.newTable()
			
			(* Inicializamos algunas de las estructuras *)
			fun setLists [] = ()
			 |	setLists (n::nodes) =
			 	let
			 		val adj_n = Graph.adj n
			 		val adj_n_set = Set.empty(Graph.comp)
			 		
			 		val mvl_n = List.mapPartial (fn (x,y) => 
			 					if Graph.eq(x,n) then SOME y else if Graph.eq(y,n) then SOME x else NONE) moves
			 		val mvl_n_set = Set.empty(Graph.comp)
			 	in
			 		List.app (fn m => Set.addList(adjSet, [(n,m),(m,n)])) adj_n;
			 		Set.addList(adj_n_set, adj_n);
			 		tabRInsert adjList (n, adj_n_set);
			 		Set.addList(mvl_n_set, mvl_n);
			 		tabRInsert moveList (n, mvl_n_set);
			 		(*DEBUG*)(*
			 		print (TigerTemp.tempname(gtemp n) ^ " -> "); 
			 		Set.print (TigerTemp.tempname o gtemp) mvl_n_set; 
			 		print "\n";
			 		*)(*DEBUG*)
			 		setLists nodes
			 	end
				
			val _ = setLists (Graph.nodes graph)
			val _ = Set.addList(precolored, List.map tnode registers)
			val _ = List.app (fn t => (tabRInsert color (tnode t, t);())) registers
			val nodes_set = Set.empty(Graph.comp)
			val _ = Set.addList(nodes_set, Graph.nodes graph)
			val init = Set.difference(nodes_set, precolored)
			
			fun adjacent n = 
				let
					val adj_n = valOf(tabSearch adjList n) handle e => raise Fail "110"
					val sel_stack = Set.empty(Graph.comp)
					val _ = Set.addList (sel_stack, Stack.toList selectStack)
				in
					Set.difference(adj_n, Set.union (sel_stack, coalescedNodes))
				end
			
			fun nodeMoves n =
				let
					val movel_n = Set.empty(p_comp)
					val _ = Set.app (fn m => Set.addList (movel_n, [(n,m),(m,n)])) (valOf (tabSearch moveList n))  handle e => raise Fail "120"
				in
					(*DEBUG*)(*
					print (TigerTemp.tempname (gtemp n) ^ " -> ");
					Set.print pp_pair movel_n;
					*)(*DEBUG*)
					Set.intersection(movel_n, Set.union(activeMoves, worklistMoves))
				end
				
			fun moveRelated n = not (Set.isEmpty (nodeMoves n))
			
			fun addEdge (u,v) =
				if not (Set.member(adjSet, (u,v))) andalso not(Graph.eq(u,v)) then (
					Set.addList(adjSet, [(u,v), (v,u)]);
					
					if not (Set.member(precolored, u)) then (
						Set.add(valOf(tabSearch adjList u), v)  handle e => raise Fail "136";
						setDegree u (degree u + 1)) else ();
					
					if not (Set.member(precolored, v)) then (
						Set.add(valOf(tabSearch adjList v), u)  handle e => raise Fail "140";
						setDegree v (degree v + 1)) else ()
				) else ()
			
			(* MakeWorkList *)
			fun makeWorklist n =
				if degree n >= K then
					Set.add(spillWorklist, n)
				else if moveRelated n then
					Set.add(freezeWorklist, n)
				else
					Set.add(simplifyWorklist, n)
			
			fun enableMoves nodes =
			 	let
			 		fun aux (m as (x,y)) =
			 			let
			 				val xy = Set.member (activeMoves, (x,y))
			 				val yx = Set.member (activeMoves, (y,x))
			 			in
			 				if xy orelse yx then (
			 					if xy then 
			 						Set.delete (activeMoves, (x,y)) handle e => ()
			 					else
			 						Set.delete (activeMoves, (y,x)) handle e => ();
			 					Set.add(worklistMoves, m)
			 				) else ()
			 			end
			 	in
			 		Set.app (fn n => Set.app aux (nodeMoves n)) nodes
			 	end
			
			fun decrementDegree n =
				let
					val d = degree n
				in
					setDegree n (d-1);
					if d = K then (
						enableMoves(Set.union(Set.singleton Graph.comp n, adjacent n));
						Set.delete (spillWorklist, n) handle e => ();
						
						if moveRelated n then 
							Set.add(freezeWorklist, n)
						else
							Set.add(simplifyWorklist, n)
					) else ()
				end
			
			fun Simplify () =
				let
					val n = Set.getElement simplifyWorklist
					val adj_n_set = adjacent n
				in
					Set.delete(simplifyWorklist, n) handle e => raise Fail "177";
					Stack.push selectStack n;
					Set.app decrementDegree adj_n_set
				end
			(*
				let
					val swl = Set.listItems simplifyWorklist
					
					fun aux n =
						let 
							val adj_n = adjacent n
						in
							Set.delete(simplifyWorklist, n) handle e => raise Fail "119";
							Stack.push selectStack n;
							Set.app decrementDegree adj_n
						end
				in
					List.app aux swl
				end
				*)
			
			fun addWorkList u =
				if not (Set.member (precolored, u)) andalso not(moveRelated u) andalso degree u < K then
					(Set.delete(freezeWorklist, u) handle e => (); Set.add(simplifyWorklist, u)) else ()
			
			fun ok (t,r) =
				degree t < K orelse Set.member(precolored, t) orelse Set.member(adjSet, (t,r))
			
			fun conservative nodes =
				let
					fun f (n, k) = if degree n >= K then k+1 else k
				in
					List.foldl f 0 nodes < K
				end
			
			fun getAlias n =
				if Set.member (coalescedNodes, n) then
					getAlias (valOf(tabSearch alias n))  handle e => raise Fail "230"
				else n
			
			fun combine (u, v) = (
				if Set.member (freezeWorklist, v) then 
					Set.delete(freezeWorklist, v)
				else 
					Set.delete(spillWorklist, v) handle e => ();
				
				Set.add(coalescedNodes, v);
				tabRInsert alias (v,u);
				tabRInsert moveList 
									 (u, Set.union (valOf (tabSearch moveList u)  handle e => raise Fail "242", 
															 valOf (tabSearch moveList v)))  handle e => raise Fail "243";
				Set.app (fn t => (addEdge(t,u); decrementDegree t)) (adjacent v);
				if degree u >= K andalso Set.member(freezeWorklist, u) then
					(Set.delete(freezeWorklist, u) handle e => raise Fail "160"; Set.add(simplifyWorklist, u)) else ()
			)
			
			fun Coalesce () =
				let
					val (m as (x,y)) = Set.getElement worklistMoves
					
					(*DEBUG*)(*
					val _ = print ("m = " ^ pp_pair m ^ "\n")
					*)(*DEBUG*)
					
					val x' = getAlias x
					val y' = getAlias y
					val (u,v) = if Set.member(precolored, y) then (y',x') else (x',y')
					(*DEBUG*)(*
					val _ = print ("(u,v) = " ^ pp_pair (u,v) ^ "\n")
					*)(*DEBUG*)
					fun chkOK nodes = Set.foldr (fn (t,b) => b andalso ok(t,u)) true nodes
				in
					Set.delete(worklistMoves, m);
					
					if Graph.eq(u,v) then
						(Set.add(coalescedMoves, m); addWorkList u)
						
					else if Set.member(precolored, v) orelse Set.member(adjSet, (u,v)) then
						(Set.add(constrainedMoves, m); addWorkList u; addWorkList v)
						
					else if Set.member(precolored, u) andalso chkOK (adjacent v) 
						orelse not(Set.member(precolored, u)) 
							andalso conservative(Set.listItems (Set.union(adjacent u, adjacent v))) 
						then
							(Set.add(coalescedMoves, m); combine(u,v); addWorkList u)
						else
							Set.add(activeMoves, m)
				end
				
			fun freezeMoves u =
				let
					fun aux (m as (x,y)) =
						let
							val v = if Graph.eq(getAlias y, getAlias u) 
								then getAlias x else getAlias y
						in
							Set.delete(activeMoves, m) handle e => raise Fail "197";
							Set.add(frozenMoves, m);
							if Set.isEmpty(nodeMoves v) andalso degree v < K then
								(Set.delete(freezeWorklist,v) handle e => raise Fail "200";
								 Set.add(simplifyWorklist, v))
							else ()
						end
				in
					Set.app aux (nodeMoves u)
				end
				
			fun Freeze () =
				let
					val u = Set.getElement freezeWorklist
				in
					Set.delete(freezeWorklist, u) handle e => raise Fail "119"; 
					Set.add(simplifyWorklist, u); 
					freezeMoves u
				end
			
			fun selectNode () = 
				let
					fun max(n,(ln, max_val)) = 
						if spillCost n > max_val then 
							([n],spillCost n) 
						else 
							(ln, max_val)
				in
					Set.foldr max ([], 0) spillWorklist
				end
			
			fun SelectSpill () = 
				let
					val (m,_) = selectNode ()
				in
					(*DEBUG*)(*
					print ("Select: "^TigerTemp.tempname (gtemp (hd m))^"\n");
					*)(*DEBUG*)
					Set.delete(spillWorklist, hd m) handle e => raise Fail "235";
					Set.add(simplifyWorklist, hd m);
					freezeMoves (hd m)
				end
			
			fun AssignColors () = 
				let
					val stack = Stack.toList selectStack
					
					fun colorifyNodes [] = ()
					 |	colorifyNodes (n::stack') =
					 	let
					 		val okColors = Set.empty(TigerTemp.comptemps)
					 		val _ = Set.addList(okColors, registers)
					 		val _ = Set.delete(okColors, TigerFrame.SP) handle e => ()
					 		
					 		fun filterColor w =
					 			if Set.member(Set.union(coloredNodes, precolored), getAlias w) then
					 				let
					 					val a = getAlias w
					 					val col = valOf(tabSearch color (getAlias w))  handle e => raise Fail "344"
					 				in
					 					Set.delete(okColors, valOf(tabSearch color (getAlias w))) handle e => ()
					 				end else ()
					 	in
					 		Set.app filterColor (valOf(tabSearch adjList n))  handle e => raise Fail "354";
					 		(*DEBUG*)(*
		 					print ("nodo: " ^ TigerTemp.tempname (gtemp n) ^ "\n");
		 					print ("alias: " ^ TigerTemp.tempname (gtemp (getAlias n)) ^ "\n");
		 					print "adyacentes: "; Set.app (print o TigerTemp.tempname o gtemp) (valOf(tabSearch adjList n));
		 					print "\nokColors: "; Set.print TigerTemp.tempname okColors; print "\n";
		 					*)(*DEBUG*)
					 		if Set.isEmpty(okColors) then
					 			Set.add(spilledNodes, n)
					 		else (
					 			Set.add(coloredNodes, n);
					 			tabRInsert color (n, Set.getElement okColors); 
					 			()
					 		);
					 		colorifyNodes stack'
					 	end
					 
					fun colorifyAlias n =
						(*DEBUG*)(*
						(print (TigerTemp.tempname (gtemp n) ^ "->" ^ TigerTemp.tempname (gtemp (getAlias n)) ^ "\n");
						*)(*DEBUG*)
							(tabRInsert color (n, valOf(tabSearch color (getAlias n)));())  handle e => raise Fail "365"
				in
					(*DEBUG*)(*
						List.app (print o TigerTemp.tempname o gtemp) stack; print "\n";
					*)(*DEBUG*)
					colorifyNodes stack;
					Set.app colorifyAlias coalescedNodes
				end
			
			fun repeat () = (
				(*DEBUG*)(*
				print "--- repeat ---\n";
				print "simplifyWorklist: "; Set.print (TigerTemp.tempname o gtemp) simplifyWorklist; print "\n";
				print "worklistMoves: "; 
					Set.print pp_pair worklistMoves; print "\n";
				print "freezeWorklist: "; Set.print (TigerTemp.tempname o gtemp) freezeWorklist; print "\n";
				print "spillWorklist: "; Set.print (TigerTemp.tempname o gtemp) spillWorklist; print "\n";
				*)(*DEBUG*)
				if not (Set.isEmpty(simplifyWorklist)) then Simplify ()
				else if not (Set.isEmpty(worklistMoves)) then Coalesce ()
				else if not (Set.isEmpty(freezeWorklist)) then Freeze ()
				else if not (Set.isEmpty(spillWorklist)) then SelectSpill () 
				else ();
				
				if Set.isEmpty(simplifyWorklist) andalso Set.isEmpty(worklistMoves)
					andalso Set.isEmpty(freezeWorklist) andalso Set.isEmpty(spillWorklist) 
				then ()
				else repeat ())
		in
			Set.addList(worklistMoves, moves);
			Set.app makeWorklist init;
			repeat ();
			AssignColors ();
			List.app (fn (n,t) => (tabRInsert initial (gtemp n, t);())) (tabAList color);
			(*DEBUG*)(*
			List.app (fn (t,x) => print (TigerTemp.tempname t ^ " -> " ^ TigerTemp.tempname x ^ "\n")) (tabAList initial);
			*)
			(initial, List.map gtemp (Set.listItems spilledNodes))
		end
end