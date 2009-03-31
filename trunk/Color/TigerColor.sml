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
			EQUAL => Graph.comp (c,d)
		| ord => ord
	
	fun color {interference: TigerLiveness.igraph, 
						 initial: allocation, 
						 spillCost: Graph.node -> int, 
						 registers: TigerFrame.register list} =
		let
			val TigerLiveness.IGRAPH {graph, tnode, gtemp, moves} = interference
			val K = List.length TigerFrame.registers
			
			val degreeMap = mkDegreeMap graph
			fun degree n =
				case tabSearch degreeMap n of
					SOME i => i
				| NONE => Error (ErrorInternalError "problemas con TigerColor.color.degree!", 0)
			
			fun decrementDegree n = (tabRInsert degreeMap (n, degree n - 1);())
			
			(* Worklists *)
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
			 		setLists nodes
			 	end
				
			val _ = setLists (Graph.nodes graph)
			val _ = Set.addList(precolored, List.map tnode registers)
			val _ = List.app (fn t => (tabRInsert color (tnode t, t);())) registers
			
			fun adjacent n = 
				let
					val adj_n = valOf(tabSearch adjList n)
					val sel_stack = Set.empty(Graph.comp)
					val _ = Set.addList (sel_stack, Stack.toList selectStack)
				in
					Set.difference(adj_n, Set.union (sel_stack, coalescedNodes))
				end
			
			fun nodeMoves n =
				let
					val movel_n = Set.empty(p_comp)
					val _ = Set.app (fn m => Set.add (movel_n, (n,m))) (valOf (tabSearch moveList n))
				in
					Set.intersection(movel_n, Set.union(activeMoves, worklistMoves))
				end
				
			fun moveRelated n = not (Set.isEmpty (nodeMoves n))
			
			(* MakeWorkList *)
			fun makeWorklist n =
				if degree n >= K then
					Set.add(spillWorklist, n)
				else if moveRelated n then
					Set.add(freezeWorklist, n)
				else
					Set.add(simplifyWorklist, n)
			
			fun Simplify () =
				let
					val swl = Set.listItems simplifyWorklist
					
					fun aux n =
						let 
							val adj_n = adjacent n
						in
							Set.delete(simplifyWorklist, n);
							Stack.push selectStack n;
							Set.app decrementDegree adj_n
						end
				in
					List.app aux swl
				end
			
			fun addWorkList u =
				if not (Set.member (precolored, u)) andalso not(moveRelated u) andalso degree u < K then
					(Set.delete(freezeWorklist, u); Set.add(simplifyWorklist, u)) else ()
			
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
					getAlias (valOf(tabSearch alias n))
				else n
			
			fun combine (u, v) = (
				if Set.member (freezeWorklist, v) then 
					Set.delete(freezeWorklist, v)
				else 
					Set.delete(spillWorklist, v);
				
				Set.add(coalescedNodes, v);
				tabRInsert alias (v,u);
				tabRInsert moveList 
									 (u, Set.union (valOf (tabSearch moveList u), 
															 valOf (tabSearch moveList v)));
				Set.app (fn t => Graph.mk_edge{from=t,to=u}) (adjacent v);
				
				if degree u >= K andalso Set.member(freezeWorklist, u) then
					(Set.delete(freezeWorklist, u); Set.add(simplifyWorklist, u)) else ()
			)
			
			fun Coalesce () =
				let
					val wlm = Set.listItems worklistMoves
					
					fun aux (m as (x,y)) =
						let
							val x' = getAlias x
							val y' = getAlias y
							val (u,v) = if Set.member(precolored, y) then (y',x') else (x',y')
							fun chkOK nodes = Set.foldr (fn (t,b) => b andalso ok(t,u)) true nodes
						in
							if Graph.eq(u,v) then
								(Set.add(coalescedMoves, m); addWorkList u)
							else if Set.member(precolored, v) orelse Set.member(adjSet, (u,v)) then
								(Set.add(constrainedMoves, m); addWorkList u; addWorkList v)
							else if Set.member(precolored, v) andalso chkOK (adjacent v) 
								orelse not(Set.member(precolored, u)) 
									andalso conservative(Set.listItems (Set.union(adjacent u, adjacent v))) 
								then
									(Set.add(coalescedMoves, m); combine(u,v); addWorkList u)
								else
									Set.add(activeMoves, m)
						end
				in
					List.app aux wlm
				end
			
			fun freezeMoves u =
				let
					fun aux (m as (x,y)) =
						let
							val v = if Graph.eq(getAlias y, getAlias u) 
								then getAlias x else getAlias y
						in
							Set.delete(activeMoves, m);
							Set.add(frozenMoves, m);
							if Set.isEmpty(nodeMoves v) andalso degree v < K then
								(Set.delete(freezeWorklist,v);
								 Set.add(simplifyWorklist, v))
							else ()
						end
				in
					Set.app aux (nodeMoves u)
				end
				
			fun Freeze () =
				let
					fun aux u = 
						(
							Set.delete(freezeWorklist, u); 
							Set.add(simplifyWorklist, u); 
							freezeMoves u
						)
				in
					Set.app aux freezeWorklist
				end
			
			fun selectNode () = 
				let
					fun min(n,(ln, min_val)) = 
						if spillCost n < min_val then 
							([n],spillCost n) 
						else 
							(ln, min_val)
				in
					Set.foldl min ([], valOf(Int.maxInt)) spillWorklist
				end
			
			fun SelectSpill () = 
				let
					val (m,_) = selectNode ()
				in
					Set.delete(spillWorklist, hd m);
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
					 		
					 		fun filterColor w =
					 			if Set.member(Set.union(coloredNodes, precolored), getAlias w) then
					 				Set.delete(okColors, valOf(tabSearch color (getAlias w))) else ()
					 	in
					 		Set.app filterColor (valOf(tabSearch adjList n));
					 		if Set.isEmpty(okColors) then
					 			Set.add(spilledNodes, n)
					 		else (
					 			Set.add(coloredNodes, n);
					 			tabRInsert color (n, Set.getElement okColors);
					 			colorifyNodes stack'
					 		)
					 	end
					 
					fun colorifyAlias n =
						(tabRInsert color (n, valOf(tabSearch color (getAlias n)));())
				in
					colorifyNodes stack;
					Set.app colorifyAlias coalescedNodes
				end
			
			fun repeat () =
				if Set.isEmpty(simplifyWorklist) then
					if Set.isEmpty(worklistMoves) then
						if Set.isEmpty(freezeWorklist) then
							if Set.isEmpty(spillWorklist) then
								repeat ()
							else SelectSpill ()
						else Freeze ()
					else Coalesce ()
				else Simplify ()
		in
			Set.addList(worklistMoves, moves);
			List.app makeWorklist (Graph.nodes graph);
			repeat ();
			AssignColors ();
			(tabAAplica (gtemp, fn r => r, color), List.map gtemp (Set.listItems spilledNodes))
		end
	
	and mkDegreeMap graph =
		let
			val dg = TigerGraph.newTable ()
		in
			List.app (fn n => (tabRInsert dg (n, List.length (Graph.adj n));())) (Graph.nodes graph);
			dg
		end
end