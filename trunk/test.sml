open BasicIO Nonstdio
open TigerAssem
open TigerTemp

fun main() =
let
	val a = namedtemp("a")
	val b = namedtemp("b")
	val c = namedtemp("c")
	val l1 = namedlabel("L1")
	val l2 = namedlabel("L2")
	
	val li = [
		(* n0 *) OPER {assem="a := 0", src=[], dst=[a], jump=NONE},
		(* n1 *) LABEL {assem="l1: ", lab=l1}, 
		(* n2 *) OPER {assem="b := a+1", src=[a], dst=[b], jump=NONE},
		(* n3 *) OPER {assem="c := c+b", src=[c,b], dst=[c], jump=NONE},
		(* n4 *) OPER {assem="a := b*2", src=[b], dst=[a], jump=NONE},
		(* n5 *) OPER {assem="a<N", src=[a], dst=[], jump=SOME[l1,l2]},
		(* n6 *) LABEL {assem="l2: ", lab=l2},
		(* n7 *) OPER {assem="return c", src=[c], dst=[], jump=SOME []}
	]
	
	val (fgraph, lnodes) = TigerMakeGraph.instrs2graph li
	val (igraph, fn2tmps) = TigerLiveness.interferenceGraph fgraph
in
	TigerLiveness.show igraph
end

val _ = main()