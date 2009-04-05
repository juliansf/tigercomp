open BasicIO Nonstdio
open TigerAssem
open TigerTemp
open tigertab

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
(*
	val b = namedtemp("b")
	val c = namedtemp("c")
	val d = namedtemp("d")
	val e = namedtemp("e")
	val f = namedtemp("f")
	val g = namedtemp("g")
	val h = namedtemp("h")
	val i = namedtemp("i")
	val j = namedtemp("j")
	val k = namedtemp("k")
	val m = namedtemp("m")
	
	val li = [
		OPER {assem="`d0 := mem[`s0+12]\n", dst=[g], src=[j], jump=NONE},
		OPER {assem="`d0 := `s0 - 1\n", dst=[h], src=[k], jump=NONE},
		OPER {assem="`d0 := `s0 * `s1\n", dst=[f], src=[g,h], jump=NONE},
		OPER {assem="`d0 := mem[`s0+8]\n", dst=[e], src=[j], jump=NONE},
		OPER {assem="`d0 := mem[`s0+16]\n", dst=[m], src=[j], jump=NONE},
		OPER {assem="`d0 := mem[`s0]\n", dst=[b], src=[f], jump=NONE},
		OPER {assem="`d0 := `s0 + 8\n", dst=[c], src=[e], jump=NONE},
		MOVE {assem="`d0 := `s0\n", dst=d, src=c},
		OPER {assem="`d0 := `s0 + 4\n", dst=[k], src=[m], jump=NONE},
		MOVE {assem="`d0 := `s0\n", dst=j, src=b},
		OPER {assem="", dst=[], src=[d,k,j], jump=SOME []}
	]
	*)
	val fr = TigerFrame.newFrame(TigerTemp.namedlabel "_main", [])
	(*val (li, alloc) = TigerRegAlloc.alloc (li, fr)*)
	val li = TigerRegAlloc.rewriteCode (li, fr, [])
	
in
	print ("\nASSEM:\n\n");
	List.app (print o TigerAssem.format (fn t => TigerTemp.tempname t(*valOf(tigertab.tabSearch alloc t)*))) li
end

val _ = main()