open BasicIO Nonstdio
open TigerAssem
open TigerTemp

fun main() =
let
(*
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
*)
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
		OPER {assem="g := mem[j+12]", dst=[g], src=[j], jump=NONE},
		OPER {assem="h := k - 1", dst=[h], src=[k], jump=NONE},
		OPER {assem="f := g * h", dst=[f], src=[g,h], jump=NONE},
		OPER {assem="e := mem[j+8]", dst=[e], src=[j], jump=NONE},
		OPER {assem="m := mem[j+16]", dst=[m], src=[j], jump=NONE},
		OPER {assem="b := mem[f]", dst=[b], src=[f], jump=NONE},
		OPER {assem="c := e + 8", dst=[c], src=[e], jump=NONE},
		MOVE {assem="d := c", dst=d, src=c},
		OPER {assem="k := m + 4", dst=[k], src=[m], jump=NONE},
		MOVE {assem="j := b", dst=j, src=b},
		OPER {assem="", dst=[], src=[d,k,j], jump=SOME []}
	]
	
	val (li, alloc) = TigerRegAlloc.alloc (li, TigerFrame.newFrame(TigerTemp.namedlabel "_main", []))
in
	()
end

val _ = main()