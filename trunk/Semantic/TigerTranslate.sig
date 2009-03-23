signature TigerTranslate =
sig

	(*Manejo de frames y levels*)
	type level
	type access
	
	val getResult : unit -> TigerFrame.frag list
	val addProc : TigerTree.stm * TigerFrame.frame -> unit
	val addString : string -> TigerTemp.label
	
	val outermost : level
	val newLevel : level * string * bool list -> level
	val getLevelLabel : level -> TigerTemp.label
	
	val formals : level -> access list
	val allocLocal : level -> bool -> access
	
	val preWhileFor : level -> unit
	val posWhileFor : level -> unit
	
	(* Traduccion a codigo intermedio *)
	type exp
	
	val procEntryExit : level * exp -> unit

	val unEx : exp -> TigerTree.exp
	val unNx : exp -> TigerTree.stm
	val unCx : exp -> TigerTemp.label * TigerTemp.label -> TigerTree.stm
	
	val seq : TigerTree.stm list -> TigerTree.stm
	(*Una funci'on por cada constructor de datos del AST*)
	val unitExp : unit -> exp
	val nilExp : unit -> exp
	val intExp : int -> exp
	val stringExp : string -> exp
	val callExp : TigerTemp.label * exp list * level * level * bool -> exp
	val opExp : TigerAbs.oper * exp * exp -> exp
	val recordExp : exp list * level -> exp
	val seqExp : exp list * exp * bool -> exp
	val assignExp : exp * exp -> exp
	val ifExp : exp * exp * exp * TigerAbs.ifop * bool -> exp
	val whileExp : exp * exp * level -> exp
	val forExp : access * exp * exp * exp * level -> exp
	val letExp : exp list * exp * bool -> exp
	val breakExp: level -> exp
	val arrayExp: exp * exp * level -> exp
	
	val simpleVar : access * level -> exp
	val fieldVar : exp * int * level -> exp
	val subscriptVar : exp * exp * level -> exp
	
	val varDec : access * exp -> exp
end