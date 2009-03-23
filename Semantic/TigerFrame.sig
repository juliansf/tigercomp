signature TigerFrame =
sig
	type register
	type frame
	type access
	
	datatype frag = PROC of { body : TigerTree.stm, frame : frame }
								| STRING of TigerTemp.label * string
	
	val VarAreaOffset : TigerTemp.temp (* Placeholder para el calculo del offset del Storage Area *)
	val FP : TigerTemp.temp
	val ZERO : TigerTemp.temp
	val SP : TigerTemp.temp
	val RV : TigerTemp.temp
	val LR : TigerTemp.temp
	
	val specialregs : TigerTemp.temp list
	val argregs : TigerTemp.temp list
	val calleesaves : TigerTemp.temp list
	val callersaves : TigerTemp.temp list
	val calldefs : TigerTemp.temp list
	val registers : TigerTemp.temp list

	val wordSize : int
	val prologSize : int
	val linkAreaSize : int
	val LRSaveOffset : int
	val externalCall : string * TigerTree.exp list -> TigerTree.exp
	val newFrame : (TigerTemp.label * bool list) -> frame
	val getFormals : frame -> access list
	val getFrameLabel : frame -> TigerTemp.label
	val isLeaf : frame -> bool
	val getMaxCallArgs : frame -> int
	val getLocalOffset : frame -> int
	val setMaxCallArgs : frame -> int -> unit
	
	val varAreaOffset : TigerTemp.label -> int
	
	val allocLocal : frame -> bool -> access
	val string : TigerTemp.label -> string -> string
	val sl_access : int -> TigerTree.exp
	val var_access : access * int * frame -> TigerTree.exp
	
	val procEntryExit1 : TigerTree.stm * frame -> TigerTree.stm
	val procEntryExit2 : frame * TigerAssem.instr list -> TigerAssem.instr list
(*	val procEntryExit3 : frame * TigerAssem.instr list -> 
												{ prolog : string, body : TigerAssem.instr list, epiloge : string }*)
end