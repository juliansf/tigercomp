signature TigerPPCGen =
sig
	val codegen : TigerFrame.frame -> TigerTree.stm -> TigerAssem.instr list
	val literals : (TigerTemp.label * string) list -> string
	val sections : string * string * string
end