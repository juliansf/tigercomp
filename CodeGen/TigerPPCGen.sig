signature TigerPPCGen =
sig
	val codegen : TigerFrame.frame -> TigerTree.stm -> TigerAssem.instr list
end