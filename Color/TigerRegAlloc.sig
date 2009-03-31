signature TigerRegAlloc =
sig
	type allocation = (TigerTemp.temp, TigerTemp.temp) tigertab.Tabla
	
	val alloc : TigerAssem.instr list * TigerFrame.frame -> 
								TigerAssem.instr list * allocation
end