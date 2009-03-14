structure TigerPPCGen :> TigerPPCGen =
struct
	open TigerAssem
	open TigerFrame
	open TigerTree
	open TigerTemp
	
	fun codegen frame stm = [LABEL (newlabel())]
end