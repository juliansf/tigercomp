structure TigerAssem :> TigerAssem =
struct
	type reg = string
	type temp = TigerTemp.temp
	type label = TigerTemp.label
	
	datatype instr = OPER of {assem: string,
														dst: temp list,
														src: temp list,
														jump: label list option}
								 | LABEL of {assem: string,
								 						 lab: label}
								 | MOVE of {assem: string,
								            dst: temp,
								            src: temp}
	
	fun format f instr = case instr of OPER desc => #assem(desc) | LABEL desc => #assem(desc) | MOVE desc => #assem(desc)
end