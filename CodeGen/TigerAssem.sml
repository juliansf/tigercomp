structure TigerAssem :> TigerAssem =
struct
	open Regex
	open TigerTemp
	
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
	
	fun format tmap instr = 
		let 
			fun f lst n = tmap (List.nth(lst, valOf(Int.fromString n)))
		in
			case instr of 
				OPER {assem, dst, src, jump} =>
					"\t" ^ (
					replace (regcomp "`d([0-9])+?" [Extended]) [Tr (f dst, 1)] 
						(replace (regcomp "`s([0-9])+?" [Extended]) [Tr (f src, 1)] assem))
			| LABEL desc => #assem(desc)
			| MOVE {assem, dst, src} =>
					"\t" ^ (
					replace (regcomp "`d([0-9])+?" [Extended]) [Tr (f [dst], 1)]
						(replace (regcomp "`s([0-9])+?" [Extended]) [Tr (f [src], 1)] assem))
		end
end