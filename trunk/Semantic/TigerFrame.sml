structure TigerFrame :> TigerFrame =
struct
	open TigerTemp
	open tigertab
	open TigerTree
	type register = string
	datatype access = InFrame of int | InReg of TigerTemp.temp
	type frame = {localOffset: int ref, formals: access list, label: TigerTemp.label}
	
	datatype frag = PROC of { body : TigerTree.stm, frame : frame }
							| STRING of TigerTemp.label * string
	
	(* Crea la lista ["sm","s(m+1)",...,"sn"] *)
	fun mkRegs s m n = 
		let 
			fun f i j rl = 
				if i <= j 
				then s^makestring(i) :: f (i+1) j rl 
				else rl 
		in f m n [] end
	
	val ZERO = namedtemp("r0")
	val RV = namedtemp("r3")
	val SP = namedtemp("r1")
	val params = mkRegs 3 10
	val volatile = ["r0"] @ params @ ["r11", "r12"]
	val nonvolatile = mkRegs 14 31
	val dedicated = ["r1", "r2", "r13"]
	val registers = volatile @ nonvolatile
	val tempMap = tabNueva()
	val wordSize = 8
	val prologSize = wordSize * 1
	val incrLocal = ~wordSize
	fun externalCall (name, params) = CALL (NAME (namedlabel(name)), params)

	fun newFrame (name, formals) =
		let
			val argsoffset = ref 0
			fun processFormals (arg, argList) = 
				case arg of true => (argsoffset := !argsoffset - incrLocal;
														 InFrame(!argsoffset) :: argList) 
										|false => InReg (newtemp()) :: argList
		in																																								
			{localOffset = ref 0, formals = List.foldl processFormals [] (true::formals), label = name}		(*true::formals es para dejar como 1er arg al static link*)
		end
			
	fun formals {localOffset, formals, label} = formals
	
	fun name {localOffset, formals, label} = label
	
	fun allocLocal {localOffset, formals, label} escapes =
		if escapes
		then (localOffset := !localOffset + incrLocal; InFrame(!localOffset) )
		else InReg (newtemp())
									 
	fun string label s = labelname(label) ^ ": .ascii \"" ^ s ^ "\"\n"
	
	fun sl_access ~1 = TEMP FP
		| sl_access 0 = MEM (BINOP (PLUS, CONST prologSize, TEMP FP))
		| sl_access n = MEM (BINOP (PLUS, CONST prologSize, sl_access (n-1)))
	
	fun var_access (InReg t, _) = TEMP t
		| var_access (InFrame off, n) =
				let 
					fun aux 0 = TEMP FP
					  | aux n = MEM (BINOP (PLUS, CONST prologSize, aux (n-1)))
				in MEM (BINOP (PLUS, CONST off, aux n)) end
	
	fun getFrameLabel (frame:frame) = #label(frame)
	
	fun procEntryExit1 (body, frame) = body
(*	val procEntryExit2 : frame * TigerAssem.instr list -> TigerAssem.inst list
	val procEntryExit3 : frame * TigerAssem.instr list -> 
												{ prolog : string, body : TigerAssem.instr list, epiloge : string }*)
end