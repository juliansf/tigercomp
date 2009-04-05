structure TigerFrame :> TigerFrame =
struct
	open TigerTemp
	open tigertab
	open TigerTree
	
	structure A = TigerAssem
	
	type register = temp
	datatype access = InFrame of int * varkind | InReg of TigerTemp.temp
	type frame = {localOffset: int ref, 
								formals: access list, 
								label: TigerTemp.label, 
								leaf: bool ref, 
								maxArgs: int ref}
	
	datatype frag = PROC of { body : TigerTree.stm, frame : frame }
							| STRING of TigerTemp.label * string
							
	val framesTable : (label, frame ref) Tabla = tabNueva ()
		
	(* Usamos este temp como un placeholder para reemplazarlo por 
		 el offset correspondiente en TigerAssem.format *)
	val VarAreaOffset = namedtemp("VarAreaOffset") 
	
	val FP = namedtemp("fp") (* No tenemos frame pointer, se reemplaza en CodeGen por SP+fs *)
	
	val ZERO = namedtemp("r0") (* Language Specific *)
	
	val SP = namedtemp("r1") (* Stack Pointer *)
	val R2 = namedtemp("r2") (* Read-only small data area anchor *)
	val RV = namedtemp("r3") (* Parameter Passing / Return Value *)
	
	val R4 = namedtemp("r4") (* Parameter Passing Regs *)
	val R5 = namedtemp("r5")
	val R6 = namedtemp("r6")
	val R7 = namedtemp("r7")
	val R8 = namedtemp("r8")
	val R9 = namedtemp("r9")
	val R10 = namedtemp("r10")
	
	val R11 = namedtemp("r11")
	val R12 = namedtemp("r12")
	
	val R13 = namedtemp("r13") (* Read-write small data area anchor *)
	
	val R14 = namedtemp("r14") (* Non-volatile registers *)
	val R15 = namedtemp("r15")
	val R16 = namedtemp("r16")
	val R17 = namedtemp("r17")
	val R18 = namedtemp("r18")
	val R19 = namedtemp("r19")
	val R20 = namedtemp("r20")
	val R21 = namedtemp("r21")
	val R22 = namedtemp("r22")
	val R23 = namedtemp("r23")
	val R24 = namedtemp("r24")
	val R25 = namedtemp("r25")
	val R26 = namedtemp("r26")
	val R27 = namedtemp("r27")
	val R28 = namedtemp("r28")
	val R29 = namedtemp("r29")
	val R30 = namedtemp("r30")
	val R31 = namedtemp("r31")
	
	val CR = namedtemp("cr") (* Condition Register *)
	val LR = namedtemp("lr") (* A este registro solo se accede mediante instrucciones especiales *)
	
	val specialregs = [R2, R13, CR]
	val argregs = [R4, R5, R6, R7, R8, R9, R10]
	val calleesaves = [R31, R30, R29, R28, R27, R26, R25, R24, R23, 
									 R22, R21, R20, R19, R18, R17, R16, R15, R14]
	val callersaves = [ZERO, R11, R12]
	
	val calldefs = callersaves @ [RV] @ argregs
	val registers = [SP] @ calldefs @ calleesaves
	
	val tempMap = tabNueva();
	val tempMap = 
		let
			val t = tabNueva()
		in
			List.app (fn r => (tabRInsert t (r,r);())) specialregs;
			t
		end
	
	val wordSize = 4
	val quadWord = wordSize * 4
	val prologSize = wordSize * 1
	val linkAreaSize = 6 * wordSize (* 6 words *)
	val LRSaveOffset = 2 * wordSize
	val SLSaveOffset = 3 * wordSize
	val incrLocal = wordSize
	val GPRSaveAreaSize = wordSize * 18
	val argRegsLength = List.length (RV::argregs)
	fun externalCall (name, params) = CALL (NAME (namedlabel(name)), params)
	
	val calleeOffsets =
		let
			val t = tabNueva();
			fun f (r, off) = (tabRInsert t (r,off); off-wordSize)
		in 
			List.foldl f (~wordSize) calleesaves; 
			t
		end
	val (calleeTemps, calleeTempsList) = 
		let 
			val tab = tabNueva();
			fun f r = let val t = newtemp() in tabRInsert tab (t,r); (t,r) end
			val lst = List.map f calleesaves
		in
			(tab, lst)
		end

	fun newFrame (name, formals) =
		let
			(* Los primeros 8 argumentos van en Registros *)
			fun procFormals _ _ [] = []
			 |	procFormals offset [] (a::args) = 
			 			InFrame(offset, FORMAL) :: procFormals (offset+wordSize) [] args
			 |	procFormals offset (r::aregs) (a::args) = 
			 			(case a of
			 					true => InFrame(offset, FORMAL)
			 				|	false => InReg(newtemp())) :: procFormals (offset+wordSize) aregs args
			
			val frame = {localOffset = ref 0, 
									 formals = InFrame (SLSaveOffset, RELATIVE) :: procFormals linkAreaSize argregs (formals), 
									 label = name,
									 leaf = ref true,
									 maxArgs = ref 0}
			
		in																												
			(tabInsert framesTable (name, ref frame); frame)
		end
			
	fun getFormals (frame:frame) = List.tl(#formals(frame)) handle Empty => []
	fun getFrameLabel (frame:frame) = #label(frame)
	fun isLeaf (frame:frame) = !(#leaf(frame))
	fun getMaxCallArgs (frame:frame) = !(#maxArgs(frame))
	fun getLocalOffset (frame:frame) = !(#localOffset(frame))
	fun frameSize (frame:frame) =
		let
			val total = linkAreaSize 
								+ wordSize * !(#maxArgs frame) 
								+ !(#localOffset frame) 
								+ GPRSaveAreaSize
								
			val rem = total mod quadWord
			val rest = if rem <> 0 then quadWord - rem else 0
		in
			(*print (labelname (#label frame)^": "^Int.toString(total)^"\n");*)
			total + rest
		end
	
	fun varAreaOffset label vk = 
		let
			val frame = valOf (tabSearch framesTable label)
		in
			case vk of
				FORMAL => 0
			|	LOCAL => wordSize * !(#maxArgs(!frame)) + linkAreaSize
			| RELATIVE => 0
		end
	
	fun varOffsetStr lab vk off =
		let
			val offset = TigerUtils.int (varAreaOffset lab vk + off)
		in
			case vk of
				FORMAL => offset ^ "+" ^ TigerTemp.labelname lab ^ "_framesize"
			| LOCAL => offset
			| RELATIVE => offset
		end
	
	fun getTempOffset (frame:frame) t = 
		case tabSearch calleeTemps t of
			SOME r => valOf(tabSearch calleeOffsets r)
		|	NONE => 
			let 
				val off = !(#localOffset frame ) 
			in  
				#localOffset frame := off+incrLocal; 
				off + varAreaOffset (#label frame) LOCAL
			end
	
	fun setMaxCallArgs (frame:frame) allRegs n =
		let
			val leaf = #leaf(frame)
			val maxArgs = #maxArgs(frame)
			val argCount = if allRegs andalso n < argRegsLength then argRegsLength else n
		in
			leaf := false;
			if argCount > !maxArgs then maxArgs := argCount else ()
		end
	
	fun allocLocal {localOffset, formals, label, leaf, maxArgs} escapes =
		if escapes
		then let val off = !localOffset in localOffset := off+incrLocal; InFrame(off, LOCAL) end
		else InReg (newtemp())
	
	fun sl_access ~1 = TEMP SP
		| sl_access 0 = MEM (BINOP (PLUS, CONST SLSaveOffset, TEMP SP))
		| sl_access n = MEM (BINOP (PLUS, CONST SLSaveOffset, sl_access (n-1)))
	
	fun var_access (InReg t, _, _) = TEMP t
		| var_access (InFrame (off, vk), n, (frame:frame)) =
				let 
					fun aux 0 = TEMP SP
					  | aux n = MEM (BINOP (PLUS, CONST SLSaveOffset, aux (n-1)))
				in MEM (VARACCESS (vk, off, #label(frame), (aux n))) end

	fun procEntryExit1 (body, {localOffset, formals, label, leaf, maxArgs}) = 
		let
			fun copyArg (InReg t, ar) = MOVE (TEMP t, TEMP ar)
			 |	copyArg (InFrame (off,vk), ar) = MOVE (MEM(VARACCESS(vk, off, label, TEMP SP)), TEMP ar)
			
			fun saveReg (t, r) = 
				(MOVE (TEMP t, TEMP r), MOVE (TEMP r, TEMP t))
			
			fun saveLR proc = 
				if !leaf then TigerTree.seq proc
				else 
					let 
						val t = newtemp()
					in 
						TigerTree.seq ([MOVE (TEMP t, TEMP LR) ] @ proc @ [MOVE (TEMP LR, TEMP t)]) 
					end
				
			val argsMoves = List.map copyArg (ListPair.zip (formals, RV::argregs))
			val (entry, exit) = ListPair.unzip (List.map saveReg calleeTempsList)
		in 
			saveLR (entry @ [MOVE (MEM (TEMP SP), TEMP FP)] 
							@ argsMoves @ [body, MOVE (TEMP SP, MEM(TEMP SP))] @ exit) 
		end

	fun procEntryExit2 (frame, body) =
		body @ [A.OPER {assem="blr\n", src=[SP]@calleesaves, dst=[], jump=SOME[]}]
		
	fun procEntryExit3 (frame:frame, linstr) =
		let
			val prolog = "\t.align 2\n"
								 ^ "\t.globl " ^ labelname (#label frame) ^ "\n"
		in
			{prolog=prolog, body=linstr, epilogue=""}
		end
end