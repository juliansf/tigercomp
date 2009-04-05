structure TigerPPCGen :> TigerPPCGen =
struct
	open TigerAssem
	open TigerFrame
	open TigerError
	open TigerUtils
	
	structure T = TigerTree
	structure A = TigerAssem
	
	fun relOp relop =
		case relop of
			T.EQ 	=>	"eq"	|	T.NE	=>	"ne"	
		|	T.LT	=>	"lt"	|	T.LE	=>	"le"	
		|	T.GT	=>	"gt"	|	T.GE	=>	"ge"	
		|	T.ULT	=>	"lt"	|	T.ULE	=>	"le"
		|	T.UGT	=>	"gt"	|	T.UGE	=>	"ge"
	
	
	fun codegen frame (stm: T.stm) : A.instr list =
		let 
			val ilist = ref (nil: A.instr list)
			fun emit x = ilist := x :: !ilist
			fun result (gen) = let val t = TigerTemp.newtemp() in gen t; t end
			
			fun munchStm (T.SEQ (a,b)) = (munchStm a; munchStm b)
			
			 |	munchStm (T.MOVE(T.TEMP t, T.MEM (T.VARACCESS (vk, off, lab, e)))) = 
			 			emit (A.OPER {
			 				assem="lwz `d0, " ^ TigerFrame.varOffsetStr lab vk off ^ "(`s0)\n", 
			 				dst=[t], src=[munchExp e], jump=NONE})
			 
			 		
			 |	munchStm (T.MOVE(T.TEMP t, T.VARACCESS (vk, off, lab, e))) = 
			 			emit (A.OPER {
			 				assem="addi `d0, `s0, " ^ TigerFrame.varOffsetStr lab vk off ^ "\n", 
			 				dst=[t], src=[munchExp e], jump=NONE})
			
			 |	munchStm (T.MOVE(T.TEMP t, T.MEM(T.BINOP(T.PLUS, T.CONST j, e)))) = 
			 			emit (A.OPER {assem="lwz `d0, " ^ int j ^ "(`s0)\n", 
			 										dst=[t], src=[munchExp e], jump=NONE})
			 			
			 |	munchStm (T.MOVE(T.TEMP t, T.MEM(T.BINOP(T.PLUS, e, T.CONST j)))) = 
			 			emit (A.OPER {assem="lwz `d0, " ^ int j ^ "(`s0)\n", 
			 										dst=[t], src=[munchExp e], jump=NONE})
			 									
			 |	munchStm (T.MOVE(T.TEMP t, T.MEM e)) = 
			 			emit (A.OPER {assem="lwz `d0, 0(`s0)\n", 
			 										dst=[t], src=[munchExp e], jump=NONE})
			 									
			 |	munchStm (T.MOVE(T.TEMP t, T.CONST j)) =
			 			emit (A.OPER {assem="li `d0, " ^ int j ^ "\n",
			 										dst=[t], src=[], jump=NONE})
				
			 |	munchStm (T.MOVE(T.TEMP t1, T.TEMP t2)) =
			 			if LR = t1 then (
			 				emit (A.OPER {assem="lwz `d0, " ^ int TigerFrame.LRSaveOffset ^ "(r1)\n",
			 											dst=[t2], src=[], jump=NONE});
			 				emit (A.OPER {assem="mtlr `s0\n", dst=[], src=[t2], jump=NONE}))
			 			else if LR = t2 then (
			 				emit (A.OPER {assem="mflr `d0\n", dst=[t1], src=[], jump=NONE});
			 				emit (A.OPER {assem="stw `s0, " ^ int TigerFrame.LRSaveOffset ^ "(r1)\n", 
			 											dst=[], src=[t1], jump=NONE})
			 			)
			 			else (
			 				emit (A.MOVE {assem="mr `d0, `s0\n", dst=t1, src=t2}))
			 			
			 |	munchStm (T.MOVE(T.TEMP t, e)) =
			 			emit (A.MOVE {assem="mr `d0, `s0\n",
			 										dst=t, src=munchExp e})
			 
			 |	munchStm (T.MOVE(T.MEM (T.VARACCESS (vk, off, lab, e)), e')) = 
			 			emit (A.OPER {
			 				assem="stw `s1, " ^ TigerFrame.varOffsetStr lab vk off ^ "(`s0)\n", 
			 				dst=[], src=[munchExp e, munchExp e'], jump=NONE})
				 			
			 										
			 |	munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
			 			emit (A.OPER {assem="stw `s1, " ^ int i ^ "(`s0)\n",
			 										dst=[], src=[munchExp e1, munchExp e2], jump=NONE})
			 										
			 |	munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) = 
			 			emit (A.OPER {assem="stw `s1, " ^ int i ^ "(`s0)\n",
			 										dst=[], src=[munchExp e1, munchExp e2], jump=NONE})
			 									
			 |	munchStm (T.MOVE(T.MEM(T.CONST i), e)) =
			 			emit (A.OPER {assem="stw `s0, " ^ int i ^ "(r0)\n",
			 										dst=[], src=[munchExp e], jump=NONE})
			 										
			 |	munchStm ( T.MOVE (T.MEM (T.TEMP t1), T.TEMP t2)) = 
			 			if SP = t1 andalso FP = t2 
			 			then
			 				emit (A.OPER {assem="stwu r1, -" 
			 												^ TigerTemp.labelname (getFrameLabel frame) ^ "_framesize(r1)\n",
			 											dst=[], src=[], jump=NONE})
			 			else
			 				emit (A.OPER {assem="stw `s1, 0(`s0)\n",
			 											dst=[], src=[t1, t2], jump=NONE})
			 
			 |	munchStm (T.MOVE(T.MEM e1, e2)) = 
			 			emit (A.OPER {assem="stw `s1, 0(`s0)\n",
			 										dst=[], src=[munchExp e1, munchExp e2], jump=NONE})
			 										
			 |	munchStm (T.JUMP(T.NAME l, labels)) =
			 			emit (A.OPER {assem="b " ^ TigerTemp.labelname l ^ "\n",
			 										dst=[], src=[], jump=SOME labels})
			 			
			 |	munchStm (T.JUMP(e, labels)) =
			 			emit (A.OPER {assem="b `s0\n",
			 										dst=[], src=[munchExp e], jump=SOME labels})
			 										
			 |	munchStm (T.EXP(T.CALL(T.NAME l, args))) = (
			 			emit (A.OPER {assem="bl " ^ TigerTemp.labelname l ^ "\n",
			 										dst=TigerFrame.calldefs,
			 										src=munchArgs args,
			 										jump=NONE}))
			 
			 |	munchStm(T.EXP(e)) =
			 			emit (A.MOVE {assem="mr `d0, `s0\n",
			 										dst=TigerTemp.newtemp(), src=munchExp e})
			 
			 |	munchStm (T.CJUMP(relop, e, T.CONST i, lv, lf)) = 
			 			(
			 				emit (A.OPER {assem="cmpwi cr0, `s0, " ^ int i ^ "\n",
			 											dst=[], 
			 											src=[munchExp e], 
			 											jump=NONE});
			 				emit (A.OPER {assem= "b" ^ relOp relop ^ " " ^ TigerTemp.labelname lv ^ "\n",
			 											dst=[], src=[], jump=SOME [lv, lf]})
			 			)
			 			
			 |	munchStm (T.CJUMP(relop, e1, e2, lv, lf)) = 
			 			(
			 				emit (A.OPER {assem="cmpw cr0, `s0, `s1\n",
			 											dst=[], 
			 											src=[munchExp e1, munchExp e2], 
			 											jump=NONE});
			 				emit (A.OPER {assem= "b" ^ relOp relop ^ " " ^ TigerTemp.labelname lv ^ "\n",
			 											dst=[], src=[], jump=SOME [lv, lf]})
			 			)
			 			
			 |	munchStm (T.LABEL l) =
			 			emit (A.LABEL {assem=TigerTemp.labelname l ^ ":\n", lab=l})
			 
			 | munchStm _ = Error (ErrorInternalError "TigerPPCGen.munchStm: pattern matching incompleto!", 0)
			
			
			and munchExp (T.CONST i) = 
						result (fn r => 
							emit(A.OPER {assem="li `d0, " ^ int i ^ "\n",
													 dst=[r], src=[], jump=NONE}))
			
			 |	munchExp (T.NAME l) = 
			 			result (fn r => (
			 				emit(A.OPER {assem="addis `d0, 0, hi16(" ^ TigerTemp.labelname l ^ ")\n", 
			 										 dst=[r], src=[], jump=NONE});
			 				emit(A.OPER {assem="ori `d0, `s0, lo16(" ^ TigerTemp.labelname l ^ ")\n",
			 										 dst=[r], src=[r], jump=NONE})))
			 					
			 			
			 |	munchExp (T.TEMP t) = t
			 
			 |	munchExp (T.MEM(T.BINOP(T.PLUS, T.CONST i, e))) = 
			 			result (fn r => 
			 				emit(A.OPER {assem="lwz `d0, " ^ int i ^ "(`s0)\n",
			 										 dst=[r], src=[munchExp e], jump=NONE}))
			 
			 |	munchExp (T.MEM(T.BINOP(T.PLUS, e, T.CONST i))) = 
			 			result (fn r => 
			 				emit(A.OPER {assem="lwz `d0, " ^ int i ^ "(`s0)\n",
			 										 dst=[r], src=[munchExp e], jump=NONE}))
			 																	 
			 |	munchExp (T.MEM(T.CONST i)) = 
			 			result (fn r => 
			 				emit(A.OPER {assem="lwz `d0, " ^ int i ^ "(r0)\n",
			 										 dst=[r], src=[], jump=NONE}))
			 
			 | 	munchExp (T.MEM (T.VARACCESS (vk, off, lab, e))) =
						result (fn r =>
							emit(A.OPER {
			 					assem="lwz `d0, " ^ TigerFrame.varOffsetStr lab vk off ^ "(`s0)\n", 
			 					dst=[r], src=[munchExp e], jump=NONE}))
			 			
			 |	munchExp (T.MEM e) = 
			 			result (fn r => 
			 				emit(A.OPER {assem="lwz `d0, 0(`s0)\n",
			 										 dst=[r], src=[munchExp e], jump=NONE}))
			 
			 (*
			 |	munchExp (T.CALL(T.NAME l, args)) = 
			 			result (fn r => (
			 				emit(A.OPER {assem="bl " ^ TigerTemp.labelname l ^ "\n",
			 										 dst=TigerFrame.calldefs,
			 										 src= munchArgs args,
			 										 jump=NONE})))
			 *)
			 
			 |	munchExp (T.BINOP(T.PLUS, T.CONST 0, e)) = munchExp e
			 			
			 |	munchExp (T.BINOP(T.PLUS, e, T.CONST 0)) = munchExp e
			 			
			 |	munchExp (T.BINOP(T.PLUS, T.CONST i, e)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="addi `d0, `s0, " ^ int i ^ "\n", 
			 											dst=[r], 
			 											src=[munchExp e], 
			 											jump=NONE}))
			 			
			 |	munchExp (T.BINOP(T.PLUS, e, T.CONST i)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="addi `d0, `s0, " ^ int i ^ "\n", 
			 											dst=[r], 
			 											src=[munchExp e], 
			 											jump=NONE}))
			 			
			 |	munchExp (T.BINOP(T.PLUS, e1, e2)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="add `d0, `s0, `s1\n", 
			 											dst=[r], 
			 											src=[munchExp e1, munchExp e2], 
			 											jump=NONE}))
			 
			 |	munchExp (T.BINOP(T.MINUS, T.CONST 0, e)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="neg `d0, `s0\n", 
			 											dst=[r], 
			 											src=[munchExp e], 
			 											jump=NONE}))
			 
			 |	munchExp (T.BINOP(T.MINUS, e, T.CONST 0)) = munchExp e
			 
			 |	munchExp (T.BINOP(T.MINUS, e, T.CONST i)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="addi `d0, `s0, -" ^ int i ^ "\n", 
			 											dst=[r], 
			 											src=[munchExp e], 
			 											jump=NONE}))
			 
			 |	munchExp (T.BINOP(T.MINUS, T.CONST i, e)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="subfic `d0, `s0, " ^ int i ^ "\n", 
			 											dst=[r], 
			 											src=[munchExp e], 
			 											jump=NONE}))
			 
			 |	munchExp (T.BINOP(T.MINUS, e1, e2)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="subf `d0, `s1, `s0\n", 
			 											dst=[r], 
			 											src=[munchExp e1, munchExp e2], 
			 											jump=NONE}))
			 
			 
			 |	munchExp (T.BINOP(T.MUL, T.CONST (~1), e)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="neg `d0, `s0\n", 
			 											dst=[r], 
			 											src=[munchExp e], 
			 											jump=NONE}))
			  
			 |	munchExp (T.BINOP(T.MUL, e, T.CONST (~1))) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="neg `d0, `s0\n", 
			 											dst=[r], 
			 											src=[munchExp e], 
			 											jump=NONE}))
			 
			 |	munchExp (T.BINOP(T.MUL, T.CONST 0, e)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="li `d0, 0\n", 
			 											dst=[r], 
			 											src=[], 
			 											jump=NONE}))
			 
			 |	munchExp (T.BINOP(T.MUL, e, T.CONST 0)) =
			 			result (fn r => 
			 				emit (A.OPER {assem="li `d0, 0\n", 
			 											dst=[r], 
			 											src=[], 
			 											jump=NONE}))
			 
			 |	munchExp (T.BINOP(T.MUL, T.CONST 1, e)) = 
			 			result (fn r => 
			 				emit (A.MOVE {assem="mr `d0, `s0\n", 
			 											dst=r, 
			 											src=munchExp e}))
			 
			 |	munchExp (T.BINOP(T.MUL, e, T.CONST 1)) = 
			 			result (fn r => 
			 				emit (A.MOVE {assem="mr `d0, `s0\n", 
			 											dst=r, 
			 											src=munchExp e}))
			 
			 |	munchExp (T.BINOP(T.MUL, T.CONST i, e)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="mulli `d0, `s0, " ^ int i ^ "\n", 
			 											dst=[r], 
			 											src=[munchExp e], 
			 											jump=NONE}))
			 
			 |	munchExp (T.BINOP(T.MUL, e, T.CONST i)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="mulli `d0, `s0, " ^ int i ^ "\n", 
			 											dst=[r], 
			 											src=[munchExp e], 
			 											jump=NONE}))
			 											
			 |	munchExp (T.BINOP(T.MUL, e1, e2)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="mullw `d0, `s0, `s1\n", 
			 											dst=[r], 
			 											src=[munchExp e1, munchExp e2], 
			 											jump=NONE}))
			 
			 
			 |	munchExp (T.BINOP(T.DIV, e1, e2)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="divw `d0, `s0, `s1\n", 
			 											dst=[r], 
			 											src=[munchExp e1, munchExp e2], 
			 											jump=NONE}))
			 
			 
			 |	munchExp (T.BINOP(T.AND, T.CONST i, e)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="andi. `d0, `s0, " ^ int i ^ "\n", 
			 											dst=[r], 
			 											src=[munchExp e], 
			 											jump=NONE}))
			 
			 |	munchExp (T.BINOP(T.AND, e, T.CONST i)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="andi. `d0, `s0, " ^ int i ^ "\n", 
			 											dst=[r], 
			 											src=[munchExp e], 
			 											jump=NONE}))
			 
			 |	munchExp (T.BINOP(T.AND, e1, e2)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="and `d0, `s0, `s1\n", 
			 											dst=[r], 
			 											src=[munchExp e1, munchExp e2], 
			 											jump=NONE}))
			 
			 	
			 |	munchExp (T.BINOP(T.OR, T.CONST i, e)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="ori. `d0, `s0, " ^ int i ^ "\n", 
			 											dst=[r], 
			 											src=[munchExp e], 
			 											jump=NONE}))
			 
			 |	munchExp (T.BINOP(T.OR, e, T.CONST i)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="ori. `d0, `s0, " ^ int i ^ "\n", 
			 											dst=[r], 
			 											src=[munchExp e], 
			 											jump=NONE}))
			 
			 |	munchExp (T.BINOP(T.OR, e1, e2)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="or `d0, `s0, `s1\n", 
			 											dst=[r], 
			 											src=[munchExp e1, munchExp e2], 
			 											jump=NONE}))
			 
			 
			 |	munchExp (T.BINOP(T.XOR, T.CONST i, e)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="xori. `d0, `s0, " ^ int i ^ "\n", 
			 											dst=[r], 
			 											src=[munchExp e], 
			 											jump=NONE}))
			 
			 |	munchExp (T.BINOP(T.XOR, e, T.CONST i)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="xori. `d0, `s0, " ^ int i ^ "\n", 
			 											dst=[r], 
			 											src=[munchExp e], 
			 											jump=NONE}))
			 
			 |	munchExp (T.BINOP(T.XOR, e1, e2)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="xor `d0, `s0, `s1\n", 
			 											dst=[r], 
			 											src=[munchExp e1, munchExp e2], 
			 											jump=NONE}))
			 
			 
			 |	munchExp (T.BINOP(T.LSHIFT, e1, e2)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="slw `d0, `s0, `s1\n", 
			 											dst=[r], 
			 											src=[munchExp e1, munchExp e2], 
			 											jump=NONE}))
			 
			 
			 |	munchExp (T.BINOP(T.RSHIFT, e1, e2)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="srw `d0, `s0, `s1\n", 
			 											dst=[r], 
			 											src=[munchExp e1, munchExp e2], 
			 											jump=NONE}))
			 

			 |	munchExp (T.BINOP(T.ARSHIFT, e, T.CONST i)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="srawi `d0, `s0, " ^ int i ^ "\n", 
			 											dst=[r], 
			 											src=[munchExp e], 
			 											jump=NONE}))
			 											
			 |	munchExp (T.BINOP(T.ARSHIFT, e1, e2)) = 
			 			result (fn r => 
			 				emit (A.OPER {assem="sraw `d0, `s0, `s1\n", 
			 											dst=[r], 
			 											src=[munchExp e1, munchExp e2], 
			 											jump=NONE}))
			 
			 (*|	munchExp (T.ESEQ _) = Error( ErrorInternalError "CodeGen: ESEQ presente en un arbol canonizado!", 0)*)
			 |	munchExp e = 
			 			let
			 				(*val _ = print (tigerpp.ppIrExp e ^ "\n")*)
			 			in
			 				Error (ErrorInternalError "CodeGen.codegen.munchExp: pattern matching incompleto!", 0)
			 			end
			
			(*
				if there are args
					if there are parameter passing registers available 
					then choose the next available and save the arg there
					else save the next arg in the parameter passing area
					
					after that return reg :: munchArgs (i+1) args
				else
					return []
			*)
			
			and munchArgs args =
				let 
					val i = ref 0
					(*val formals = TigerFrame.getFormals (valOf(tigertab.tabSearch ))*)
					
					fun ma [] regs = []
					 |	ma (a::args') [] = (* guardar a en la memoria *)
					 			let
					 				val t = munchExp a
					 				val argOffset = (!i * TigerFrame.wordSize) + TigerFrame.linkAreaSize
					 			in
					 				emit(A.OPER {assem="stw `s0, " ^ int argOffset ^ "(r1)\n", 
					 										 dst=[], src=[t], jump=NONE});
					 				i := !i+1;
					 				ma args' []
					 			end
					 |	ma (a::args') (r::regs) = (* copiar a en r *)
					 			let
					 				val t = munchExp a
					 			in
					 				emit(A.MOVE {assem="mr `d0, `s0\n", dst=r, src=t});
					 				i := !i+1;
					 				r :: ma args' regs
					 			end 
				in
					ma args (TigerFrame.RV :: TigerFrame.argregs)
				end
			
		in 
			munchStm stm; rev (!ilist)
		end
		
		fun literals list =
			let
				fun gen_code ((l,s), code) =
					let
						val str = valOf(String.fromString s)
						val size = String.size str
						val lab = TigerTemp.labelname l
						val def = "\t.globl " ^ lab ^ "\n"
					 					^ "\t.align 2\n"
					 					^ lab ^ ":\n"
					 					^ "\t.long " ^ int size ^ "\n"
					 					^ "\t.ascii \"" ^ String.toString str ^ "\"\n"
					in
					 def ^ code
					end
			in
				List.foldr gen_code "" list
			end
			
			val sections =
				let
					val init = "\t.section __TEXT,__text,regular,pure_instructions\n"
									 ^ "\t.section __TEXT,__picsymbolstub1,symbol_stubs,pure_instructions,32\n"
									 ^ "\t.machine ppc7400\n"
					val data = "\t.data\n"
					val text = "\t.text\n"
				in
					(init, data, text)
				end
end