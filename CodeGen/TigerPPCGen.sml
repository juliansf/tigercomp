structure TigerPPCGen :> TigerPPCGen =
struct
	open TigerAssem
	open TigerFrame
	
	structure T = TigerTree
	structure A = TigerAssem
	
	fun sign i = if (i < 0) then "-"^makestring(~i) else makestring(i)
	
	fun relOp relop =
		case relop of
			EQ 	=>	"eq"	|	NE	=>	"ne"	
		|	LT	=>	"lt"	|	LE	=>	"le"	
		|	GT	=>	"gt"	|	GE	=>	"ge"	
		|	ULT	=>	"lt"	|	ULE	=>	"le"
		|	UGT	=>	"gt"	|	UGE	=>	"ge"
	
	fun munchArgs i args = [TigerFrame.CR0] (* placeholder *)
	
	fun codegen frame (stm: T.stm) : A.instr list =
		let 
			val ilist = ref (nil: A.instr list)
			fun emit x = ilist := x :: !ilist
			fun result (gen) = let val t = TigerTemp.newtemp() in gen t; t end
			
			fun munchStm (T.SEQ (a,b)) = (munchStm a; munchStm b)
			
			 |	munchStm (T.MOVE(T.TEMP t, T.MEM(T.BINOP(T.PLUS, T.CONST j, e)))) = 
			 			emit (A.OPER {assem="lwz `d0, " ^ sign j ^ "(`s0)\n", 
			 										dst=[t], src=[munchExp e], jump=NONE})
			 			
			 |	munchStm (T.MOVE(T.TEMP i, T.MEM(T.BINOP(T.PLUS, e, TCONST j)))) = 
			 			emit (A.OPER {assem="lwz `d0, " ^ sign j ^ "(`s0)\n", 
			 										dst=[t], src=[munchExp e], jump=NONE})
			 									
			 |	munchStm (T.MOVE(T.TEMP t, T.MEM e)) = 
			 			emit (A.OPER {assem="la `d0, (`s0)\n", 
			 										dst=[t], src=[munchExp e], jump=NONE})
			 									
			 |	munchStm (T.MOVE(T.TEMP t, T.CONST j)) =
			 			emit (A.OPER {assem="li `d0, " ^ sign j ^ "\n",
			 										dst=[t], src=[], jump=NONE})
			 			
			 |	munchStm (T.MOVE(T.TEMP t, e)) =
			 			emit (A.MOVE {assem="mr `d0, `s0\n",
			 										dst=t, src=munchExp e})
			 										
			 |	munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
			 			emit (A.OPER {assem="stw `s0, " ^ sign i ^ "(`d0)\n",
			 										dst=[munchExp e1], src=[munchExp e2], jump=NONE})
			 										
			 |	munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) = ()
			 			emit (A.OPER {assem="stw `s0, " ^ sign i ^ "(`d0)\n",
			 										dst=[munchExp e1], src=[munchExp e2], jump=NONE})
			 
			 (*										
			 |	munchStm (T.MOVE(T.MEM(T.CONST i), e)) = ()
			 			emit (A.OPER {assem="stw `s0, " ^ sign i ^ "(r0)\n",
			 										dst=[], src=[munchStm], jump=NONE})
			 *)
			 
			 |	munchStm (T.MOVE(T.MEM e1, e2)) = 
			 			emit (A.OPER {assem="stw `s0, 0(`d0)\n",
			 										dst=[munchExp e1], src=[munchExp e2], jump=NONE})
			 										
			 |	munchStm (T.JUMP(T.NAME l, labels)) =
			 			emit (A.OPER {assem="ba " ^ TigetTemp.labelname l ^ "\n",
			 										dst=[], src=[], jump=SOME labels})
			 			
			 |	munchStm (T.JUMP(e, labels)) =
			 			emit (A.OPER {assem="ba `s0\n",
			 										dst=[], src=[munchExp e], jump=SOME labels})
			 										
			 |	munchStm (T.EXP(T.CALL(e, args))) =
			 			emit (A.OPER {assem="bl `s0\n",
			 										dst=TigerFrame.calldefs,
			 										src=munchExp e :: munchArgs(0, args),
			 										jump=NONE})
			 
			 |	munchStm(T.EXP(e)) =
			 			emit (A.MOVE {assem="mr `d0, `s0\n",
			 										dst=TigerTemp.newtemp(), src=munchExp e})
			 
			 |	munchStm (T.CJUMP(relop, e1, e2, lv, lf)) = 
			 			(
			 				emit (A.OPER {assem="cmpw `d0, `s0, `s1\n",
			 											dst=[TigerFrame.CR0], 
			 											src=[munchExp e1, munchExp e2], 
			 											jump=NONE});
			 				emit (A.OPER {assem= relOp relop ^ ", " ^ TigerTemp.labelname lv ^ "\n",
			 											dst=[], src=[TigerFrame.CR0], jump=SOME [lv, lf]})
			 			)
			 			
			 |	munchStm (T.LABEL l) =
			 			emit (A.LABEL {assem=TigerTemp.labelname l ^ ":\n", lab=l})
			
			and munchExp (T.CONST 0) = result ()
			 |	munchExp (T.CONST i) = result ()
			 |	munchExp (T.NAME l) = result ()
			 |	munchExp (T.TEMP t) = result ()
			 |	munchExp (T.MEM(T.BINOP(T.PLUS, T.CONST i, e))) = result ()
			 |	munchExp (T.MEM(T.BINOP(T.PLUS, e, T.CONST i))) = result ()
			 |	munchExp (T.MEM(T.CONST i)) = result ()
			 |	munchExp (T.MEM e) = result ()
			 |	munchExp (T.CALL(e, args)) = result ()
			 |	munchExp (T.BINOP(T.PLUS, T.CONST 0, e)) = result ()
			 |	munchExp (T.BINOP(T.PLUS, e, T.CONST 0)) = result ()
			 |	munchExp (T.BINOP(T.PLUS, T.CONST i, e)) = result ()
			 |	munchExp (T.BINOP(T.PLUS, e, T.CONST i)) = result ()
			 |	munchExp (T.BINOP(T.PLUS, e1, e2)) = result ()
			 
			 
			 |	munchExp (T.BINOP(T.MINUS, T.CONST (~1), e)) = result ()
			 |	munchExp (T.BINOP(T.MINUS, T.CONST 0, e)) = result ()
			 |	munchExp (T.BINOP(T.MINUS, e, T.CONST 0)) = result ()
			 |	munchExp (T.BINOP(T.MINUS, e, T.CONST 1)) = result ()
			 |	munchExp (T.BINOP(T.MINUS, T.CONST i, e)) = result ()
			 |	munchExp (T.BINOP(T.MINUS, e, T.CONST i)) = result ()
			 |	munchExp (T.BINOP(T.MINUS, e1, e2)) = result ()
			 
			 |	munchExp (T.BINOP(T.MUL, T.CONST (~1), e)) = result () (* neg *)
			 |	munchExp (T.BINOP(T.MUL, e, T.CONST (~1))) = result () (* neg *)
			 |	munchExp (T.BINOP(T.MUL, T.CONST 0, e)) = result () (* r0 *)
			 |	munchExp (T.BINOP(T.MUL, e, T.CONST 0)) = result () (* r0 *)
			 |	munchExp (T.BINOP(T.MUL, T.CONST 1, e)) = result () (* e *)
			 |	munchExp (T.BINOP(T.MUL, e, T.CONST 1)) = result () (* e *)
			 |	munchExp (T.BINOP(T.MUL, T.CONST i, e)) = result () (* mulli *)
			 |	munchExp (T.BINOP(T.MUL, e, T.CONST i)) = result () (* mulli *)
			 |	munchExp (T.BINOP(T.MUL, e1, e2)) = result () (* mullw *)
			 
			 |	munchExp (T.BINOP(T.DIV, e1, e2)) = result () (* divw *)
			 
			 |	munchExp (T.BINOP(T.AND, T.CONST i, e)) = result () (* andi. *)
			 |	munchExp (T.BINOP(T.AND, e, T.CONST i)) = result () (* andi. *)
			 |	munchExp (T.BINOP(T.AND, e1, e2)) = result () (* and. *)
			 
			 |	munchExp (T.BINOP(T.OR, T.CONST i, e)) = result () (* ori. *)
			 |	munchExp (T.BINOP(T.OR, e, T.CONST i)) = result () (* ori. *)
			 |	munchExp (T.BINOP(T.OR, e1, e2)) = result () (* or. *)
			 
			 |	munchExp (T.BINOP(T.XOR, T.CONST i, e)) = result () (* xori. *)
			 |	munchExp (T.BINOP(T.XOR, e, T.CONST i)) = result () (* xori. *)
			 |	munchExp (T.BINOP(T.XOR, e1, e2)) = result () (* xor. *)
			 
			 |	munchExp (T.BINOP(T.LSHIFT, e1, e2)) = result () (* slw *)
			 
			 |	munchExp (T.BINOP(T.RSHIFT, e1, e2)) = result () (* srw *)
			 
			 |	munchExp (T.BINOP(T.ARSHIFT, e, T.CONST i)) = result () (* srawi *)
			 |	munchExp (T.BINOP(T.ARSHIFT, e1, e2)) = result () (* sraw *)
			 
			 |	munchExp (T.ESEQ _) = Error( ErrorInternalError "CodeGen: ESEQ presente en un arbol canonizado!", 0)
			 |	munchExp _ = Error (ErrorInternalError "CodeGen.codegen.munchExp: pattern matching incompleto!", 0)
			
		in 
			munchStm stm; rev (!ilist)
		end
end