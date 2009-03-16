structure TigerPPCGen :> TigerPPCGen =
struct
	open TigerAssem
	open TigerFrame
	
	structure T = TigerTree
	structure A = TigerAssem
	
	fun codegen frame (stm: T.stm) : A.instr list =
		let 
			val ilist = ref (nil: A.instr list)
			fun emit x = ilist := x :: !ilist
			fun result (gen) = let val t = TigerTemp.newtemp() in gen t; t end
			
			fun munchStm (T.SEQ (a,b)) = (munchStm a; munchStm b)
			 |	munchStm (T.MOVE(T.TEMP i, T.MEM(T.BINOP(T.PLUS, T.CONST j, e)))) = ()
			 |	munchStm (T.MOVE(T.TEMP i, T.MEM(T.BINOP(T.PLUS, e, TCONST j)))) = ()
			 |	munchStm (T.MOVE(T.TEMP i, T.MEM e)) = ()
			 |	munchStm (T.MOVE(T.TEMP i, T.CONST j)) = ()
			 |	munchStm (T.MOVE(T.TEMP i, e)) = ()
			 |	munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) = ()
			 |	munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) = ()
			 |	munchStm (T.MOVE(T.MEM(T.CONST i), e)) = ()
			 |	munchStm (T.MOVE(T.MEM e1, T.MEM e2)) = ()
			 |	munchStm (T.MOVE(T.MEM e1, e2)) = ()
			 |	munchStm (T.JUMP(T.NAME l, labels)) = ()
			 |	munchStm (T.JUMP(e, labels)) = ()
			 |	munchStm (T.EXP(T.CALL(e, args))) = ()
			 |	munchStm (T.CJUMP(relop, e1, e2, lv, lf)) = ()
			 |	munchStm (T.LABEL l) = ()
			
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