structure TigerTranslate :> TigerTranslate =
struct
	open TigerTemp
	open TigerError
	open TigerAbs
	open TigerTree
	open TigerFrame
	
	type frag = TigerFrame.frag
	
	type level = { depth: int, lstack: label list ref, frame: frame, parent: frame option }
	type access = level * TigerFrame.access
	
	val outermost = {depth=0, lstack = ref [], frame = newFrame (namedlabel "_tigermain", []), parent=NONE }
	
	val lfrag : frag list ref = ref []
							
	fun getResult() = !lfrag
	fun addProc(body, frame) = lfrag := PROC {body=body, frame=frame} :: (!lfrag) 
	fun addString string = 
		let 
			fun p (PROC _) = false
			 |	p (STRING (l,s)) = if s = string then true else false
		in
			case List.find p (!lfrag) of
				SOME (STRING (l,s)) => l
			|	SOME _ => Error (ErrorInternalError "problemas con TigerTranslate.addString!", 0)
			| NONE => let val l = newlabel() in (lfrag := STRING (l, string) :: (!lfrag); l) end
		end
	
	fun toplb lst = hd(!lst)
	fun pushlb lst x = lst := x :: (!lst)
	fun poplb lst = lst := tl(!lst)
	
	fun preWhileFor (level:level) = pushlb (#lstack(level)) (newlabel())
	fun posWhileFor (level:level) = poplb (#lstack(level))
	
	fun sl_access (caller:level) (callee:level) =
		TigerFrame.sl_access (#depth(caller) - #depth(callee))
	
	fun var_access (varlevel:level, access) (level:level) =
		TigerFrame.var_access (access, #depth(level) - #depth(varlevel), #frame(varlevel))
	
	fun newLevel 
	(parent:level, name, formals) =
		{depth = #depth(parent) + 1, lstack = ref [], 
		 frame = newFrame (namedlabel2 name, formals), 
		 parent = SOME (#frame(parent)) }
		
	fun getLevelLabel (level:level) = getFrameLabel(#frame(level))
	
	fun formals (level:level) = List.map (fn x => (level, x)) (TigerFrame.getFormals (#frame(level)))
	
	fun allocLocal (level:level) escapes =
		(level, TigerFrame.allocLocal (#frame(level)) escapes)

	datatype exp =
	  Ex of TigerTree.exp (* Expresion *)
	| Nx of TigerTree.stm (* No Result *)
	| Cx of TigerTemp.label * TigerTemp.label -> TigerTree.stm (* Condicion *)
	
	(* Funciones desempaquetadoras de expresiones *)
	fun unEx (Ex e) = e
		|	unEx (Nx s) = ESEQ (s, CONST 0)
		| unEx (Cx c) = 
			let 
				val (lv, lf) = (newlabel(), newlabel())
				val temp = newtemp()
			in
				ESEQ ( seq [ MOVE ( TEMP temp, CONST 0 ),
										 c (lv, lf),
										 LABEL lv,
										 MOVE ( TEMP temp, CONST 1 ),
										 LABEL lf ],
							TEMP temp )
			end
			
	fun unNx (Ex e) = EXP e
		|	unNx (Nx s) = s
		| unNx (Cx c) = 
			let 
				val (lv, lf) = (newlabel(), newlabel())
			in
				seq [c (lv, lf), LABEL lv, LABEL lf]
			end
	
	fun unCx (Ex (CONST n)) = (fn (lv, lf) => JUMP (NAME (if n = 0 then lf else lv), [lv, lf]))
		|	unCx (Ex e) = (fn (lv, lf) => CJUMP ( NE, e, CONST 0, lv, lf ))
		| unCx (Nx s) = Error ( ErrorInternalError "problemas con Translate.unCx!", 0)
		| unCx (Cx c) = c
		
	(* Funciones de traduccion a codigo intemedio *)
	fun unitExp () = Ex (CONST 0)
	fun nilExp () = Ex (CONST 0) (* puntero a null *)
	fun intExp n = Ex (CONST n)
	
	fun stringExp s = Ex (NAME (addString s))
		
	fun callExp (name, params, caller, callee, proc:bool) =
		let 
			val params' = sl_access caller callee :: List.map unEx params
			val _ = TigerFrame.setMaxCallArgs (#frame(caller)) (List.length params')
		in
			if not proc then
				let
					val temp = newtemp()
				in
					Ex (ESEQ (SEQ (EXP (CALL (NAME name, params')), MOVE (TEMP temp, TEMP RV)), TEMP temp))
				end
			else
				Nx (EXP (CALL (NAME name, params')))
		end
	
	fun opExp (oper, exp1, exp2) =
		let 
			val exp1 = unEx exp1 and exp2 = unEx exp2
		in
			case oper of
				Plus => Ex (BINOP (PLUS, exp1, exp2))
			| Minus => Ex (BINOP (MINUS, exp1, exp2))
			| Times => Ex (BINOP (MUL, exp1, exp2))
			| Div => Ex (BINOP (DIV, exp1, exp2))
			| Eq => Cx (fn (lv,lf) => CJUMP (EQ, exp1, exp2, lv, lf))
			| Neq => Cx (fn (lv,lf) => CJUMP (NE, exp1, exp2, lv, lf))
			| Gt => Cx (fn (lv,lf) => CJUMP (GT, exp1, exp2, lv, lf))
			| Geq => Cx (fn (lv,lf) => CJUMP (GE, exp1, exp2, lv, lf))
			| Lt => Cx (fn (lv,lf) => CJUMP (LT, exp1, exp2, lv, lf))
			| Leq => Cx (fn (lv,lf) => CJUMP (LE, exp1, exp2, lv, lf))
		end
	
	fun recordExp (inits, level:level) =
		let 
			val inits' = List.map unEx inits
			val temp = newtemp()
			val _ = TigerFrame.setMaxCallArgs (#frame(level)) (List.length inits' + 1)
		in
			Ex (ESEQ (
						SEQ (EXP (externalCall ("_createRecord", (CONST (List.length inits)::inits'))),
								MOVE (TEMP temp, TEMP RV)), 
						TEMP temp))
		end
	
	fun seqExp (expl, lastexp, proc) = 
		if proc then Nx (SEQ (seq (List.map unNx expl), unNx lastexp))
		else Ex (ESEQ (seq (List.map unNx expl), unEx lastexp))
	
	fun assignExp (var, exp) = Nx (MOVE (unEx var, unEx exp))
	
	fun ifExp (a, b, _, TigerAbs.And, _) =
		Cx (fn (lv, lf) => let val li = newlabel()
											 in seq([(unCx a) (li,lf), LABEL li, (unCx b) (lv,lf)]) end)

	| ifExp (a, _, b, TigerAbs.Or, _) =
		Cx (fn (lv, lf) => let val li = newlabel()
											 in seq([(unCx a) (lv,li), LABEL li, (unCx b) (lv,lf)]) end)	
		
	| ifExp (cond, then', else', TigerAbs.If, false) =
		let
			val (lv, lf, join) = (newlabel(), newlabel(), newlabel())
			val temp = newtemp()
		in
			Ex (ESEQ (
						seq [(unCx cond) (lv, lf),
								LABEL lv, MOVE (TEMP temp, unEx then'), JUMP (NAME join, [join]),
								LABEL lf, MOVE (TEMP temp, unEx else'), LABEL join],
						TEMP temp))
		end
	
	| ifExp (cond, then', else', TigerAbs.If, true) =
		let
			val (lv, lf, join) = (newlabel(), newlabel(), newlabel())
		in
			Nx (seq [(unCx cond) (lv, lf),
								LABEL lv, unNx then', JUMP (NAME join, [join]),
								LABEL lf, unNx else', LABEL join])
		end
		
	fun whileExp (test, body, level:level) =
		let
			val (lv, lf, ls) = (newlabel(), toplb(#lstack(level)), newlabel())
		in
			Nx(seq[LABEL ls, (unCx test) (lv, lf), LABEL lv, unNx body, JUMP (NAME ls, [ls]), LABEL lf]) 
		end
		
	fun forExp (index, lo, hi, body, level:level) =
		let
			val index' = var_access index level
			val (loop, sale, sigue) = (newlabel(), toplb(#lstack(level)), newlabel())
			val tmphi = newtemp() 
		in
			Nx (seq [ MOVE (index', unEx lo),
				MOVE (TEMP tmphi, unEx hi),
				CJUMP (GT, index', TEMP tmphi, sale, loop),
				LABEL sigue,
				MOVE (index', BINOP (PLUS, index', CONST 1)),
				LABEL loop,
				unNx body,
				CJUMP (GE, index', TEMP tmphi, sale, sigue),
				LABEL sale])
		end
	
	fun letExp (inits, body, true) = Nx (seq(List.map unNx inits @ [unNx body]))
		| letExp (inits, body, false) = Ex(
				case inits of 
					[] => unEx body 
				| inits => ESEQ (seq (List.map unNx inits), unEx body))

	fun breakExp (level:level) =
		let 
			val label = toplb (#lstack(level))
				handle Empty => raise BreakError
		in
			Nx(JUMP (NAME label, [label]))
		end
		
	fun arrayExp(size, init, level:level)=
		let 
			val temp = newtemp()
			val _ = TigerFrame.setMaxCallArgs (#frame(level)) 2
		in
			Ex (ESEQ (
						SEQ (EXP (externalCall ("_createArray", [unEx size, unEx init]) ),
								MOVE (TEMP temp, TEMP RV)), 
						TEMP temp))
		end		
	
	(* Traduccion de Variables *)
	fun simpleVar (access, level) = Ex (var_access access level)
	
	fun fieldVar (varaddr, offset, level:level) =
		let
			val varaddr' = unEx varaddr
			val _ = TigerFrame.setMaxCallArgs (#frame(level)) 1
		in
			Ex (ESEQ (EXP (externalCall ("_checkNil", [varaddr'])),
								MEM (BINOP (PLUS, BINOP (MUL, CONST wordSize, CONST offset), varaddr'))))
		end
	
	fun subscriptVar (varaddr, expOffset, level:level) =
		let
			val expOffset' = unEx expOffset
			val varaddr' = unEx varaddr
			val _ = TigerFrame.setMaxCallArgs (#frame(level)) 2
		in
			Ex (ESEQ (EXP (externalCall ("_checkIndex", [expOffset', varaddr'])),
								MEM (BINOP (PLUS, BINOP (MUL, CONST wordSize, expOffset'), varaddr'))))
		end
	
	(* Traduccion de declaraciones *)
	fun varDec (access, init) = 
		let
			val varaddr = var_access access (#1(access))
		in
			Nx (MOVE (varaddr, unEx init))
		end
		
	(* Construccion de un nuevo Fragmento de tipo PROC *)
	fun procEntryExit (level:level, body) =
		let
			val body' = case body of Nx s => s | s => MOVE (TEMP RV, unEx s)
									
			val frame = #frame(level)
			val funlabel = getFrameLabel(frame)
			val procbody = 
				seq[SEQ(LABEL (namedlabel "Prologo"), MOVE (MEM (TEMP SP), TEMP FP)), 
						SEQ(LABEL (namedlabel "Body"), body'), 
						SEQ(LABEL (namedlabel "Epilogo"), MOVE (TEMP SP, MEM(TEMP SP)))]
		in
			addProc(SEQ(LABEL funlabel, procEntryExit1 (procbody, frame)), frame )
		end	
end