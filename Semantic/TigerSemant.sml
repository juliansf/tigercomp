structure TigerSemant :> TigerSemant =
struct
	open TigerAbs
	open tigertab
	open TigerEnv
	open TigerTypes
	open TigerUtils
	open TigerError
	open TigerTranslate
	
	type venv = (string, enventry) Tabla 
	type tenv = (string, TigerTypes.ty) Tabla
	type expty = { exp: TigerTranslate.exp, ty:TigerTypes.ty }
	
	(******************************************************)
	(* TransExp:                                          *)
	(*  Tipa las expresiones y devuelve codigo intermedio *)
	(******************************************************)
	fun transExp ( venv, tenv, expr, level:level ) =
		let 
			fun trexp ( VarExp (v,p) ) = transVar ( venv, tenv, v, p, level )
				| trexp ( UnitExp _ ) = { exp=unitExp(), ty=UNIT }
				| trexp ( NilExp _ ) = { exp=nilExp(), ty=NIL }
				| trexp ( IntExp (n,p) ) = { exp=intExp n, ty=INT RO } (* Ojo! El RO puede no estar bien *)
				| trexp ( StringExp (s,p) ) = { exp=stringExp s, ty=STRING }
				| trexp ( CallExp ({ func, args }, pos) ) =
						let 
							val { flevel, label, formals, result, ext } =
							(* Buscamos la funcion en el entorno *)
							case tabSearch venv func of
								(* Obtenemos los parametros formales y el tipo del resultado *)
								SOME (FunEntry { level, label, formals, result, ext }) => 
									{ flevel = level, label=label, formals=formals, result=result, ext=ext }
							| _ => Error ( ErrorUndefinedFunction func, pos )
							
							(* Obtenemos los tipos de los argumentos pasados a la funcion *)
							val (expList,tyList) = 
								List.foldr ((fn (r, (el,tl) ) => ((fn x => (#exp(x)::el, #ty(x)::tl)) o trexp) r)) ([],[]) args
							
							(* 
								Usamos equalTypes para chequear cada uno de los tipos de los argumentos 
								pasados a la funcion. Si alguno esta mal, se informa cual es la
								posicion del argumento.
							*)
							val count = ref 1
							fun equalTypes (a, b) = 
								(if weakCompTypes ( a, b ) then ( count := !count + 1; true )
								else Error ( ErrorWrongFunArgType (func, !count), pos ))
								handle InternalError msg => Error ( ErrorInternalError msg, pos )
						in
							(* La cantidad de argumentos pasados es la correcta *)
							if List.length tyList = List.length formals
							then 
								( 
									(* Se chequean los parametros *)
									ListPair.all equalTypes (tyList, formals); 
									{ exp=callExp(label, expList, level, flevel, result=UNIT, ext) , ty=result } 
								)
							else
								(* La cantidad de argumetnos pasados es menor o mayor a la esperada *)
								if List.length tyList < List.length formals
								then Error ( ErrorFewArgsForFunction func, pos )
								else Error ( ErrorManyArgsForFunction func, pos )
						end
				| trexp ( OpExp ({left, oper, right}, pos) ) = (* Operadores Binarios *)
						let 
							(* Aplicamos trexp a los operandos *)
							val { exp=expleft, ty=tyleft } = trexp left
							val { exp=expright, ty=tyright } = trexp right
							
							fun i () = (* Int *)
								case ( tyleft, tyright ) of
									( INT _, INT _) => { exp=opExp(oper, expleft, expright), ty=INT RO }
								| _ => Error ( ErrorInvalidArgsTypesForOperator oper, pos )
							
							fun is () = (* Int, String *)
								case ( tyleft, tyright ) of
									( INT _, INT _) => { exp=opExp(oper, expleft, expright), ty=INT RO }
								| ( STRING, STRING ) => { exp=opExpString(oper, expleft, expright, level), ty=INT RO }
								| _ => Error ( ErrorInvalidArgsTypesForOperator oper, pos )
								
							fun isar () = isar' (tyleft, tyright) (* Int, String, Array, Record *)
							and isar' (a, b) =
								case ( a, b ) of
									( INT _, INT _) => { exp=opExp(oper, expleft, expright), ty=INT RO }
								| ( STRING, STRING ) => { exp=opExpString(oper, expleft, expright, level), ty=INT RO }
								| ( RECORD (_,uniq1), RECORD (_,uniq2) ) =>
										if uniq1 = uniq2 then { exp=opExp(oper, expleft, expright), ty=INT RO }
										else Error ( ErrorInvalidArgsTypesForOperator oper, pos )
								| ( RECORD _, NIL ) => { exp=opExp(oper, expleft, expright), ty=INT RO }
								| ( NIL, RECORD _ ) => { exp=opExp(oper, expleft, expright), ty=INT RO }
								| ( ARRAY (_,uniq1), ARRAY (_,uniq2) ) => 
										if uniq1 = uniq2 then { exp=opExp(oper, expleft, expright), ty=INT RO }
										else Error ( ErrorInvalidArgsTypesForOperator oper, pos )
								| (NAME (n,tyr), b) => 
										(case !tyr of
											SOME (r as RECORD _) => isar' (r, b)
										| SOME (a as ARRAY _) => isar' (a, b)
										| _ => Error (ErrorInternalError "comparando tipo NAME que tiene (ref NONE)!", pos))
								| (a, NAME (n,tyr)) => 
										(case !tyr of
											SOME (r as RECORD _) => isar' (a, r)
										| SOME (b as ARRAY _) => isar' (a, b)
										| _ => Error (ErrorInternalError "comparando tipo NAME que tiene (ref NONE)!", pos))
								| _ => Error ( ErrorInvalidArgsTypesForOperator oper, pos )
						in
							case oper of
								Plus 	=> i()
							| Minus => i()
							| Times => i()
							| Div 	=> i()
							| Eq 		=> isar()
							| Neq 	=> isar()
							| Gt		=> is()
							| Geq		=> is()
							| Lt		=> is()
							| Leq 	=> is()
						end
				| trexp ( RecordExp ({ fields, typ }, pos) ) = 
						let
							val (rty, realfields) = case tabSearch tenv typ of
								SOME (R as RECORD(fl,_)) => (R, fl)
								| SOME _ => Error (ErrorTypeIsNotRecord typ, pos)
								| _ => Error (ErrorUndefinedType typ, pos)
							val errorList = ref []
							val expList = ref []
							fun comp x (y,z) = x = y
							fun checkField defl (name, ty) =
								case List.find (comp name) (!defl) of
									SOME (_, e) => let
															val {exp=expexp, ty=tyexp} = trexp e 
														in
															defl := listRemoveItem (comp name) (!defl);
															(if weakCompTypes (tyexp, ty) then
																expList := expexp :: (!expList)
															else
																errorList := TypeMismatch name :: (!errorList))
															handle InternalError msg => Error ( ErrorInternalError msg, pos )
														end
									| NONE => errorList := MissingField name :: (!errorList)
							fun checkError (name, _) =
								case List.find (comp name) realfields of
									SOME _ => DuplicatedField name
									| NONE => FieldNotMember name
							val defl = ref fields;
						in
							List.app (checkField defl) realfields;
							case List.map checkError (!defl) @ (!errorList) of
								[] => {exp=recordExp (rev (!expList), level), ty=rty}
								| l => Error (ErrorRecordFields (rev l), pos)
						end

				| trexp ( SeqExp (expl, pos) ) =
						let
							val firstexp = trexp (hd expl)
							val lastexp = ref firstexp
							
							fun checkExp (exp :: []) = ( lastexp := trexp exp; [] )
								| checkExp (exp :: expl) = #exp(trexp exp) :: checkExp expl
								| checkExp _ = Error ( ErrorInternalError "problemas con Semant.transExp.trexp( SeqExp .. ).checkExp!", pos )
								
							val expl' =  #exp(firstexp) :: checkExp (tl expl)
						in
							{ exp=seqExp (expl', #exp(!lastexp), #ty(!lastexp) = UNIT), ty=(#ty(!lastexp)) }
						end
						
				| trexp ( AssignExp ({ var, exp }, pos) ) =
						let
							val { exp=expvar, ty=tyvar } = transVar (venv, tenv, var, pos, level)
							val { exp=expexp, ty=tyexp } = trexp exp
						in
							if tyvar = INT RO then Error ( ErrorReadOnlyVariable var, pos )
							else 
								(if weakCompTypes (tyvar, tyexp) then { exp=assignExp(expvar,expexp), ty=UNIT }
								else Error ( ErrorAssignTypeMismatch var, pos ))
								handle InternalError msg => Error ( ErrorInternalError msg, pos )
						end
				| trexp ( IfExp ({ test, then', else', oper },pos) ) =
						let
							val { exp=exptest, ty=tytest } = trexp test
							val { exp=expthen, ty=tythen } = trexp then'
							val { exp=expelse, ty=tyelse } =
								case else' of
									SOME else'' => trexp else''
							  | NONE => { exp=unitExp(), ty=UNIT }
						in
							if not (isInt tytest) then Error ( ErrorIfTest oper, pos ) 
							else
								(if weakCompTypes (tythen, tyelse) then { exp=ifExp(exptest,expthen,expelse,oper,tythen=UNIT), ty=tythen }
								else Error ( ErrorIfTypeMismatch oper, pos ))
								handle InternalError msg => Error ( ErrorInternalError msg, pos )
						end
				| trexp ( WhileExp ({ test, body }, pos) ) =
						let
							val _ = preWhileFor(level) 
							val { exp=exptest, ty=tytest } = trexp test
						in
							if not (isInt tytest) then Error ( ErrorWhileTest, pos ) 
							else
								let
									val { exp=expbody, ty=tybody } = trexp body
									val expWhile = whileExp(exptest, expbody, level)
									val _ = posWhileFor(level)
								in
									if tybody = UNIT then { exp=expWhile, ty=UNIT }
									else Error ( ErrorWhileBody, pos )
								end
						end
				| trexp ( ForExp ({ var, escape, lo, hi, body }, pos) ) =
						let
							val { exp=explo, ty=tylo } = trexp lo
							val { exp=exphi, ty=tyhi } = trexp hi
							
							val indexAccess = allocLocal level (!escape)
							
							val venv' = fromTable venv
							val venv' = tabRInsert venv' (var, VarEntry { access=indexAccess, ty=INT RO } )
							
						in 
							if not (isInt tylo) then Error ( ErrorForLowExp, pos ) else ();
							if not (isInt tyhi) then Error ( ErrorForHiExp, pos ) else ();
							preWhileFor(level);
							let 
								val { exp=expbody, ty=tybody } = transExp ( venv', tenv, body, level )
							in
								if tybody <> UNIT then Error ( ErrorForWrongBodyType, pos ) 
								else
									let 
										val expFor = forExp (indexAccess, explo, exphi, expbody, level)
									in 
										posWhileFor(level);
										{ exp=expFor, ty=UNIT } 
									end
							end
						end
				| trexp ( LetExp ({ decs, body }, pos) ) =
						let
							val { venv=venv', tenv=tenv', decs=expdecs} =
								List.foldl (fn (x,y) => transDec (#venv(y), #tenv(y), x, level, #decs(y))) 
													 {venv=fromTable venv, tenv=fromTable tenv, decs=[]} decs
							
							val { exp=expbody, ty=tybody } = transExp ( venv', tenv', body, level )
						in
							{ exp=letExp(List.rev expdecs, expbody, tybody=UNIT), ty=tybody }
						end
				| trexp ( BreakExp pos ) = ({ exp=breakExp(level), ty=UNIT } 
						handle BreakError => Error (ErrorWrongBreakUsage, pos))
				| trexp ( ArrayExp ({typ, size, init}, pos) ) =
						let
							val (aty, auniq) = case tabSearch tenv typ of
								SOME (ARRAY (t,u)) => (t,u)
								| _ => Error ( ErrorUndefinedType typ, pos )
							val { exp=expsize, ty=tysize } = trexp size
							val { exp=expinit, ty=tyinit } = trexp init
						in
							if not (isInt(tysize)) then 
								Error ( ErrorArraySizeTypeIsNotInt tysize, pos ) else ();
							(if not (weakCompTypes(aty, tyinit)) then  
                Error ( ErrorArrayTypeMismatch typ, pos ) else ())			
              handle InternalError msg => Error ( ErrorInternalError msg, pos );				

							{ exp=arrayExp (expsize, expinit, level), ty=ARRAY (aty, auniq) }
						end						
		in
			trexp expr
		end

	and transVar ( venv, tenv, SimpleVar v, pos, level ) =
		let
			val {access, ty} = case tabSearch venv v of
				SOME (VarEntry v) => v
				| _ => Error ( ErrorUndefinedVariable v, pos )
		in
			{ exp=simpleVar (access, level), ty=ty }
		end

	| transVar ( venv, tenv, FieldVar (var, symbol), pos, level) =
		let
			val { exp=expvar, ty=tyvar } = transVar ( venv, tenv, var, pos, level )
			
			val ml = 
				case tyvar of
					NAME (n, ref (SOME (RECORD (ml, _)))) => ml
				| RECORD (ml, _) => ml
				| _ => Error ( ErrorVariableIsNotRecord var, pos )
				
			val offset = ref ~1
		in
			case List.find (fn (x,y) => (offset := !offset + 1; x = symbol)) ml of
				SOME (sym,ty) => { exp=fieldVar(expvar, !offset, level), ty=ty }
			| NONE => Error ( ErrorRecordFieldUndefined (var, symbol), pos )
		end
			
	| transVar ( venv, tenv, SubscriptVar (var, exp), pos, level ) =
		let
			val { exp=expvar, ty=tyvar } = transVar ( venv, tenv, var, pos, level )
			val { exp=expexp, ty=tyexp } = transExp ( venv, tenv, exp, level )
			
			val ty = case tyvar of
				NAME (n, ref (SOME (ARRAY (ty,_)))) => ty
			| ARRAY (ty, _) => ty
			| _ => Error ( ErrorVariableIsNotArray var, pos ) 
		in
			if isInt tyexp then { exp=subscriptVar(expvar, expexp, level), ty=ty }
			else Error ( ErrorArrayIndexIsNotInt, pos )
		end
	
	and transDec ( venv, tenv, VarDec ({ name, escape, typ=NONE, init}, pos ), level, decs ) =
		let
			val { exp=expinit, ty=tyinit } = transExp ( venv, tenv, init, level )
			val tyinit = case tyinit of INT _ => INT RW | t => t
			val access = allocLocal level (!escape)
			val venv = tabRInsert venv ( name, VarEntry { access=access, ty=tyinit } )
		in
			if tyinit <> UNIT then { venv=venv, tenv=tenv, decs=varDec (access, expinit) :: decs }
			else Error ( ErrorVarDecInitIsUnit name, pos )
		end
	
	| transDec ( venv, tenv, VarDec ({ name, escape, typ=SOME ty, init }, pos ), level, decs ) =
		let
			val ty' = case tabSearch tenv ty of
				SOME t => t
			| NONE => Error ( ErrorUndefinedType ty, pos )
			
			val { exp=expinit, ty=tyinit } = transExp ( venv, tenv, init, level )
		in
			(if weakCompTypes ( ty', tyinit ) 
			then
				let 
					val access = allocLocal level (!escape)
					val venv = tabRInsert venv ( name, VarEntry { access=access, ty=ty' } )
				in
					{ venv=venv, tenv=tenv, decs=varDec (access, expinit) :: decs }
				end
			else Error ( ErrorVarDecTypeMismatch name, pos ))
			handle InternalError msg => Error ( ErrorInternalError msg, pos )
	  end
	
	| transDec ( venv, tenv, FunctionDec lfuncs, level, decs ) =
		let 
      val batchFunEnv = tabNueva();
			
      fun trdec1 ({ name, params, result, body }, pos) =
				let
					val	fenv = tabNueva ()

					fun chkParam { name = argname, escape, typ } =
						case tabSearch tenv typ of
						SOME t => 
							(case tabInsert fenv (argname, 0) of
					    	SOME _ => Error ( ErrorDupFieldInFunDec (name, argname), pos)
    				   | NONE => (t, !escape))
						| NONE => Error ( ErrorUndefinedType typ, pos )

					val (formals, escapes) = ListPair.unzip (List.map chkParam params)
					
					val result = case result of
						SOME res => (
							case tabSearch tenv res of
								SOME t => t
							|	NONE => Error ( ErrorUndefinedType res, pos ) )
					| NONE => UNIT
					
					val funlevel = newLevel(level, name, escapes)
				in
					case tabInsert batchFunEnv (name, 0) of
						SOME _ => Error ( ErrorFunAlreadyDeclared name, pos )
					| NONE => (tabRInsert venv 
							( name, FunEntry { level=funlevel, 
																 label=getLevelLabel funlevel,
																 formals=formals, 
																 result=result, 
																 ext=false } ); ())
				end
				
			fun trdec2 ({ name, params, result, body }, pos) =
				let				
					fun insertField env ({ name=fname, escape, typ }, access) =
						case tabSearch tenv typ of
				    	SOME ty => ( tabRInsert env (fname, VarEntry{ access=access, ty=ty }); () )
				    | NONE => Error ( ErrorInternalError "problemas con Semant.transDec.trdec2.insertField!", pos )
					
					(* Pattern Matching exhaustivo de val (FunEntry r) = valOf( tabSearch venv name ) *)
					val r = case tabSearch venv name of
							SOME (FunEntry r') => r'
						| _ => Error (ErrorInternalError "problemas con Seman.transDec.trdec2->r!", pos)
					
					val venv' = fromTable venv
					val _ = List.app (insertField venv') (ListPair.zip (params, formals(#level r)))
						
					val { exp=expbody, ty=tybody } = transExp (venv', tenv, body, #level(r))
					
					val tyres = case tabSearch venv name of
						    SOME (FunEntry { level, label, formals, result, ext }) => result
							| _ => Error ( ErrorInternalError "problemas con Semant.transDec.trdec2!", pos )
				in
					(if weakCompTypes (tyres, tybody) then (procEntryExit (#level(r), expbody))
					else Error ( ErrorFunDecTypeMismatch name, pos ))
					handle InternalError msg => Error ( ErrorInternalError msg, pos )
				end
		in
			List.app trdec1 lfuncs;
			List.app trdec2 lfuncs;
			{ venv=venv, tenv=tenv, decs=decs }
		end
		
	| transDec ( venv, tenv, TypeDec ltdecs, level, decs ) = 
		let 
			val tenv' = tabNueva()
			
			fun fillTable ({name, ty}, pos) =
				let 
					val dec = (name, transTy (tenv, ty)) 
						handle DupFieldInRecordDec field => Error ( ErrorDupFieldInRecordDec field, pos)
				in 
					case tabInsert tenv' dec of
						SOME _ => Error (ErrorTypeAlreadyDeclared name, pos)
						| NONE => (tabRInsert tenv dec; ())
				end
					
			fun aux n name (graph, list) =
				case tabSearch tenv' n of
					SOME (RECORD _) => (graph, name :: list)
					| _ => ((n, name) :: graph, listRemoveItem (fn x => x = n) list)
			
			fun genFields name ((_, NAME (n, _)), gl) = aux n name gl	
				| genFields _ _ = Error (ErrorInternalError "problemas con Semant.transDec.genFields!", 0)
					
			fun genGraph ((name, ty), (graph, list)) =
				case ty of
					NAME (n,_) => aux n name (graph, list)
				| ARRAY (NAME (n,_) , _) => aux n name (graph, list)
				| RECORD ( ml, _) => List.foldl (genFields name) (graph, list) ml
				| _ => Error (ErrorInternalError "problemas con Semant.transDec.genGraph!", 0)
			
			fun firstPass (name, nextPassList) =
				let
					fun typeField (f as (name, NAME (n,_)), (ml,count)) =
						(case tabSearch tenv n of
							SOME (ARRAY (NAME _, _)) => (f::ml, count+1)
						| SOME (RECORD _) => (f::ml, count+1)
                        | SOME (NAME _) => (f::ml, count+1)
						| SOME ty => ((name, ty)::ml, count)
						| NONE => raise UndefinedType n)
					| typeField _ = Error (ErrorInternalError "problemas con Semant.transDec.firstPass.typeField!", 0)
				in 
					case valOf (tabSearch tenv name) of
						NAME (n,_) => (
							case tabSearch tenv n of
								SOME (RECORD _) => name :: nextPassList
							| SOME (ty as NAME _) => (tabRInsert tenv (name, ty); name :: nextPassList)
							| SOME ty => (tabRInsert tenv (name, ty); nextPassList)
							| NONE => raise UndefinedType n)
					| ARRAY (NAME (n,_),uniq) => (
							case tabSearch tenv n of
								SOME (RECORD _) => name :: nextPassList
							| SOME ty => (tabRInsert tenv (name, ARRAY (ty,uniq)); nextPassList)
							| NONE => raise UndefinedType n)
					| RECORD (ml,uniq) => (
							case List.foldr typeField ([], 0) ml of 
								(ml,count) => 
									(tabRInsert tenv (name, RECORD (ml, uniq)); 
									if 0 = count then nextPassList else name :: nextPassList))
					| ty => (tabRInsert tenv (name, ty); nextPassList)
				end 
				handle e =>
					let
						val n = case e of UndefinedType n => n | _ => name
						val pos = #2(valOf (List.find (fn (x,y) => #name(x) = name) ltdecs)) handle _ => 0
					in 
						Error ( ErrorUndefinedType n, pos ) 
					end
				
			fun secondPass (name, tenv) = 
				let
					fun getType n = valOf (tabSearch tenv n)
								handle _ => Error ( ErrorInternalError "problemas en Semant.transDec.secondPass.getType!", 0)
								
					fun typeField (name, ty) =
						case ty of
							NAME (n, tyr) => tyr := SOME (getType n)
						|	_ => ()
				in (
					case valOf (tabSearch tenv name) of
						NAME (n,_) => ( tabRInsert tenv (name, getType n); tenv )
					| ARRAY (NAME (n,_), uniq) => ( tabRInsert tenv (name, ARRAY (getType n, uniq)); tenv )
					| RECORD (ml,_) => ( List.app typeField ml; tenv )
					| _ => Error ( ErrorInternalError "problemas en Semant.transDec.secondPass!", 0)) 
					handle _ => tenv	
				end	
		in 
			List.app fillTable ltdecs;
			let val (graph, list) = List.foldl genGraph ([],[]) (tabAList tenv') in
			case cyclesort (graph) of
				(torder, []) => List.foldr secondPass tenv (List.foldl firstPass [] torder @ list)
			| (l1, name::l2) => 
				let val (_,pos) = valOf (List.find (fn (x,y) => #name(x) = name) ltdecs) 
				in Error (ErrorRecursiveTypeDeclaration, pos) end
			end;
			{ venv=venv, tenv=tenv, decs=decs } 
		end
		
	and transTy (tenv, NameTy ty) =
		NAME ( ty, ref NONE )
		
	| transTy (tenv, RecordTy flist) =
			let
				val menv = tabNueva()
				fun aux { name, escape, typ } = 
					let val field = (name, transTy (tenv, NameTy typ)) in
					case tabInsert menv field of
						SOME _ => raise DupFieldInRecordDec name
					| NONE => field
					end
			in
				RECORD (List.map aux flist, ref ()) 
			end
	
	| transTy (tenv, ArrayTy ty) = ARRAY ( transTy (tenv, NameTy ty), ref ())
	
	fun transProg prog = 
		let 
			val {exp=body, ty} = transExp ( TigerEnv.venv, TigerEnv.tenv, prog, outermost)
			val _ = procEntryExit (outermost, body)
		in
			getResult()
		end
end
