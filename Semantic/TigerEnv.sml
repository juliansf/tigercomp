structure TigerEnv =
struct
	open tigertab
	open TigerTypes
	
	datatype enventry =
		VarEntry of { access:TigerTranslate.access, ty:ty }
	| FunEntry of { level:TigerTranslate.level, label:TigerTemp.label, formals:ty list, result:ty, ext:bool }
	
	val tenv = tigertab.tabNueva()
	val venv = tigertab.tabNueva()
	
	val tenv = tabRInsert tenv ( "int", INT RW )
	val tenv = tabRInsert tenv ( "string", STRING )
	
	val outer = TigerTranslate.outermost
	val label = TigerTemp.namedlabel
	
	val venv = tabRInsert venv ( "print", FunEntry { level=outer, label=label("_print"), formals=[STRING], result=UNIT, ext=true } )
	val venv = tabRInsert venv ( "print_int", FunEntry { level=outer, label=label("_print_int"), formals=[INT RW], result=UNIT, ext=true } )
	val venv = tabRInsert venv ( "flush", FunEntry { level=outer, label=label("_flush"), formals=[], result=UNIT, ext=true } )
	val venv = tabRInsert venv ( "getchar", FunEntry { level=outer, label=label("_get_char"), formals=[], result=STRING, ext=true } )
	val venv = tabRInsert venv ( "ord", FunEntry { level=outer, label=label("_ord"), formals=[STRING], result=INT RO, ext=true } )
	val venv = tabRInsert venv ( "chr", FunEntry { level=outer, label=label("_chr"), formals=[INT RW], result=STRING, ext=true } )
	val venv = tabRInsert venv ( "size", FunEntry { level=outer, label=label("_size"), formals=[STRING], result=INT RO, ext=true } )
	val venv = tabRInsert venv ( "substring", FunEntry { level=outer, label=label("_substring"), formals=[STRING, INT RW, INT RW], result=STRING, ext=true } )
	val venv = tabRInsert venv ( "concat", FunEntry { level=outer, label=label("_concat"), formals=[STRING, STRING], result=STRING, ext=true } )
	val venv = tabRInsert venv ( "not", FunEntry { level=outer, label=label("_not"), formals=[INT RW], result=INT RO, ext=true } )
	val venv = tabRInsert venv ( "exit", FunEntry { level=outer, label=label("_exit_now"), formals=[INT RW], result=UNIT, ext=true } )
end