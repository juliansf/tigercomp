open Scanner
open Parser
open TigerProgName
open TigerLineNumber
open BasicIO Nonstdio

fun lexstream(is: instream) =
	Lexing.createLexer(fn b => fn n => buff_input is b 0 n);

fun parseError e lexbuf= 
		case e of
		  Fail s => raise Fail s
		|	Match => Error ( ErrorParsingError (Lexing.getLexeme lexbuf), Line() )
		| Parsing.ParseError _ => Error ( ErrorParsingError (Lexing.getLexeme lexbuf), Line() )
		| e => raise e

fun main(tigername, args) =
	let	
		val outputName = ref ""
		val arbol = ref false
		val arbol_ir = ref false
		val arbol_canon = ref false
		val code_list = ref false
		val asm_only = ref false
		val output = ref false
		val inputs = ref []
		
		fun option (arg, rel) =
			case arg of
				"-arbol" => (arbol := true; false)
			|	"-irtree" => (arbol_ir := true; false)
			| "-canontree" => (arbol_canon := true; false)
			| "-codelist" => (code_list := true; false)
			| "-S" => (asm_only := true; false)
			| "-o" => (output := true; true)
			| arg => 
					(if String.isPrefix "-" arg 
					then TigerError.ErrorUnrecognizedOption tigername arg
					else if !output then outputName := arg else inputs := arg :: (!inputs); false)
		
		fun parseInput file =
			let 
				val input = open_in file 
					handle _ => raise ErrorFileNotFound file
				
				val lexbuf = lexstream input
			in
				progName := file;
				prog Token lexbuf
					handle e => parseError e lexbuf
			end
			
		(* Parseamos los argumentos *)
		val _ = List.foldl option false args
		
		(* Parseamos las entradas y construimos el AST *)
		val expr = 
			case !inputs of
				[] => raise ErrorNoInputFiles	tigername
			| [file] => parseInput file
			| files =>  raise Fail (tigername ^ ": lo siento, actualmente no soporto multiples fuentes :(.\n")
		
		(* Code Generation *)
		fun codegen {body, frame} = 
					(TigerFrame.procEntryExit2 
						(frame, List.concat (List.map (fn s => TigerCodeGen.codegen frame s) body)), frame)
		(* Code Generation *)
		
		(* Register Allocation *)
		fun regalloc (l,f) =
			let
				val (l', ralloc) = TigerRegAlloc.alloc (l,f)
				fun ralloc' t = TigerTemp.tempname(valOf(tigertab.tabSearch ralloc t))
			in
				(TigerFrame.procEntryExit3 (f, l'), f, ralloc')
			end
		(* Register Allocation *)
		
		(* Assembler *)
		fun get_asm ({prolog, body, epilogue}, f, ra) =
			let 
				val lab = TigerFrame.getFrameLabel f
				val fs = TigerFrame.frameSize f 
				val asm = prolog 
								^ (List.foldr (fn (x,y) => x^y) epilogue (List.map (TigerAssem.format ra lab fs) body))
			in
				asm
			end
		(* Assembler *)
		
		(* Escapes *)	
		val _ = tigerescap.findEscape expr
		
		(* AST *)
		val _ = if !arbol then tigerpp.exprAst expr else ()
		
		(* Intermediate Representation *)
		val lfrag = TigerSemant.transProg expr
		
		(* Canonized Code *)
		val lfrag2 = TigerCanon.canonize lfrag
		
		(* Code Generation *)
		val literals = List.mapPartial (fn frag => 
					case frag of TigerCanon.LITERAL l => SOME l | _ => NONE) lfrag2
		
		val fragments = List.mapPartial (fn frag => 
					case frag of TigerCanon.FUNC f => SOME f | _ => NONE) lfrag2
		
		val lfrag3 = List.map codegen fragments
		
		(* Register Allocation *)
		val lfrag4 = List.map regalloc lfrag3
		
		(* Assembler *)								
		val (init, data, text) = TigerCodeGen.sections
		val asm = init
						^ data
						^ TigerCodeGen.literals literals
						^ text
						^ List.foldr (fn (x, y) => get_asm(x)^y) "" lfrag4
		
		
	in	
		if !arbol_ir then 
			(print "\n::: IR TREE :::\n"; List.app tigerpp.ppfrag lfrag) else ();
		if !arbol_canon then 
			(print "\n::: CANONICAL TREE :::\n"; List.app tigerpp.ppCanonFrag lfrag2) else ();
		if !code_list then 
			(print "\n::: CODE LIST :::\n";
			List.app (fn (l,f) => 
				let val lab = TigerFrame.getFrameLabel f; val fs = TigerFrame.frameSize f in
					List.app (print o (TigerAssem.format TigerTemp.tempname lab fs)) l 
				end) lfrag3) else ();
		if !asm_only then
			let
				val name = if !output andalso !outputName <> "" then !outputName else
					let
						val toks = String.tokens (fn x => x = #".") (hd (!inputs))
						val toks' = String.tokens (fn x => x = #"/")
					in
						(if List.last toks = "tig" 
						then List.last (toks' (hd toks)) 
						else List.last (toks' (hd (!inputs)))) ^ ".s"
					end
				val file = TextIO.openOut(name)
			in
				TextIO.output(file, asm);
				TextIO.flushOut file;
				TextIO.closeOut file
			end 
		else 
			let
				val name = FileSys.tmpName() ^ ".s"
				val file = TextIO.openOut(name)
				val out = if !output andalso !outputName <> "" then " -o " ^ !outputName else ""
			in
				TextIO.output(file, asm);
				TextIO.flushOut file;
				Process.system( "gcc -arch ppc -g /usr/share/tiger/lib/runtime.c " ^ name ^ out );
				TextIO.closeOut file
			end
	end	handle e => ShowErrors e

val _ = main( CommandLine.name(), CommandLine.arguments() )
