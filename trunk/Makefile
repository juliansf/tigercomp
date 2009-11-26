# Unix makefile for tigermain
TIGERHOME=/usr/share/tiger

HOME=/usr/share
MOSMLHOME=${HOME}/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac -v

GCC=gcc
CFLAGS= -g
MOSMLC=mosmlc -c -liberal
MOSMLL=mosmlc
TOPLEVEL= -toplevel

# Unix
REMOVE=rm -f
CD=cd
MOVE=mv
EXEFILE=

# DOS
#REMOVE=del
#CD=cd
#MOVE=move
#EXEFILE=.exe

# Tiger Tree
ROOT=.
LEXER=./Lexer
PARSER=Parser
SEMANTIC=Semantic
CANON=Canonizer
CODEGEN=CodeGen
LIVE=Liveness
COLOR=Color
MISC=Misc
BIN=bin
LIB=runtime

EXPORTBIN="# Tiger Compiler\nexport PATH=$(TIGERHOME)/bin:$PATH"

LOADPATH=-I $(LEXER) -I $(PARSER) -I $(MISC) -I $(SEMANTIC) -I $(CANON) -I $(CODEGEN) -I $(LIVE) -I $(COLOR)

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

GRALOBJS= \
	TigerLineNumber.uo \
	TigerProgName.uo \
	TigerAbs.uo \
	TigerTypes.uo \
	TigerError.uo \
	TigerSet.uo \
	TigerStack.uo \
	tigertab.uo \
	TigerUtils.uo \
	Parser.uo \
	Scanner.uo \
	tigerescap.uo \
	TigerTemp.uo \
	TigerGraph.uo \
	TigerTree.uo \
	TigerAssem.uo \
	TigerFrame.uo \
	TigerTranslate.uo \
	TigerEnv.uo \
	TigerSemant.uo \
	TigerCanon.uo \
	TigerCodeGen.uo \
	TigerFlow.uo \
	TigerMakeGraph.uo \
	TigerLiveness.uo \
	TigerColor.uo \
	TigerRegAlloc.uo \
	tigerpp.uo \
	tigermain.uo

all: tiger

install: 
	@if test -f $(BIN)/tiger$(EXEFILE); then \
		mkdir -p $(TIGERHOME) $(TIGERHOME)/bin $(TIGERHOME)/lib; \
		cp $(BIN)/tiger$(EXEFILE) $(TIGERHOME)/bin/; \
		cp $(LIB)/runtime.c $(TIGERHOME)/lib/; \
		if test -f ~/.profile; then \
			echo $(EXPORTBIN) >> ~/.profile; \
		elif test -f ~/.bashrc; then \
			echo $(EXPORTBIN) >> ~/.bashrc; \
	      	else \
			echo "Agrega $(TIGERHOME)/bin a tu path!"; \
		fi; \
		echo "Instalacion finalizada!"; \
	else "Debes hacer primero \"make depend; make\"."; \
	fi;

tiger: modrtpath $(GRALOBJS) $(OBJSGEN)
	if ! test -d $(BIN); then mkdir $(BIN); fi; \
	$(MOSMLL) $(LOADPATH) -o $(BIN)/tiger$(EXEFILE) tigermain.uo; \
	mv tigermain.sml.bak tigermain.sml;

modrtpath: 
	cp tigermain.sml tigermain.sml.bak; \
	sed -i '' -e 's;RUNTIMEPATH;$(TIGERHOME)/lib;g;' tigermain.sml;

Parser.sml Parser.sig: $(PARSER)/Parser.y 
	$(MOSMLYACC) $(PARSER)/Parser.y

Scanner.sml: $(LEXER)/Scanner.lex
	$(MOSMLLEX) $(LEXER)/Scanner.lex

TigerAbs.sml: $(PARSER)/tigerabs.sml

TigerLineNumber.sml: $(MISC)/TigerLineNumber.sml

tigerpp.sml: $(MISC)/tigerpp.sml

test: 
	$(MOSMLC) $(LOADPATH) test.sml
	if ! test -d $(BIN); then mkdir $(BIN); fi; \
	$(MOSMLL) $(LOADPATH) -o $(BIN)/test$(EXEFILE) test.uo

clean:
	$(REMOVE) Makefile.bak

	$(CD) $(PARSER);\
	$(REMOVE) Parser.output;\
	$(REMOVE) Parser.sig;\
	$(REMOVE) Parser.sml;\
	$(REMOVE) *.ui;\
	$(REMOVE) *.uo

	$(CD) $(LEXER);\
	$(REMOVE) Scanner.sml;\
	$(REMOVE) *.ui;\
	$(REMOVE) *.uo

	$(CD) $(MISC);\
	$(REMOVE) *.ui;\
	$(REMOVE) *.uo

	$(CD) $(SEMANTIC);\
	$(REMOVE) *.ui;\
	$(REMOVE) *.uo

	$(CD) $(CANON);\
	$(REMOVE) *.ui;\
	$(REMOVE) *.uo

	$(CD) $(CODEGEN);\
	$(REMOVE) *.ui;\
	$(REMOVE) *.uo

	$(CD) $(LIVE);\
	$(REMOVE) *.ui;\
	$(REMOVE) *.uo

	$(CD) $(COLOR);\
	$(REMOVE) *.ui;\
	$(REMOVE) *.uo

	$(REMOVE) tigermain
	$(REMOVE) *.ui
	$(REMOVE) *.uo
	$(REMOVE) errlist
	$(REMOVE) *.o

.sig.ui:
	$(MOSMLC) $(LOADPATH) $<

TigerLineNumber.uo:
	$(MOSMLC) $(LOADPATH) $(MISC)/TigerLineNumber.sml
TigerProgName.uo:
	$(MOSMLC) $(LOSDPATH) $(MISC)/TigerProgName.sml
TigerAbs.uo:
	$(MOSMLC) $(LOADPATH) $(PARSER)/TigerAbs.sml
TigerTypes.uo:
	$(MOSMLC) $(LOADPATH) $(SEMANTIC)/TigerTypes.sml
TigerError.uo:
	$(MOSMLC) $(LOADPATH) $(MISC)/TigerError.sml
TigerUtils.uo:
	$(MOSMLC) $(LOADPATH) $(MISC)/TigerUtils.sml	
TigerSet.uo: 
	$(MOSMLC) $(LOADPATH) $(MISC)/TigerSet.sig $(MISC)/TigerSet.sml
TigerStack.uo: 
	$(MOSMLC) $(LOADPATH) $(MISC)/TigerStack.sig $(MISC)/TigerStack.sml
Parser.uo: 
	$(MOSMLC) $(LOADPATH) $(PARSER)/Parser.sig $(PARSER)/Parser.sml
Scanner.uo: 
	$(MOSMLC) $(LOADPATH) $(LEXER)/Scanner.sml
tigermain.uo:
	$(MOSMLC) $(LOADPATH) tigermain.sml
tigertab.uo:
	$(MOSMLC) $(LOADPATH) $(MISC)/tigertab.sig $(MISC)/tigertab.sml
tigerescap.uo:
	$(MOSMLC) $(LOADPATH) $(MISC)/tigerescap.sig $(MISC)/tigerescap.sml
TigerTemp.uo:
	$(MOSMLC) $(LOADPATH) $(SEMANTIC)/TigerTemp.sig $(SEMANTIC)/TigerTemp.sml
TigerGraph.uo:
	$(MOSMLC) $(LOADPATH) $(MISC)/TigerGraph.sig $(MISC)/TigerGraph.sml
TigerTree.uo:
	$(MOSMLC) $(LOADPATH) $(SEMANTIC)/TigerTree.sml
TigerAssem.uo:
	$(MOSMLC) $(LOADPATH) $(CODEGEN)/TigerAssem.sig $(CODEGEN)/TigerAssem.sml
TigerFrame.uo:
	$(MOSMLC) $(LOADPATH) $(SEMANTIC)/TigerFrame.sig $(SEMANTIC)/TigerFrame.sml	
TigerTranslate.uo:
	$(MOSMLC) $(LOADPATH) $(SEMANTIC)/TigerTranslate.sig $(SEMANTIC)/TigerTranslate.sml
TigerEnv.uo:
	$(MOSMLC) $(LOADPATH) $(SEMANTIC)/TigerEnv.sml
TigerSemant.uo:
	$(MOSMLC) $(LOADPATH) $(SEMANTIC)/TigerSemant.sig $(SEMANTIC)/TigerSemant.sml
TigerCanon.uo:
	$(MOSMLC) $(LOADPATH) $(CANON)/TigerCanon.sig $(CANON)/TigerCanon.sml
TigerCodeGen.uo:
	$(MOSMLC) $(LOADPATH) $(CODEGEN)/TigerPPCGen.sig $(CODEGEN)/TigerPPCGen.sml \
						  $(CODEGEN)/TigerCodeGen.sig $(CODEGEN)/TigerCodeGen.sml
TigerFlow.uo:
	$(MOSMLC) $(LOADPATH) $(LIVE)/TigerFlow.sml
TigerMakeGraph.uo:
	$(MOSMLC) $(LOADPATH) $(LIVE)/TigerMakeGraph.sig $(LIVE)/TigerMakeGraph.sml
TigerLiveness.uo:
	$(MOSMLC) $(LOADPATH) $(LIVE)/TigerLiveness.sig $(LIVE)/TigerLiveness.sml
TigerColor.uo: 
	$(MOSMLC) $(LOADPATH) $(COLOR)/TigerColor.sig $(COLOR)/TigerColor.sml
TigerRegAlloc.uo: 
	$(MOSMLC) $(LOADPATH) $(COLOR)/TigerRegAlloc.sig $(COLOR)/TigerRegAlloc.sml

tigerpp.uo:
	$(MOSMLC) $(LOADPATH) $(MISC)/tigerpp.sml

#.sml.uo:
#	$(MOSMLC) $<

depend: Parser.sml Scanner.sml

### DO NOT DELETE THIS LINE
