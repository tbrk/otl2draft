# $Id: Makefile 139 2008-04-28 06:31:00Z tbourke $
#
HEAP_SUFFIX=x86-bsd
MLTON=mlton
MLTONOPT=-loop-passes 1

SMLNJ_MLBUILD=ml-build
SMLNJ_HEAP2EXEC=heap2exec

SRC=	lazy_seq.sig lazyseq.sml main.sml \
	otl_reader.sig otlreader.sml  \
	otl_line.sig otl_tree.sig otllatex.sml otlline.sml \
	otltree.sml runmain.sml

otl2draft: withmlton

withsmlnj: $(SRC)
	$(SMLNJ_MLBUILD) sources.cm Main.callmain otl2draft
	$(SMLNJ_HEAP2EXEC) otl2draft.$(HEAP_SUFFIX) otl2draft
	@rm otl2draft.$(HEAP_SUFFIX)

withmlton: $(SRC)
	$(MLTON) $(MLTONOPT) -verbose 1 otl2draft.mlb
	strip otl2draft

mingw32: $(SRC)
	$(MLTON) -target mingw32 -cc mingw32-gcc \
		 $(MLTONOPT) -verbose 1 otl2draft.mlb
	mv otl2draft otl2draft.exe

tags: $(SRC)
	exctags -R --exclude=.cm --exclude=*.grm.sml *

clean:
	-@rm -r $$f/.cm

clobber: clean
	-@rm otl2draft otl2draft.exe
	-@rm tags

