OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
INCLUDES=
OCAMLFLAGS=$(INCLUDES) -rectypes
PROGRAM=shiny
TESTS=

# The list of object files for 
CMXS=types.cmx consts.cmx math.cmx pp.cmx shaders.cmx trace.cmx draw.cmx int.cmx
CMOS=types.cmo consts.cmo math.cmo pp.cmo shaders.cmo trace.cmo draw.cmo int.cmo

all: opt
again: clean all
com: $(CMOS) $(PROGRAM).cmo
	$(OCAMLC) $(OCAMLFLAGS) -o $(PROGRAM) graphics.cma $(CMOS) $(PROGRAM).cmo
opt: $(CMXS) $(PROGRAM).cmx
	$(OCAMLOPT) $(OCAMLFLAGS) -o $(PROGRAM) graphics.cmxa $(CMXS) $(PROGRAM).cmx
dbg: $(CMOS) $(PROGRAM).cmo
	$(OCAMLC) -g $(OCAMLFLAGS) -o $(PROGRAM) graphics.cma $(CMOS) $(PROGRAM).cmo
test: $(CMOS) $(TESTS)
	$(OCAMLC) -g $(OCAMLFLAGS) -o test graphics.cma $(CMOS) $(TESTS)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -g $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) -g $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

# Clean up
clean:
	rm -f $(PROGRAM) test
	rm -f *.cm[iox] *.o

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
