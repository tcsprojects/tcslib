ifeq ($(strip $(wildcard Config)),)
	include Config.default
else
	include Config
endif

INCLUDES=-I $(SRCDIR) -I $(OBJDIR) -I $(OCAML_DIR)


ifeq "$(COMPILE_WITH_OPT)" "YES"

COMPILEEXT=cmx
COMPILELIBEXT=cmxa
OCAMLCOMP=$(OCAMLOPT)

else

COMPILEEXT=cmo
COMPILELIBEXT=cma
OCAMLCOMP=$(OCAMLC)

endif


MODULES_DATA=$(OBJDIR)/tcsbasedata.$(COMPILEEXT) \
	$(OBJDIR)/tcsmaths.$(COMPILEEXT) \
	$(OBJDIR)/tcsarray.$(COMPILEEXT) \
	$(OBJDIR)/tcslist.$(COMPILEEXT) \
	$(OBJDIR)/tcsset.$(COMPILEEXT) \
	$(OBJDIR)/tcscache.$(COMPILEEXT) \
	$(OBJDIR)/tcsqueue.$(COMPILEEXT) \
	$(OBJDIR)/tcsstrings.$(COMPILEEXT) \
	$(OBJDIR)/tcsgraph.$(COMPILEEXT)

MODULES_UTILS=$(OBJDIR)/tcstiming.$(COMPILEEXT) \
	$(OBJDIR)/tcsstats.$(COMPILEEXT) \
	$(OBJDIR)/tcsargs.$(COMPILEEXT) \
	$(OBJDIR)/tcsmessage.$(COMPILEEXT)

MODULES_AUTOMATA=$(OBJDIR)/tcsautomata.$(COMPILEEXT) \
	$(OBJDIR)/tcsautotransform.$(COMPILEEXT) \
	$(OBJDIR)/tcsautohelper.$(COMPILEEXT) \
	$(OBJDIR)/tcsgames.$(COMPILEEXT) \
	$(OBJDIR)/tcstransitionsys.$(COMPILEEXT) \
	$(OBJDIR)/tcsparitygameparser.$(COMPILEEXT) \
	$(OBJDIR)/tcsparitysolutionparser.$(COMPILEEXT) \
	$(OBJDIR)/tcsltsparser.$(COMPILEEXT) \
	$(OBJDIR)/tcstsparser.$(COMPILEEXT) \
	$(OBJDIR)/tcsautoparser.$(COMPILEEXT) \
	$(OBJDIR)/tcsautomataparser.$(COMPILEEXT) \
	$(OBJDIR)/tcsgameparser.$(COMPILEEXT) \
	$(OBJDIR)/tcstransitionsysparser.$(COMPILEEXT)

MODULES_INTF=$(MODULES_DATA) \
	$(MODULES_UTILS) \
	$(MODULES_AUTOMATA)

PREMODULES=$(OBJDIR)/tcsautomataparserinternal.$(COMPILEEXT) \
           $(OBJDIR)/tcsgameparserinternal.$(COMPILEEXT) \
		   $(OBJDIR)/tcstransitionsysparserinternal.$(COMPILEEXT) \
		   $(OBJDIR)/tcsautomatalexer.$(COMPILEEXT) \
		   $(OBJDIR)/tcsparitygamelexer.$(COMPILEEXT) \
		   $(OBJDIR)/tcsparitysolutionlexer.$(COMPILEEXT) \
		   $(OBJDIR)/tcsltslexer.$(COMPILEEXT) \
		   $(OBJDIR)/tcstslexer.$(COMPILEEXT)

PREINTF=$(OBJDIR)/tcsautomataparserinternal.cmi \
        $(OBJDIR)/tcsgameparserinternal.cmi \
		$(OBJDIR)/tcstransitionsysparserinternal.cmi

MODULES=$(PREMODULES) $(MODULES_INTF)

INTERFACES=$(MODULES_INTF:.$(COMPILEEXT)=.cmi)

all: modules library

modules: $(PREINTF) $(INTERFACES) $(PREMODULES) $(MODULES)

LIBRARYNAME=$(OBJDIR)/tcslib.$(COMPILELIBEXT)

library: modules libexec

libexec:
	$(OCAMLCOMP) -a -o $(LIBRARYNAME) $(CPPCOMPILER) $(MODULES)

$(OBJDIR)/%.$(COMPILEEXT): $(SRCDIR)/data/%.ml
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/data/%.mli
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.$(COMPILEEXT): $(SRCDIR)/automata/%.ml
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/automata/%.mli
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.$(COMPILEEXT): $(SRCDIR)/automata/parser/%.ml
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/automata/parser/%.mli
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.$(COMPILEEXT): $(SRCDIR)/utils/%.ml
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/utils/%.mli
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(SRCDIR)/automata/parser/tcsparitygameparser.mli: $(SRCDIR)/automata/parser/tcsparitygameparser.mly
	$(OCAMLYACC) $(SRCDIR)/automata/parser/tcsparitygameparser.mly

$(SRCDIR)/automata/parser/tcsparitygameparser.ml: $(SRCDIR)/automata/parser/tcsparitygameparser.mly
	$(OCAMLYACC) $(SRCDIR)/automata/parser/tcsparitygameparser.mly

$(SRCDIR)/automata/parser/tcsparitygamelexer.ml: $(SRCDIR)/automata/parser/tcsparitygamelexer.mll
	$(OCAMLLEX) $(SRCDIR)/automata/parser/tcsparitygamelexer.mll

$(SRCDIR)/automata/parser/tcsautoparser.mli: $(SRCDIR)/automata/parser/tcsautoparser.mly
	$(OCAMLYACC) $(SRCDIR)/automata/parser/tcsautoparser.mly

$(SRCDIR)/automata/parser/tcsautoparser.ml: $(SRCDIR)/automata/parser/tcsautoparser.mly
	$(OCAMLYACC) $(SRCDIR)/automata/parser/tcsautoparser.mly

$(SRCDIR)/automata/parser/tcsautomatalexer.ml: $(SRCDIR)/automata/parser/tcsautomatalexer.mll
	$(OCAMLLEX) $(SRCDIR)/automata/parser/tcsautomatalexer.mll

$(SRCDIR)/automata/parser/tcsparitysolutionparser.mli: $(SRCDIR)/automata/parser/tcsparitysolutionparser.mly
	$(OCAMLYACC) $(SRCDIR)/automata/parser/tcsparitysolutionparser.mly

$(SRCDIR)/automata/parser/tcsparitysolutionparser.ml: $(SRCDIR)/automata/parser/tcsparitysolutionparser.mly
	$(OCAMLYACC) $(SRCDIR)/automata/parser/tcsparitysolutionparser.mly

$(SRCDIR)/automata/parser/tcsparitysolutionlexer.ml: $(SRCDIR)/automata/parser/tcsparitysolutionlexer.mll
	$(OCAMLLEX) $(SRCDIR)/automata/parser/tcsparitysolutionlexer.mll

$(SRCDIR)/automata/parser/tcsltsparser.mli: $(SRCDIR)/automata/parser/tcsltsparser.mly
	$(OCAMLYACC) $(SRCDIR)/automata/parser/tcsltsparser.mly

$(SRCDIR)/automata/parser/tcsltsparser.ml: $(SRCDIR)/automata/parser/tcsltsparser.mly
	$(OCAMLYACC) $(SRCDIR)/automata/parser/tcsltsparser.mly

$(SRCDIR)/automata/parser/tcsltslexer.ml: $(SRCDIR)/automata/parser/tcsltslexer.mll
	$(OCAMLLEX) $(SRCDIR)/automata/parser/tcsltslexer.mll

$(SRCDIR)/automata/parser/tcstsparser.mli: $(SRCDIR)/automata/parser/tcstsparser.mly
	$(OCAMLYACC) $(SRCDIR)/automata/parser/tcstsparser.mly

$(SRCDIR)/automata/parser/tcstsparser.ml: $(SRCDIR)/automata/parser/tcstsparser.mly
	$(OCAMLYACC) $(SRCDIR)/automata/parser/tcstsparser.mly

$(SRCDIR)/automata/parser/tcstslexer.ml: $(SRCDIR)/automata/parser/tcstslexer.mll
	$(OCAMLLEX) $(SRCDIR)/automata/parser/tcstslexer.mll

clean:
	rm -f $(OBJDIR)/*.o \
	      $(OBJDIR)/*.cm* \
		  $(SRCDIR)/automata/parser/tcsautoparser.ml \
		  $(SRCDIR)/automata/parser/tcsautoparser.mli \
		  $(SRCDIR)/automata/parser/tcsautomatalexer.ml \
		  $(SRCDIR)/automata/parser/tcsparitygameparser.ml \
		  $(SRCDIR)/automata/parser/tcsparitygameparser.mli \
		  $(SRCDIR)/automata/parser/tcsparitygamelexer.ml \
		  $(SRCDIR)/automata/parser/tcsparitysolutionparser.ml \
		  $(SRCDIR)/automata/parser/tcsparitysolutionparser.mli \
		  $(SRCDIR)/automata/parser/tcsparitysolutionlexer.ml \
		  $(SRCDIR)/automata/parser/tcsltsparser.ml \
		  $(SRCDIR)/automata/parser/tcsltsparser.mli \
		  $(SRCDIR)/automata/parser/tcsltslexer.ml \
		  $(SRCDIR)/automata/parser/tcstsparser.ml \
		  $(SRCDIR)/automata/parser/tcstsparser.mli \
		  $(SRCDIR)/automata/parser/tcstslexer.ml
