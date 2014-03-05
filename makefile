RF_LIBS= -L $(RUST_PATH)/x86_64-unknown-linux-gnu/stage2/lib/rustlib/x86_64-unknown-linux-gnu/lib
RF_OPTS= $(RF_LIBS) -o html/
SRC=$(wildcard *.rs)
RUSTFIND=$(shell pwd)/rustfind
RUSTSRC=$(RUST_PATH)/src/
RUSTFLAGS = --opt-level=3 -A non-camel-case-types

# generate HTML browser for the main sourcetree
html: rustfind
	@echo "(set \$$RUST_PATH=<rust tree> & do 'make rustsrc' to generate html for rust stdlibs/compiler)"
	@echo "generating HTML view of this sourcetree .."
	./rustfind rustfind.rs $(RF_LIBS) -x $(RUSTSRC) -o html/

test_dump: rustfind
	@if [ ! $(RUST_PATH) ] ; then echo "set \$$RUST_PATH to point to root of rust sourcetree" ; fi
	echo $(RUST_PATH)

	./rustfind  -d $(RF_OPTS) test_input.rs
#default behaviour, dump json map of spans..
test_json: rustfind
	./rustfind -j $(RF_OPTS) test_input.rs
interactive: rustfind
	./rustfind -i $(RF_OPTS) test_input.rs

#run this tool on test sources
test1 : rustfind
	@if [ ! $(RUST_PATH) ] ; then echo "set \$$RUST_PATH to point to root of rust sourcetree" ; fi
	echo $(RUST_PATH)
	#./rustfind test_input.rs -j $(RF_LIBS) -o html/
	./rustfind test_input.rs $(RF_OPTS)
	#firefox html/test_input.rs.html

test0 : rustfind
	@if [ ! $(RUST_PATH) ] ; then echo "set \$$RUST_PATH to point to root of rust sourcetree" ; fi
	echo $(RUST_PATH)
	./rustfind test_input0.rs -j -x $(RUSTSRC) $(RF_OPTS)
	./rustfind test_input0.rs -w -x $(RUSTSRC) $(RF_OPTS)
	firefox html/test_input0.rs.html

#make emacs ctags for this project
tags:
	ctags -e -f TAGS.emacs --options=$(RUSTSRC)/etc/ctags.rust -R .

# Make the HTML view of the main rust sourcetree
include $(RUST_PATH)/mk/crates.mk

define RUST_TARGET_LIB
rust_lib$(1): rustfind rust_lib_pre $$(patsubst %,rust_lib%,$$(filter-out native:%,$$(DEPS_$(1))))
	@echo "Generating HTML for lib$(1)"
	@export CFG_VERSION=0; export CFG_PREFIX=0;export CFG_RUSTLIBDIR=0;export CF_COMPILER=0;export CG_LIBDIR_RELATIVE=0; \
		$(RUSTFIND) $(RUSTSRC)/lib$(1)/lib.rs $(RF_LIBS) -o $(RUSTSRC)/html -x $(RUSTSRC)
rust_lib$(1)_nodeps: rustfind rust_lib_pre
	@echo "Generating HTML for lib$(1)"
	@export CFG_VERSION=0; export CFG_PREFIX=0;export CFG_RUSTLIBDIR=0;export CF_COMPILER=0;export CG_LIBDIR_RELATIVE=0; \
		$(RUSTFIND) $(RUSTSRC)/lib$(1)/lib.rs $(RF_LIBS) -o $(RUSTSRC)/html -x $(RUSTSRC)
endef

$(foreach crate,$(CRATES),$(eval $(call RUST_TARGET_LIB,$(crate))))

rust_src: rustfind rust_lib_pre $(patsubst %,rust_lib%,$(filter-out native:%,$(CRATES)))
	
rust_lib_pre:
	@echo "=================================================================="
	@echo "= Generating HTML for main rust sourcetree                       ="
	@echo "= Be patient, sorry this is unoptimized and will take a few mins ="
	@echo "=================================================================="

rust_libstd_old: rustfind
	#firefox $(RUSTSRC)/libstd/iterator.rs.html
	#todo - make rustfind copy this! or at least make a decent copy script. or make the html ref same.
	#?? find /some/tree -type d -exec echo cp /your/file '{}'/ \;

copy:
	cp sourcestyle.css $(RUSTSRC)/libstd
	cp sourcestyle.css $(RUSTSRC)/libstd/rand
	cp sourcestyle.css $(RUSTSRC)/libstd/task
	cp sourcestyle.css $(RUSTSRC)/libstd/fmt
	cp sourcestyle.css $(RUSTSRC)/libstd/num
	cp sourcestyle.css $(RUSTSRC)/libstd/rt
	cp sourcestyle.css $(RUSTSRC)/libstd/unstable
	cp sourcestyle.css $(RUSTSRC)/libstd/str
	cp sourcestyle.css $(RUSTSRC)/libsyntax
	cp sourcestyle.css $(RUSTSRC)/libsyntax/ext
	cp sourcestyle.css $(RUSTSRC)/libsyntax/parse
	cp sourcestyle.css $(RUSTSRC)/libsyntax/print
	cp sourcestyle.css $(RUSTSRC)/libsyntax/util
	cp sourcestyle.css $(RUSTSRC)/libsyntax/ext
	cp sourcestyle.css $(RUSTSRC)/libextra
	cp sourcestyle.css $(RUSTSRC)/libextra/num
	cp sourcestyle.css $(RUSTSRC)/libextra/terminfo
	cp sourcestyle.css $(RUSTSRC)/libextra/crypto
	cp sourcestyle.css $(RUSTSRC)/librustc
	cp sourcestyle.css $(RUSTSRC)/librustc/metadata
	cp sourcestyle.css $(RUSTSRC)/librustc/back
	cp sourcestyle.css $(RUSTSRC)/librustc/front
	cp sourcestyle.css $(RUSTSRC)/librustc/middle
	cp sourcestyle.css $(RUSTSRC)/librustc/driver
	cp sourcestyle.css $(RUSTSRC)/librustc/lib
	cp sourcestyle.css $(RUSTSRC)/librustc/util

#Compile the main executable
rustfind: rustfind.rs $(SRC) 
	rustc $(RUSTFLAGS) rustfind.rs

install :rustfind
	cp ./rustfind /usr/local/bin

clean:
	rm -f rustfind
	rm -f *.html
	rm -f *.*~
	rm -f html/*.html
