ifndef RUST_PATH
$(info Set $$RUST_PATH=<rust tree> to be able to link to rust std libs)
$(info $$RUST_PATH is also required to generate HTML for the rust libs)
	RF_LIBS = 
else
	RF_LIBS = -L $(RUST_PATH)/x86_64-unknown-linux-gnu/stage2/lib/rustlib/x86_64-unknown-linux-gnu/lib
endif

RF_OPTS = $(RF_LIBS) -o html/
SRC=$(wildcard *.rs)
RUST_FIND=$(shell pwd)/rustfind
RUSTSRC=$(RUST_PATH)/src
RUSTFLAGS = --opt-level=3 -A non-camel-case-types

# generate HTML browser for the main sourcetree
html: rustfind
	./rustfind rustfind.rs $(RF_LIBS) -x $(RUSTSRC)/html -o html/

test_dump: rustfind
	./rustfind  -d $(RF_OPTS) test_input.rs

#default behaviour, dump json map of spans..
test_json: rustfind
	./rustfind -j $(RF_OPTS) test_input.rs
interactive: rustfind
	./rustfind -i $(RF_OPTS) test_input.rs

#run this tool on test sources
test1 : rustfind
	./rustfind test_input.rs -j $(RF_LIBS) -o html/
	./rustfind test_input.rs $(RF_OPTS)
	#firefox html/test_input.rs.html

test0 : rustfind
	./rustfind test_input0.rs -j -x $(RUSTSRC) $(RF_OPTS)
	./rustfind test_input0.rs -w -x $(RUSTSRC) $(RF_OPTS)
	firefox html/test_input0.rs.html

#make emacs ctags for this project
tags:
	ctags -e -f TAGS.emacs --options=$(RUSTSRC)/etc/ctags.rust -R .

# Make the HTML view of the main rust sourcetree
ifdef RUST_PATH
-include $(RUST_PATH)/mk/crates.mk
else
CRATES := 
endif

define RUST_TARGET_LIB
rust_lib$(1): rustfind rust_lib_pre $$(patsubst %,rust_lib%,$$(filter-out native:%,$$(DEPS_$(1))))
	@echo "Generating HTML for lib$(1)"
	@export CFG_VERSION=0; export CFG_PREFIX=0;export CFG_RUSTLIBDIR=0;export CFG_COMPILER=0;export CFG_LIBDIR_RELATIVE=0; \
		$(RUST_FIND) $(RUSTSRC)/lib$(1)/lib.rs $(RF_LIBS) -o $(RUSTSRC)/html/lib$(1) -x $(RUSTSRC)
rust_lib$(1)_nodeps: rustfind rust_lib_pre
	@echo "Generating HTML for lib$(1)"
	@export CFG_VERSION=0; export CFG_PREFIX=0;export CFG_RUSTLIBDIR=0;export CFG_COMPILER=0;export CFG_LIBDIR_RELATIVE=0; \
		$(RUST_FIND) $(RUSTSRC)/lib$(1)/lib.rs $(RF_LIBS) -o $(RUSTSRC)/html/lib$(1) -x $(RUSTSRC)
endef

$(foreach crate,$(CRATES),$(eval $(call RUST_TARGET_LIB,$(crate))))

rust_src: rustfind rust_lib_pre $(patsubst %,rust_lib%,$(filter-out native:%,$(CRATES)))

rust_lib_pre: rustfind
	@echo "==============================================================="
	@echo "= Generating HTML for main rust sourcetree                    ="
	@echo "= Be patient, sorry this is unoptimized and will take a while ="
	@echo "==============================================================="

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
