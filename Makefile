ifndef RUST_PATH
$(info Set $$RUST_PATH=<rust tree> to be able to link to rust std libs)
$(info $$RUST_PATH is also required to generate HTML for the rust libs)
	RF_LIBS = 
else
	RF_LIBS = -L $(RUST_PATH)/x86_64-unknown-linux-gnu/stage2/lib/rustlib/x86_64-unknown-linux-gnu/lib
endif

RF_OPTS = $(RF_LIBS) -o html/ -C
SRC=$(wildcard *.rs)
RUST_FIND=$(shell pwd)/rustfind
RUSTSRC=$(RUST_PATH)/src
RUSTFLAGS = --opt-level=3 -A non-camel-case-types

# generate HTML browser for the main sourcetree
html: html_sub callgraph tags

html_sub: rustfind
	./rustfind rustfind.rs $(RF_OPTS) -x $(RUSTSRC) 

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

#make emacs ctags for this project,
tags:
	echo $(RUSTSRC)
	ctags -e -f TAGS.emacs --options=$(RUSTSRC)/etc/ctags.rust -R .

callgraph : html_sub
	neato -Tcmap html/callgraph.dot -o html/callgraph_cmap.html
	neato -Tpng html/callgraph.dot -o html/callgraph.png
	fdp -Tpng html/callgraph.dot -o html/callgraph2.png
	cat resources/callgraph_header.html html/callgraph_cmap.html resources/callgraph_footer.html > html/callgraph.html

# Make the HTML view of the main rust sourcetree
ifdef RUST_PATH
-include $(RUST_PATH)/mk/crates.mk
else
CRATES := 
endif

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

rust_lib_pre: rustfind
	@echo "========================================================================"
	@echo "= Generating HTML for main rust sourcetree                             ="
	@echo "= Be patient, sorry this is unoptimized and will take a very long time ="
	@echo "========================================================================"
	@echo $(CRATES)

# compile the main rust sourcetree html, (compiler plus libraries)
# brute force because the above wasn't working, we have extra extra options for rustc aswell.
# any suggestions on how to make the makefile better welcome.
# 'rust_src:' is a smarter target to do the same thing, but i couldn't get it to work
rust: rust_lib_pre
	@if [ ! -f /usr/local/bin/rustfind ];then  echo "run make install first"; /usr/local/bin/rustfind; fi ; 

	cd $(RUST_PATH)/src;pwd;

	cd $(RUST_PATH)/src;pwd; rustfind libstd/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind libgreen/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind librustuv/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind libnative/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind libflate/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind libarena/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind libglob/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind libterm/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind libsemver/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind libuuid/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind libserialize/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind libsync/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind libgetopts/lib.rs

	cd $(RUST_PATH)/src;pwd; rustfind libcollections/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind libnum/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind libtest/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind libtime/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind librand/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind libworkcache/lib.rs
#	cd $(RUST_PATH)/src;pwd; ~/rustfind/rustfind libextra/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind liburl/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind liblog/lib.rs

	cd $(RUST_PATH)/src;pwd; rustfind libsyntax/lib.rs
	export CFG_VERSION=0;export CFG_COMPILER=0;export CFG_PREFIX=0;export CFG_LIBDIR_RELATIVE=0; export CFG_RUSTLIBDIR=0;export CFG_COMPILER_TRIPLE=0;cd $(RUST_SRC);pwd; ~/rustfind/rustfind librustc/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind librustdoc/lib.rs

	cd $(RUST_PATH)/src;pwd; rustfind libfourcc/lib.rs
	cd $(RUST_PATH)/src;pwd; rustfind libhexfloat/lib.rs

help:
	@echo "rustfind makefile:"
	@echo " "
	@echo "make rustfind - compile the rustfind tool"
	@echo "make install - copy to /usr/local/bin"
	@echo "make rust - run rustfind on the main rust sourcetree"
	@echo "make html - run rustfind on its own sourcetree"
	@echo " "
	@echo "once compiled, you can invoke 'rustfind <cratefile.rs>' to build linked htmlview"
	@echo "but its more useful if you build the main rust sourcetree first for reference"

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
