RF_LIBS= -L $(RUST_PATH)/x86_64-unknown-linux-gnu/stage2/lib
OPTS= test_input.rs $(RF_LIBS) -o html/
SRC=$(wildcard *.rs)
RUSTFIND=$(shell pwd)/rustfind
RUSTSRC=$(RUST_PATH)/src
RUSTFLAGS = --opt-level=3 -A non-camel-case-types

# generate HTML browser for the main sourcetree
html: rustfind
	@echo "(set RUSTSRC=<rust tree> & do 'make rustsrc' to generate html for rust stdlibs/compiler)"
	@echo "generting HTML view of this sourcetree .."
	./rustfind rustfind.rs $(RF_LIBS) -x $(RUSTSRC) -o html/

test_dump: rustfind
	@if [ ! $(RUST) ] ; then echo "set RUST to point to root of rust sourcetree" ; fi
	echo $(RUST)

	./rustfind  -d $(OPTS)
#default behaviour, dump json map of spans..
test_json: rustfind
	./rustfind -j $(OPTS)
interactive: rustfind
	./rustfind -i $(OPTS)

#run this tool on test sources
test1 : rustfind
	@if [ ! $(RUST) ] ; then echo "set RUST to point to root of rust sourcetree" ; fi
	echo $(RUST)
	#./rustfind test_input.rs -j $(RF_LIBS) -o html/
	./rustfind test_input.rs $(RF_LIBS) -o html/
	#firefox html/test_input.rs.html

test0 : rustfind
	@if [ ! $(RUST) ] ; then echo "set RUST to point to root of rust sourcetree" ; fi
	echo $(RUST)
	./rustfind test_input0.rs -j -x $(RUSTSRC) $(RF_LIBS) -o html/
	./rustfind test_input0.rs -w -x $(RUSTSRC) $(RF_LIBS) -o html/
	firefox html/test_input0.rs.html

#make emacs ctags for this project
tags:
	ctags -e -f TAGS.emacs --options=$(RUSTSRC)/etc/ctags.rust -R .

# Make the HTML view of the main rust sourcetree
rust_src: rust_libextra rust_libsyntax rust_librustc
	@echo "generating HTML for main rust sourcetree "
	@echo "be patient, sorry this is unoptimized and will take a few mins"

rust_libstd: rustfind
	@echo "Generating HTML for rust libstd"
	@echo "Please be patient, this could take quite a long time"
	cd $(RUSTSRC);pwd; $(RUSTFIND) libstd/lib.rs $(RF_LIBS)
	#firefox $(RUSTSRC)/libstd/iterator.rs.html
	#todo - make rustfind copy this! or at least make a decent copy script. or make the html ref same.
	#?? find /some/tree -type d -exec echo cp /your/file '{}'/ \;

rust_libextra: rustfind
	@echo "Generating HTML for rust libextra"
	@echo "Please be patient, this could take quite a long time"
	cd $(RUSTSRC);pwd; $(RUSTFIND) libextra/lib.rs $(RF_LIBS)

rust_libsyntax: rustfind
	@echo "Generating HTML for rust libsyntax"
	@echo "Please be patient, this could take quite a long time"
	cd $(RUSTSRC);pwd; $(RUSTFIND) libsyntax/lib.rs $(RF_LIBS)

rust_librustc: rustfind
	@echo "Generating HTML for rust librustc"
	@echo "Please be patient, this could take quite a long time"
	export CFG_VERSION=0;export CFG_PREFIX=0;export CFG_RUSTLIBDIR=0;export CFG_COMPILER=0;export CFG_LIBDIR_RELATIVE=0;cd $(RUSTSRC);pwd; $(RUSTFIND) librustc/lib.rs $(RF_LIBS)

rust_libcollections: rustfind
	@echo "Generating HTML for rust libcollections"
	@echo "Please be patient, this could take quite a long time"
	cd $(RUSTSRC);pwd; $(RUSTFIND) libcollections/lib.rs $(RF_LIBS)

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
