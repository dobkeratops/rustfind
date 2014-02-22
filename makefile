RF_LIBS= -L $(RUST_PATH)/x86_64-unknown-linux-gnu/stage2/lib
OPTS= test_input.rs $(RF_LIBS)
SRC=$(wildcard *.rs)
RUSTFIND=$(pwd)/rustfind
RUSTSRC=$(RUST_PATH)/src
RUSTFLAGS = -O

# generate HTML browser for the main sourcetree
html: rustfind
	@echo "(set RUSTSRC=<rust tree> & do 'make rustsrc' to generate html for rust stdlibs/compiler)"
	@echo "generting HTML view of this sourcetree .."
	./rustfind rustfind.rs $(RF_LIBS) -x $(RUSTSRC)

test1 : rustfind
	@if [ ! $(RUST) ] ; then echo "set RUST to point to root of rust sourcetree" ; fi
	echo $(RUST)

	./rustfind  -d $(OPTS)
#default behaviour, dump json map of spans..
test2: rustfind
	./rustfind -j $(OPTS)
interactive: rustfind
	./rustfind -i $(OPTS)

#run this tool on test sources
test1 : rustfind
	@if [ ! $(RUST) ] ; then echo "set RUST to point to root of rust sourcetree" ; fi
	echo $(RUST)
	./rustfind test_input.rs -j $(RF_LIBS)
	./rustfind test_input.rs -w $(RF_LIBS)
	firefox test_input.rs.html

test0 : rustfind
	@if [ ! $(RUST) ] ; then echo "set RUST to point to root of rust sourcetree" ; fi
	echo $(RUST)
	./rustfind test_input0.rs -j -x $(RUSTSRC) $(RF_LIBS)
	./rustfind test_input0.rs -w -x $(RUSTSRC) $(RF_LIBS)
	firefox test_input0.rs.html

#make emacs ctags for this project
tags:
	ctags -e -f TAGS.emacs --options=$(RUSTSRC)/etc/ctags.rust -R .

# Make the HTML view of the main rust sourcetree
rustsrc: rustfind
	cp ./rustfind ~/bin
	@echo "generating HTML for main rust sourcetree "
	@echo "be patient, sorry this is unoptimized and will take a few mins"
	cd $(RUSTSRC);pwd; rustfind libstd/std.rs
	cd $(RUSTSRC);pwd;  rustfind libextra/extra.rs
	cd $(RUSTSRC);pwd; rustfind libsyntax/syntax.rs
	export CFG_VERSION=0;export CFG_PREFIX=0;export CFG_LIBDIR=0;export CFG_COMPILER_TRIPLE=0;cd $(RUSTSRC);pwd; rustfind librustc/rustc.rs
	firefox $(RUSTSRC)/libstd/iterator.rs.html
	#todo - make rustfind copy this! or at least make a decent copy script. or make the html ref same.
	#?? find /some/tree -type d -exec echo cp /your/file '{}'/ \;
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

