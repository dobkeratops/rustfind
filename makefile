RF_LIBS= -L $(RUST)/x86_64-unknown-linux-gnu/stage2/lib
OPTS= test_input.rs $(RF_LIBS)
SRC=$(wildcard *.rs)
RUSTFIND=$(pwd)/rustfind

# generate HTML browser for the main sourcetree
html: rustfind
	@echo "(set RUST_SRC=<rust tree> & do 'make rustsrc' to generate html for rust stdlibs/compiler)"
	@echo "generting HTML view of this sourcetree .."
	./rustfind rustfind.rs $(RF_LIBS) -x $(RUST_SRC)
	firefox rustfind.rs.html &

test1 : rustfind
	@if [ ! $(RUST) ] ; then echo "set RUST to point to root of rust sourcetree" ; fi
	echo $(RUST)

	./rustfind  -d $(OPTS)
#default behaviour, dump json map of spans..
test2: rustfind
	./rustfind -j $(OPTS)
int: rustfind
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
	./rustfind test_input0.rs -j -x $(RUST_SRC) $RF_LIBS
	./rustfind test_input0.rs -w -x $(RUST_SRC) $RF_LIBS
	firefox test_input0.rs.html


#make emacs ctags for this project
tags:
	ctags -e -f TAGS.emacs --options=$(RUST_SRC)/etc/ctags.rust -R .

# Make the HTML view of the main rust sourcetree
rustsrc: rustfind
	cp rustfind ~/bin
	@echo "generating HTML for main rust sourcetree "
	@echo "be patient, sorry this is unoptimized and will take a few mins"
	cd $(RUST_SRC);pwd; rustfind libstd/std.rs
	cd $(RUST_SRC);pwd; rustfind libsyntax/syntax.rs
	export CFG_VERSION=0;export CFG_PREFIX=0;export CFG_LIBDIR=0;export CFG_COMPILER_TRIPLE=0;cd $(RUST_SRC);pwd; rustfind librustc/rustc.rs
	cd $(RUST_SRC);pwd;  rustfind libextra/extra.rs
	firefox $(RUST_SRC)/libstd/iterator.rs.html
	#todo - make rustfind copy this! or at least make a decent copy script. or make the html ref same.
	#?? find /some/tree -type d -exec echo cp /your/file '{}'/ \;
	cp sourcestyle.css $(RUST_SRC)/libstd
	cp sourcestyle.css $(RUST_SRC)/libstd/rand
	cp sourcestyle.css $(RUST_SRC)/libstd/task
	cp sourcestyle.css $(RUST_SRC)/libstd/fmt
	cp sourcestyle.css $(RUST_SRC)/libstd/num
	cp sourcestyle.css $(RUST_SRC)/libstd/rt
	cp sourcestyle.css $(RUST_SRC)/libstd/unstable
	cp sourcestyle.css $(RUST_SRC)/libstd/str
	cp sourcestyle.css $(RUST_SRC)/libsyntax
	cp sourcestyle.css $(RUST_SRC)/libsyntax/ext
	cp sourcestyle.css $(RUST_SRC)/libsyntax/parse
	cp sourcestyle.css $(RUST_SRC)/libsyntax/print
	cp sourcestyle.css $(RUST_SRC)/libsyntax/util
	cp sourcestyle.css $(RUST_SRC)/libsyntax/ext
	cp sourcestyle.css $(RUST_SRC)/libextra
	cp sourcestyle.css $(RUST_SRC)/libextra/num
	cp sourcestyle.css $(RUST_SRC)/libextra/terminfo
	cp sourcestyle.css $(RUST_SRC)/libextra/crypto
	cp sourcestyle.css $(RUST_SRC)/librustc
	cp sourcestyle.css $(RUST_SRC)/librustc/metadata
	cp sourcestyle.css $(RUST_SRC)/librustc/back
	cp sourcestyle.css $(RUST_SRC)/librustc/front
	cp sourcestyle.css $(RUST_SRC)/librustc/middle
	cp sourcestyle.css $(RUST_SRC)/librustc/driver
	cp sourcestyle.css $(RUST_SRC)/librustc/lib
	cp sourcestyle.css $(RUST_SRC)/librustc/util

#Compile the main executable
rustfind: rustfind.rs $(SRC) tags
	rustc rustfind.rs

install :rustfind
	cp ./rustfind /usr/local/bin

clean:
	rm rustfind
	rm *.html
	rm *.*~

