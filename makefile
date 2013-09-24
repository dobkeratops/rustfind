RF_LIBS= -L $(RUST)/x86_64-unknown-linux-gnu/stage2/lib
OPTS= test_input.rs $(RF_LIBS)
html: rustfind
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

SRC=$(wildcard *.rs)

tags:
	ctags -e -f TAGS.emacs --options=$(RUST_SRC)/etc/ctags.rust -R .

rustfind: rustfind.rs $(SRC) tags
	rustc rustfind.rs

clean:
	rm rustfind
	rm *.html
	rm *.*~

