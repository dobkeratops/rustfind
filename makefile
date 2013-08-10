RF_LIBS= -L $(RUST)/x86_64-unknown-linux-gnu/stage2/lib
OPTS= test_input.rs $(RF_LIBS)
html: rustfind
	./rustfind rustfind.rs $(RF_LIBS)
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

test : rustfind
	@if [ ! $(RUST) ] ; then echo "set RUST to point to root of rust sourcetree" ; fi
	echo $(RUST)
	./rustfind test_input.rs -j $(RF_LIBS)
	./rustfind test_input.rs -w $(RF_LIBS)
	firefox test_input.rs.html

SRC=$(wildcard *.rs)

rustfind: rustfind.rs $(SRC)
	rustc rustfind.rs

clean:
	rm *~
	rm *.*~
	rm *.html
	rm rustfind

