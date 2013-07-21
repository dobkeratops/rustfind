test : rustfind
	@if [ ! $(RUST) ] ; then echo "set RUST to point to root of rust sourcetree" ; fi

	./main test_input.rs -L $(RUST)/x86_64-unknown-linux-gnu/stage2/lib

rustfind: main.rs rustfind.rs astdump.rs
	rustc main.rs


