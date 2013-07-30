test1 : rustfind
	@if [ ! $(RUST) ] ; then echo "set RUST to point to root of rust sourcetree" ; fi
	echo $(RUST)

	./rustfind test_input.rs -d -L $(RUST)/x86_64-unknown-linux-gnu/stage2/lib

#default behaviour, dump json map of spans..
test2: rustfind
	./rustfind test_input.rs -L $(RUST)/x86_64-unknown-linux-gnu/stage2/lib

rustfind: rustfind.rs find_ast_node.rs astdump.rs text_formatting.rs 
	rustc rustfind.rs


