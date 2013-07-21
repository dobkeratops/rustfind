test : rustfind
	./rustfind rustfind.rs

rustfind: main.rs rustfind.rs astdump.rs
	rustc main.rs


