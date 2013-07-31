use std::*;
use test_input2::*;
use test_input2::fruit;
mod test_input2;


fn main() {	
	use test_input2::fruit::*;	
	io::println(foo_bar_test_func(SomeStruct{red_value:1,green_value:2,blue_value:3},(4,5)).to_str());
	let a=Foo(0);
	let c=Foo(0);
	let d=Foo(1);
	let e=Foo(2);
	let f=Foo(2);
	let g=Foo(2);
	let i=Foo(2);
	let j=Foo(2);
	let k=Foo(2);
	let l=Foo(2);
}


struct Foo(int);
struct Bar(int);
struct Baz(int);



