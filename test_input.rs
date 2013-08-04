use std::*;
use test_input2::*;
mod test_input2;


fn main() {	
	use test_input2::fruit::*;	
	io::println(foo_bar_test_func(SomeStruct{red_value:1,green_value:2,blue_value:3},(4,5)).to_str());
	let a=Foo(0);
	a.my_method(1);
	let c=Foo(0);
	let d=Foo(*a+*c);
	println(d.to_str());
}

struct Foo(int);
struct Bar(int);
struct Baz(int);

impl Foo {
	fn my_method(&self,i:int){ print("my_method of foo");}
}

