use std::*;
use test_input2::*;
mod test_input2;


fn main() {	
	use test_input2::fruit::*;	
	io::println(foo_bar_test_func(SomeStruct{red_value:1,green_value:2,blue_value:3},(4,5)).to_str());
	let a=Foo{foo_field_1:2};
	a.my_method(1);
	let c=Bar(3);
	let d=Foo{foo_field_1:a.foo_field_1+*c};
	println(a.foo_field_1.to_str());
}

struct Foo{foo_field_1:int}
struct Bar(int);
struct Baz(int);
impl Foo {
	fn my_method(&self,_:int){ print("my_method of foo");}
}
