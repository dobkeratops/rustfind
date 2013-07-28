use std::*;

mod fruit {
	pub struct SomeStruct{
		red_value:int,green_value:int,blue_value:int
	}
}

fn foo(mut apples:fruit::SomeStruct,(oranges,lemon):(int,int))->int{
	let some_var_name=2*oranges;
	some_var_name-apples.red_value+lemon
}

fn main() {	
	use fruit::*;	
	io::println(foo(SomeStruct{red_value:1,green_value:2,blue_value:3},(4,5)).to_str());
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

