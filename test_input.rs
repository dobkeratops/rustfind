use std::*;

struct SomeStruct{
	red_value:int,green_value:int,blue_value:int
}

fn foo(mut apples:SomeStruct,oranges:int)->int{
	let some_very_long_variable_name=2*oranges;
	some_very_long_variable_name-apples.red_value
}

fn main() {	
	io::println(foo(SomeStruct{red_value:1,green_value:2,blue_value:3},4).to_str());
}
