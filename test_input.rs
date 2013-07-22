use std::*;

struct s{
	x:int,y:int,z:int
}

fn foo(mut apples:int,oranges:int)->int{
	apples+oranges
}

fn main() {	
	io::println(foo(2,3).to_str());
}
