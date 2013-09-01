use syntax::ast;
use std::hashmap::*;
/*new file*/  

pub type ZeroBasedIndex=uint;

/// cross crate map, extra info written out when compiling links of a crate
/// allows sunsequent crates to jump to definitions in that crate
/// TODO - check if this already exists in the 'cstore/create metadata'
/// specificially we need node->span info
#[deriving(Clone)]
pub struct CrossCrateMapItem {
	fname:~str,
	line:ZeroBasedIndex,	
	col:uint,
	len:uint
}

pub type CrossCrateMap = HashMap<ast::def_id,CrossCrateMapItem>;

