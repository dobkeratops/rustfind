
extern mod syntax;
extern mod rustc;
extern mod extra;
use rustc::{front, metadata, driver, middle};
use rustc::middle::*;

use syntax::parse;
use syntax::ast;
use syntax::ast_map;
use syntax::visit;
use syntax::visit::*;
use syntax::visit::{Visitor, fn_kind};
use find_ast_node::*;
use text_formatting::*;

use syntax::abi::AbiSet;
use syntax::ast;
use syntax::codemap::span;

use std::os;
use std::local_data;
use extra::json::ToJson;

mod text_formatting;
mod find_ast_node;

pub static ctxtkey: local_data::Key<@DocContext> = &local_data::Key;


pub macro_rules! logi{ 
	($($a:expr),*)=>(println((file!()+":"+line!().to_str()+": " $(+$a.to_str())*) .indent(2,160)))
}
//macro_rules! dump{ ($a:expr)=>(logi!(fmt!("%s=%?",stringify!($a),$a).indent(2,160));)}
macro_rules! dump{ ($($a:expr),*)=>
	(	{	let mut txt=~""; 
			$( { txt=txt.append(
				 fmt!("%s=%?",stringify!($a),$a)+",") 
				}
			);*; 
			logi!(txt); 
		}
	)
}

pub fn safe_node_id_to_type(cx: ty::ctxt, id: ast::node_id) -> Option<ty::t> {
    //io::println(fmt!("%?/%?", id, cx.node_types.len()));
    match cx.node_types.find(&(id as uint)) {
       Some(&t) => Some(t),
       None => None    }
}


/// tags: crate,ast,parse resolve
/// Parses, resolves the given crate
fn get_ast_and_resolve(cpath: &Path, libs: ~[Path]) -> DocContext {

    let parsesess = parse::new_parse_sess(None);
    let sessopts = @driver::session::options {
        binary: @"rustdoc",
        maybe_sysroot: Some(@std::os::self_exe_path().get().pop()),
        addl_lib_search_paths: @mut libs,
        .. copy (*rustc::driver::session::basic_options())
    };

    let diagnostic_handler = syntax::diagnostic::mk_handler(None);
    let span_diagnostic_handler =
        syntax::diagnostic::mk_span_handler(diagnostic_handler, parsesess.cm);

    let mut sess = driver::driver::build_session_(sessopts, parsesess.cm,
                                                  syntax::diagnostic::emit,
                                                  span_diagnostic_handler);

    let (crate, tycx) = driver::driver::compile_upto(sess, sessopts.cfg.clone(),
                                                     &driver::driver::file_input(cpath.clone()),
                                                     driver::driver::cu_no_trans, None);
                                                     
	let c=crate.unwrap();
	let t=tycx.unwrap();
    DocContext { crate: c, tycx: t, sess: sess }
}

enum Object {
	Ship{pos:(float,float),vel:(float,float),hdg:float},
	Bullet{pos:(float,float),vel:(float,float)}
}

fn main() {
    use extra::getopts::*;
    use std::hashmap::HashMap;

    let args = os::args();

    let opts = ~[
        optmulti("L")
    ];
	let o1=~Ship{pos:(0.0,0.0),vel:(0.0,0.0),hdg:0.0};
	let o2=~Ship{pos:(0.0,0.0),vel:(0.0,0.0),hdg:0.0};
	match (*o1,*o2) {
		(Ship{pos:p1,vel:v1,hdg:h1},Ship{vel:v2,_}) =>{
		},
		(_,_)=>{
		}
	}


    let matches = getopts(args.tail(), opts).get();
    let libs = opt_strs(&matches, "L").map(|s| Path(*s));
	dump!(args,matches);
	dump!(libs);
    let dc = @get_ast_and_resolve(&Path(matches.free[0]), libs);

	// TODO: parse commandline source locations,convert to codemap locations
	//dump!(ctxt.tycx);

	logi!("==== dump def table.===")
	dump_ctxt_def_map(dc);

    local_data::set(ctxtkey, dc);
	logi!("")
	logi!("==== Test node search by location...===")
 
	let mut pos=15 as uint;
	while pos<250 {
		// get the AST node under 'pos', and dump info 
		logi!(~"Find AST node at:",pos)
		let node = find_ast_node::find(dc.crate,pos);
		let node_info =  find_ast_node::get_node_info_str(dc,node);
		dump!(node_info);
		// TODO - get infered type from ctxt.node_types??
		// node_id = get_node_id()
		// node_type=ctxt.node_types./*node_type_table*/.get...
		let nid=node.last().get_node_id() ;
		if nid!=0 {
			match(safe_node_id_to_type(dc.tycx, nid)) {
				Some(t)=>
					println(fmt!("typeinfo: %?",{let ntt= rustc::middle::ty::get(t); ntt})),
				None=> logi!("typeinfo:unknown node_type for ",nid)
			}
		} else {logi!("typeinfo:-unknown node id")}
		dump!(nid,dc.tycx.def_map.find(&nid));
		

		pos+=12;
	}
}


// see: tycx.node_types:node_type_table:HashMap<id,t>
// 't'=opaque ptr, ty::get(:t)->t_box_ to resolve it

fn dump_ctxt_def_map(dc:&DocContext) {
//	let a:()=ctxt.tycx.node_types
	logi!("===Test ctxt def-map table..===");
	for dc.tycx.def_map.iter().advance |(key,value)|{
		dump!(key,value);
	}
}

