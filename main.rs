
extern mod syntax;
extern mod rustc;
extern mod extra;
use rustc::{front, metadata, driver, middle};

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


struct DocContext {
    crate: @ast::crate,
    tycx: middle::ty::ctxt,
    sess: driver::session::Session
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
                                                     driver::driver::cu_typeck, None);
                                                     
	let c=crate.unwrap();
	let t=tycx.unwrap();
    DocContext { crate: c, tycx: t, sess: sess }
}
struct Foo {
	i:int,j:int
}

// CALLING C++ FROM RUST
// =====================
//
// manually rolled vtables for C++.
// For each virtual method, create a pointer member.
// create wrapper functions that invoke the vtable and 
//

fn main() {
    use extra::getopts::*;
    use std::hashmap::HashMap;

    let args = os::args();

    let opts = ~[
        optmulti("L")
    ];


    let matches = getopts(args.tail(), opts).get();
    let libs = opt_strs(&matches, "L").map(|s| Path(*s));
	dump!(args,matches);
	dump!(libs);
    let ctxt = @get_ast_and_resolve(&Path(matches.free[0]), libs);

	// TODO: parse commandline source locations,convert to codemap locations
	//dump!(ctxt.tycx);

    local_data::set(ctxtkey, ctxt);
	logi!("")
	logi!("==== Test node search by location...===")
	dump!(find_ast_node::find(ctxt.crate,25))
	dump!(find_ast_node::find(ctxt.crate,97))
 
	let mut pos=15;
	while pos<200 {
		let node = find_ast_node::find(ctxt.crate,pos);

		// recover name, we already have this, but lets verify
		// recover source location from the span
		//dump!(node.last());
		dump!(pos);
		let node_info=
		match node.last() {
			&astnode_view_item(x)=>~"view_item: ?",
			&astnode_item(x)=>~"item: "+ctxt.sess.str_of(x.ident)+
				match x.node {
					ast::item_fn(ref fn_decl,_,_,_,_) =>~" fn_decl",
					ast::item_struct(ref struct_def,_) =>~" struct_def",
					_=>~"item_unknown"
				},

			&astnode_local(x)=>~"local: ?",
			&astnode_block(x)=>~"block: ?",
			&astnode_stmt(x)=>~"stmt: ?",
			&astnode_arm(x)=>~"arm: ?",
			&astnode_struct_field(sf)=>
				match(sf.node.kind){
					ast::named_field(nf,vis)=>"struct named_field: "+ctxt.sess.str_of(nf)+" ",
					_=>~"struct anon_field"
				},
			&astnode_pat(x)=>~"pattern: ?",
			&astnode_decl(x)=>~"decl: ?",
			&astnode_ty(x)=>~"type: "+
				match x.node{
					ast::ty_nil=> ~"nil",
					ast::ty_bot=>~"bottomtype",
					ast::ty_box(ref mt)=>~"box",
					ast::ty_vec(ref mt)=>~"vec",
					ast::ty_fixed_length_vec(ref mt,ref expr)=>~"[T,..N]",
					ast::ty_ptr(ref mt)=>~"ptr",
					ast::ty_rptr(ref lifetime,ref mt)=>~"rptr",
					ast::ty_tup(ref types)=>~"tuple[..]", //todo: factor this out, map..
					ast::ty_path(ref path,ref params,node_id)=>~"path:",
					
					ast::ty_infer=>~"infered",
					_ =>~"?"
				}
			,
			&astnode_struct_def(sf)=>~"struct def",
			_=>	~"unknown"
		};
		dump!(node_info);
		// node_id = get_node_id()
		// node_type=ctxt.node_types./*node_type_table*/.get...

		// todo: dump type
		pos+=10;
	}
//	logi!("===Test ctxt symbol table..===")
//	for ctxt.tycx.def_map.iter().advance |(key,value)|{
//		dump!(key,value);
//	}
}
