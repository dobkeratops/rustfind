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
use text_formatting::*;

use syntax::abi::AbiSet;
use syntax::ast;
use syntax::codemap::span;

use std::os;
use std::local_data;
use extra::json::ToJson;

mod text_formatting;

macro_rules! logi{ 
	($($a:expr),*)=>(println((file!()+":"+line!().to_str()+": " $(+$a.to_str())*) .indent(2,160)))
}
//macro_rules! dump{ ($a:expr)=>(logi!(fmt!("%s=%?",stringify!($a),$a).indent(2,160));)}
fn newline_if_over(a:~str,l:uint)->~str{if a.len()>l {a+~"\n"}else{a}}
macro_rules! dump{ ($($a:expr),*)=>
	(	{	let mut txt=~""; 
			$( txt=txt.append(
				fmt!("%s=%?",stringify!($a),$a)+~",") 
			);*; 
			logi!(txt); 
		}
	)
}

pub static ctxtkey: local_data::Key<@DocContext> = &local_data::Key;

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


mod find_ast_node 
{
	use syntax::ast::*;
	use syntax::visit::*;
	use syntax::codemap::*;

	/// main
	pub fn find(c:@crate,_location:uint)->~[AstNode] {
		let mut s= @mut State{
			result:~[], location:_location, stop:false
		};
		let vt=mk_vt(@Visitor{
			visit_item:f_item,
			visit_struct_field:f_struct_field,
			visit_expr:f_expr,
			visit_pat:f_pat,
			visit_decl:f_decl,
			//visit_ty:f_ty,

			.. *default_visitor::<@mut State>()
			}
		);
		visit_crate(c, (s,vt));
		s.result.clone()
	}


	#[deriving(Clone)]
	pub enum AstNode
	{
		// struct Visitor<E>.visit_mod: @fn(&_mod, span, node_id, (E, vt<E>)),
		// struct Visitor<E>.visit_view_item: @fn(&view_item, (E, vt<E>)),
		astnode_view_item(@view_item),
		// struct Visitor<E>.visit_foreign_item: @fn(@foreign_item, (E, vt<E>)),
		// struct Visitor<E>.visit_item: @fn(@item, (E, vt<E>)),
		astnode_item(@item),
		// struct Visitor<E>.visit_local: @fn(@local, (E, vt<E>)),
		// struct Visitor<E>.visit_block: @fn(&blk, (E, vt<E>)),
		// struct Visitor<E>.visit_stmt: @fn(@stmt, (E, vt<E>)),
		// struct Visitor<E>.visit_arm: @fn(&arm, (E, vt<E>)),
		// struct Visitor<E>.visit_pat: @fn(@pat, (E, vt<E>)),
		astnode_pat(@pat),
		// struct Visitor<E>.visit_decl: @fn(@decl, (E, vt<E>)),
		astnode_decl(@decl),
		// struct Visitor<E>.visit_expr: @fn(@expr, (E, vt<E>)),
		astnode_expr(@expr),
		// struct Visitor<E>.visit_expr_post: @fn(@expr, (E, vt<E>)),
		astnode_expr_post(@expr),
		// struct Visitor<E>.visit_ty: @fn(&Ty, (E, vt<E>)),
		astnode_ty(@Ty),
		// struct Visitor<E>.visit_generics: @fn(&Generics, (E, vt<E>)),
		// struct Visitor<E>.visit_fn: @fn(&fn_kind, &fn_decl, &blk, span, node_id, (E, vt<E>)),
		// struct Visitor<E>.visit_ty_method: @fn(&ty_method, (E, vt<E>)),
		astnode_ty_method(@ty_method),
		// struct Visitor<E>.visit_trait_method: @fn(&trait_method, (E, vt<E>)),
		astnode_trait_method(@trait_method),
		// struct Visitor<E>.visit_struct_def: @fn(@struct_def, ident, &Generics, node_id, (E, vt<E>)),
		astnode_struct_def(@struct_def),
		// struct Visitor<E>.visit_struct_field: @fn(@struct_field, (E, vt<E>)),
		astnode_struct_field(@struct_field),

		astnode_empty	
	}

	struct State {
		result: ~[AstNode],		// todo - full tree path, all the parent nodes.
		location: uint,
		stop: bool
	}
	fn span_contains(l:uint,s:span)->bool {
		let BytePos(lo)=s.lo;
		let BytePos(hi)=s.hi;
		l>=lo && l<hi
	}

// struct Visitor<E>.visit_mod: @fn(&_mod, span, node_id, (E, vt<E>)),
// struct Visitor<E>.visit_view_item: @fn(&view_item, (E, vt<E>)),
// struct Visitor<E>.visit_foreign_item: @fn(@foreign_item, (E, vt<E>)),
// struct Visitor<E>.visit_item: @fn(@item, (E, vt<E>)),
	fn f_item(a:@item, (s,v):(@mut State, vt<@mut State>)) {
		if span_contains(s.location, a.span) {
			s.result.push(astnode_item(a));
			let found=a.span;
			dump!(found)
		}
		visit_item(a,(s,v))
	}
// struct Visitor<E>.visit_local: @fn(@local, (E, vt<E>)),
// struct Visitor<E>.visit_block: @fn(&blk, (E, vt<E>)),
// struct Visitor<E>.visit_stmt: @fn(@stmt, (E, vt<E>)),
// struct Visitor<E>.visit_arm: @fn(&arm, (E, vt<E>)),
// struct Visitor<E>.visit_pat: @fn(@pat, (E, vt<E>)),
	fn f_pat(a:@pat, (s,v):(@mut State, vt<@mut State>)) {
		if span_contains(s.location, a.span) {
			s.result.push(astnode_pat(a));
			let found=a.span;
			dump!(found)
		}
		visit_pat(a,(s,v))
	}


// struct Visitor<E>.visit_decl: @fn(@decl, (E, vt<E>)),
	fn f_decl(a:@decl, (s,v):(@mut State, vt<@mut State>)) {
		if span_contains(s.location, a.span) {
			s.result.push(astnode_decl(a));
			let found=a.span;
			dump!(found)
		}
		visit_decl(a,(s,v))
	}


// struct Visitor<E>.visit_expr: @fn(@expr, (E, vt<E>)),
	fn f_expr(a:@expr, (s,v):(@mut State, vt<@mut State>)) {
		if span_contains(s.location, a.span) {
			s.result.push(astnode_expr(a));
			let found=a.span;
			dump!(found)
		}
		visit_expr(a,(s,v))
	}

// struct Visitor<E>.visit_expr_post: @fn(@expr, (E, vt<E>)),
	fn f_expr_post(a:@expr, (s,v):(@mut State, vt<@mut State>)) {
		if span_contains(s.location, a.span) {
			s.result.push(astnode_expr_post(a));
			let found=a.span;
			dump!(found)
		}
		visit_expr(a,(s,v))
	}

// struct Visitor<E>.visit_ty: @fn(&Ty, (E, vt<E>)),
	fn f_ty(a:@Ty, (s,v):(@mut State, vt<@mut State>)) {
		if span_contains(s.location, a.span) {
			s.result.push(astnode_ty(a));
			let found=a.span;
			dump!(found)
		}
		visit_ty(a,(s,v))
	}

// struct Visitor<E>.visit_generics: @fn(&Generics, (E, vt<E>)),
// struct Visitor<E>.visit_fn: @fn(&fn_kind, &fn_decl, &blk, span, node_id, (E, vt<E>)),

// struct Visitor<E>.visit_ty_method: @fn(&ty_method, (E, vt<E>)),
// struct Visitor<E>.visit_trait_method: @fn(&trait_method, (E, vt<E>)),
// struct Visitor<E>.visit_struct_def: @fn(@struct_def, ident, &Generics, node_id, (E, vt<E>)),
// struct Visitor<E>.visit_struct_field: @fn(@struct_field, (E, vt<E>)),
	fn f_struct_field(a:@struct_field, (s,v):(@mut State, vt<@mut State>)) {
		if span_contains(s.location, a.span) {
			s.result.push(astnode_struct_field(a));
			let found=a.span;
			dump!(found)
		}
		visit_struct_field(a,(s,v))
	}

}

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

    local_data::set(ctxtkey, ctxt);
	logi!("==== Test node search by location...===")
	dump!(find_ast_node::find(ctxt.crate,25))
	dump!(find_ast_node::find(ctxt.crate,97))
	logi!("Test ctxt symbol table..")
	for ctxt.tycx.def_map.iter().advance |(key,value)|{
		dump!(key,value);
	}
}
