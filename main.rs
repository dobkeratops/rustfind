
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
    let ctxt = @get_ast_and_resolve(&Path(matches.free[0]), libs);

	// TODO: parse commandline source locations,convert to codemap locations
	//dump!(ctxt.tycx);

	logi!("==== dump def table.===")
	dump_ctxt_table(ctxt);

    local_data::set(ctxtkey, ctxt);
	logi!("")
	logi!("==== Test node search by location...===")
 
	let mut pos=15 as uint;
	while pos<250 {
		// get the AST node under 'pos', and dump info 
		logi!(~"Find AST node at:",pos)
		let node = find_ast_node::find(ctxt.crate,pos);
		let node_info = get_node_info_str(ctxt,node);
		dump!(node_info);
		// TODO - get infered type from ctxt.node_types??
		// node_id = get_node_id()
		// node_type=ctxt.node_types./*node_type_table*/.get...

		pos+=12;
	}
}

fn get_node_info_str(ctxt:&DocContext,node:&[find_ast_node::AstNode])->~str
{
	fn path_to_str(ctxt:&DocContext, path:&ast::Path)->~str
	{
		let mut acc=~"";
		let mut first=true;
		for path.idents.iter().advance |x|{
			if !first {acc=acc.append(~"::");}
			acc=acc.append(ctxt.sess.str_of(*x));
			first=false
		}
		acc
		// typeparams too... path.types?
	}
	fn pat_to_str(ctxt:&DocContext,p:&ast::pat)->~str{
		// todo -factor out and recurse
		match p.node {
			ast::pat_ident(bind_mode,ref path, opt)=>~"pat_ident:"+path_to_str(ctxt,path),
			ast::pat_enum(ref path,ref efields)=>~"pat_enum:"+path_to_str(ctxt,path),//	`todo-fields..
			ast::pat_struct(ref path,ref sfields,b)=>~"pat_struct:"+path_to_str(ctxt,path)+~"{"+sfields.map(|x|pat_to_str(ctxt,x.pat)+~",").to_str()+~"}",
			ast::pat_tup(ref elems)=>~"pat_tupl:"+elems.map(|&x|pat_to_str(ctxt,x)).to_str(),
			ast::pat_box(ref box)=>~"box",
			ast::pat_uniq(ref u)=>~"uniq",
			ast::pat_region(ref p)=>~"rgn",
			ast::pat_lit(ref e)=>~"literal",
			ast::pat_range(ref e_start,ref e_end)=>~"range",
		
			_=>~"?"
		}
	};
	fn ty_to_str(ctxt:&DocContext,t:&ast::Ty)->~str{
		match t.node{
			ast::ty_nil=> ~"nil",
			ast::ty_bot=>~"bottomtype",
			ast::ty_box(ref mt)=>~"box",
			ast::ty_vec(ref mt)=>~"vec",
			ast::ty_fixed_length_vec(ref mt,ref expr)=>~"[T,..N]",
			ast::ty_ptr(ref mt)=>~"*",
			ast::ty_rptr(ref lifetime,ref mt)=>~"&",
			ast::ty_tup(ref types)=>~"("+types.map(|x|ty_to_str(ctxt,x)).to_str()+")", //todo: factor this out, map..
			ast::ty_path(ref path,ref params,node_id)=>~"path:id="+node_id.to_str()+" "+path_to_str(ctxt,path)
			,
		
			ast::ty_infer=>~"infered",
			_ =>~"?"
		}
	}

	match node.last() {
//			TODO -factor out repeatedly used functions here..
//			fn astnode_pat_to_str(&astnode_pat(x))->~str
//			fn path_to_str(&astnode_pat(x))->~str
//			fn expr_to_str(&astnode_pat(x))->~str

		&astnode_view_item(x)=>~"view_item: ?",
		&astnode_item(x)=>~"item: "+
			~"id="+x.id.to_str()+~" "+
			ctxt.sess.str_of(x.ident)+
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
			~"id="+sf.node.id.to_str()+~" "+
			match(sf.node.kind){
				ast::named_field(nf,vis)=>"struct named_field: "+ctxt.sess.str_of(nf)+" ",
				_=>~"struct anon_field"
			}+
			~":"+ty_to_str(ctxt,&sf.node.ty)/*sf.node.ty ..parse it.. */,
		&astnode_pat(p)=>~"pattern: "+
			~"id="+p.id.to_str()+~" "+
			pat_to_str(ctxt,p)
		,
		&astnode_decl(x)=>~"decl: ?",
		&astnode_ty(x)=>~"type: "+ty_to_str(ctxt,x),
		&astnode_struct_def(sf)=>~"struct def",
		_=>	~"unknown"
	}
}
// see: tycx.node_types:node_type_table:HashMap<id,t>
// 't'=opaque ptr, ty::get(:t)->t_box_ to resolve it

fn dump_ctxt_table(ctxt:&DocContext) {
//	let a:()=ctxt.tycx.node_types
	logi!("===Test ctxt def-map table..===");
	for ctxt.tycx.def_map.iter().advance |(key,value)|{
		dump!(key,value);
	}
}

