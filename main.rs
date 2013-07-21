extern mod syntax;
extern mod rustc;

extern mod extra;

use rustc::{front, metadata, driver, middle};

use syntax::parse;
use syntax::ast;
use syntax::ast_map;

use std::os;
use std::local_data;
use extra::json::ToJson;

macro_rules! logi{ 
	()=>(println(file!()+":"+line!().to_str()+": "));
	($a:expr)=>(println(file!()+":"+line!().to_str()+": "+$a.to_str()));
	($a:expr,$b:expr)=>(println(file!()+":"+line!().to_str()+": "+$a.to_str()+$b.to_str()));
	($a:expr,$b:expr,$c:expr)=>(println(file!()+":"+line!().to_str()+": "+$a.to_str+$b.to_str(),$c.to_str()))

}


pub static ctxtkey: local_data::Key<@DocContext> = &local_data::Key;



struct DocContext {
    crate: @ast::crate,
    tycx: middle::ty::ctxt,
    sess: driver::session::Session
}

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

trait MyToStr {
	fn myToStr(&self)->~str;
}
impl<A:MyToStr,B:MyToStr> MyToStr for (A,B) {
	fn myToStr(&self)->~str{ let &(ref a, ref b)=self; ~"("+a.myToStr()+~","+b.myToStr()+~")"}
}
impl MyToStr for ast::mutability {
	fn myToStr(&self)->~str{
		match(self) {
			&m_mutbl=>~"m_mutbl",
//			m_imm=>~"m_imm",
//			m_const=>~"m_const",
		}
	}
}
/*
impl<'self> MyToStr for &ast::mutability<'self> {
	fn myToStr(&'self self)->~str{
		match(self) {
			&m_mutbl=>~"m_mutbl",
//			m_imm=>~"m_imm",
//			m_const=>~"m_const",
		}
	}
}
*/

impl MyToStr for int {
	fn myToStr(&self)->~str{self.to_str()}
}
impl MyToStr for ast::expr {
	fn myToStr(&self)->~str { 
		"{"+"id:"+self.id.myToStr()+",node:"+self.node.myToStr()+",span:"+self.span.myToStr()+"}"
	}
}
impl<T:MyToStr> MyToStr for Option<T> {
	fn myToStr(&self)->~str{
		match(self) {
			&Some(ref x)=>~"Some("+x.myToStr()+")",
			&None=>~"None"
		}
	}
}
impl MyToStr for syntax::codemap::BytePos {
	fn myToStr(&self)->~str {
		let &syntax::codemap::BytePos(x)=self; x.to_str()
	}
}
impl MyToStr for @syntax::codemap::ExpnInfo {
	fn myToStr(&self)->~str{~"ExpnInfo{TODO}"}
}
impl MyToStr for syntax::codemap::ExpnInfo {
	fn myToStr(&self)->~str{~"ExpnInfo{TODO}"}
}

fn fMyToStr(x:&syntax::codemap::ExpnInfo)->~str {
	x.myToStr()
}

impl MyToStr for @ast::expr {
	fn myToStr(&self)->~str {
		~"{"+~"id:"+self.id.to_str()+~",node:"+self.node.myToStr()+~",span"+self.span.myToStr()+~"}"
	}
}

impl MyToStr for syntax::codemap::span {
	fn myToStr(&self)->~str {
		~"{"+~"lo:"+self.lo.myToStr()+~",hi:"+self.hi.myToStr()+~",expn_info:"+self.expn_info.myToStr()+~"}"
	}
}

impl MyToStr for ast::item_ {
	fn myToStr(&self)->~str{
		use syntax::ast::*;
		match (self) {
			&item_static(ref t,ref m,ref e)=> ~"item_static"+(*m,*e).myToStr()+~")",
			&item_fn(ref decl, ref purity,ref AbiSet, ref Generics, ref refblk)=>
				~"item_fn()",
			&item_mod(ref m) => ~"item_mod()",
			&item_foreign_mod(ref m) => ~"item_foreign_mod()",
			&item_ty(ref _ty,ref g) => ~"item_ty()",
			&item_enum(ref e,ref g) => ~"item_enum()",
			&item_struct(ref sd,ref g) => ~"item_struct()",
			&item_trait(ref g, ref trs,ref tms)=>~"item_trait",
			&item_impl(ref g, ref opt_tr, ref ty, ref ms)=>~"item_impl",
			&item_mac(ref m)=>~"item_mac()",

		}
	}
}
impl MyToStr for ast::expr_ {
	fn myToStr(&self)->~str {
		match(self) {
			_ =>~"expr_(TODO)" // all the complexity of statements is handled here
		}
	}
}

fn my_visit(c:&ast::crate){

	for c.node.module.items.iter().advance|x|{
		println(x.node.myToStr());		
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

    let ctxt = @get_ast_and_resolve(&Path(matches.free[0]), libs);
    debug!("defmap:");
    for ctxt.tycx.def_map.iter().advance |(k, v)| {
        debug!("%?: %?", k, v);
    }
    local_data::set(ctxtkey, ctxt);
//	println(ctxt.to_str());
	my_visit(ctxt.crate);
/*
    let mut v = @mut RustdocVisitor::new();
    v.visit(ctxt.crate);

    let mut crate = v.clean();
    println(crate.to_json().to_str());
*/
}
