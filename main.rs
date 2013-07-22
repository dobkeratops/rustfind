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

use syntax::abi::AbiSet;
use syntax::ast;
use syntax::codemap::span;

use std::os;
use std::local_data;
use extra::json::ToJson;

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

/// tags: pretty print,code formatting,brace indentation, json braces, brackets,nesting
trait Indent {
	fn indent(&self,tab:int,line:int)->Self;
}

impl Indent for ~str {
	fn indent(&self,tabsize:int, linesize:int)->~str {
		// todo-write as iterators.
		fn change_indent(c:u8)->int{
			match c as char {
				'{'|'('|'['=> 1,
				'}'|')'|']'=> -1,
				_ => 0
			}
		}			
		let mut a:~str=~"";
		let mut i=0;
		let mut indent=0;
		let len=self.len();
		while i<len {
			// skip leading whitespace. we are on a newline.
			while i<len && (self[i]==' 'as u8 || self[i]=='\t'as u8) { i+=1;}
			// measure line size from here
			let mut dii=0;
			if change_indent(self[i])<0 { indent-=1;dii=1}/*TODO-more elegant*/
			let mut cur_linesize=indent*tabsize;
			let mut ii=i;
			let mut inner_brace_level=indent;
			let mut last_open=len;
			let mut first_base_delim=len;
			
			while ii<len && inner_brace_level>=indent {
				let c=self[ii];
				if c=='\n'as u8 {break};
				if cur_linesize >= linesize {break};
				cur_linesize+=1;
				let di=change_indent(c);
				if di>0 && inner_brace_level==indent {last_open=ii;};
				inner_brace_level+=di;
				if inner_brace_level==indent {
					if (di<0 ||c==','as u8||c==';'as u8){last_open=ii;};
 
					if c==','as u8||c==';'as u8 && first_base_delim==len{	
						//first_base_delim=ii;
					}
				}
				ii+=1
			}
			{
				let mut ii=0;
				while ii<tabsize*indent { a.push_char(' '); ii+=1 }
			}
			indent+=dii;	// the extra one we considered.
			let init_indent=indent;

			if cur_linesize<linesize 
			{
				// copy the line. we dont overstep
				while i<len && self[i]!='\n'as u8 && indent>=init_indent && i<=first_base_delim && i<=last_open{
					let c=self[i];
					indent+=change_indent(c);
					a.push_char(c as char);
					i+=1;
				}
				if self[i]=='\n'as u8 {ii+=1;}
			}
			else {
				// copy the line until the lines' last opening
				while i<len && indent>=init_indent && i<=last_open && i<=first_base_delim{
					let c=self[i];
					indent+=change_indent(c);
					a.push_char(c as char);
					i+=1;
				}
			}
			a.push_char('\n');
		}
		a
	}
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

impl MyToStr for syntax::ast::ident {
	fn myToStr(&self)->~str{~"ident{"+~"name:"+self.name.myToStr()+~"}"}
}
impl MyToStr for syntax::ast::Name {
	fn myToStr(&self)->~str{
		self.to_str()
	}
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
			&item_static(ref t,ref m,ref e)=> ~"item_static"+(*m,*e).myToStr(),
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
impl MyToStr for ast::item {
	fn myToStr(&self)->~str {
		~"item{"+
			~"ident:"+self.ident.myToStr()+
			~",node:"+self.node.myToStr()+
			~",span:"+self.span.myToStr()+
		~"}"
	}
}

impl MyToStr for ast::_mod{
	fn myToStr(&self)->~str{
		~"_mod{"+~"view_items:"+array_myToStr(self.view_items)+~",items"+arrayOfSharedPtr_myToStr(self.items)+~"}"		
	}
}
impl<T: MyToStr> MyToStr for syntax::codemap::spanned<T> {
	fn myToStr(&self)->~str{
		~"spanned<>{"+~"node:"+self.node.myToStr()+~",span:"+self.span.myToStr()+~"}"
	}
}
impl MyToStr for ast::crate_ {
	fn myToStr(&self)->~str{
		~"crate_{"+~"module:"+self.module.myToStr()
		+~",attrs:?,config:?"
		+~"}"
	}
}
impl MyToStr for ast::view_item {
	fn myToStr(&self)->~str {
		~"view_item{"+
		~"node:"+self.node.myToStr()+
		~",attrs:?"+
		~",vis:?"+
		~",span:"+self.span.myToStr()+
		"}"
	}
}
impl MyToStr for ast::view_item_ {
	fn myToStr(&self)->~str {
		use syntax::ast::*;
		match (self) {
		&view_item_extern_mod(ref ident, ref mis, ref nid)=>~"view_item_extern_mod()",
		&view_item_use(ref vps)=>~"view_item_use()"
		}
	}
}
/*
impl<T:MyToStr> MyToStr for ~[T]{
	fn myToStr(&self)->~str {
		self.iter().transform( |x|{ x.myToStr()}).collect()
	}
}
*/
fn arrayOfSharedPtr_myToStr<T:MyToStr>(a:&[@T])->~str {
	let mut acc:~str=~"[";
	for a.iter().advance |x| {acc=acc.append( (*x).myToStr() )+","};
	acc=acc.append(~"]");
	acc
//	a.iter().map( |x|{ x.myToStr()}).to_str()
}
fn array_myToStr<T:MyToStr>(a:&[T])->~str {
	let mut acc:~str=~"[";
	for a.iter().advance |x| {acc=acc.append( x.myToStr() )+","};
	acc=acc.append(~"]");
	acc
//	a.iter().map( |x|{ x.myToStr()}).to_str()
}

fn my_visit(c:&ast::crate){
	for c.node.module.items.iter().advance|x|{
		println(x.myToStr());		
	}
}

#[deriving(Clone)]
struct MyData {
	depth:int,
}

fn makeMyVisitor()->syntax::visit::vt<@MyData>
{
	use syntax::visit::*;
	use syntax::ast::*;
	use syntax::codemap::*;
/*	
	pub fn visit_fn(fk:&fn_kind,fd:&fn_decl,b:&blk,s:span,nid:node_id, evt:(E,vt<E>)) 
	{
		dump!(fk,fd,b,s,nid,evt);
	};
*/
 	fn my_visit_fn<E>(item: &ast::item, fd: &ast::fn_decl, purity: &ast::purity,
                     abi: &syntax::abi::AbiSet, gen: &ast::Generics, rcx: E) 
	{
		dump!(item,fd,purity,abi,gen,rcx);
	};

	fn my_visit_item(item: @ast::item, (rcx, vt): (@mut MyData, syntax::visit::vt<@mut MyData>)) 
	{
		dump!(item);
		match item.node {
	/*
		    ast::item_mod(ref m) => {
		        for m.items.iter().advance |i| { (vt.visit_item)(*i, (rcx.clone(), vt)); }
		    },
		    ast::item_enum(ref ed, ref gen) => visit_enum_def(item, ed, gen, rcx),
		    ast::item_struct(sd, ref gen) => visit_struct_def(item, sd, gen, rcx),
		    ast::item_fn(ref fd, ref pur, ref abi, ref gen, _) =>
		        visit_fn(item, fd, pur, abi, gen, rcx),
	*/            _ => (),
		}

	}

	type Vct=(@ MyData, syntax::visit::vt<@ MyData>);
	let v=@Visitor {
/*		visit_item:
		|item: @ast::item, (rcx, vt): Vct| {
			dump!();


		},
*/
		visit_fn:
		|fnk:&fn_kind, fd: &ast::fn_decl, blk:&spanned<blk_>,  sp:syntax::codemap::span,
                     nid:node_id, (rcx,vt): Vct| 
		{
			dump!(fnk,fd,sp,blk,nid);
		},


		..*visit::default_visitor::<@ MyData>()
	};
	mk_vt(v)
}

/*
pub enum item_ {
    item_static(Ty, mutability, @expr),
    item_fn(fn_decl, purity, AbiSet, Generics, blk),
    item_mod(_mod),
    item_foreign_mod(foreign_mod),
    item_ty(Ty, Generics),
    item_enum(enum_def, Generics),
    item_struct(@struct_def, Generics),
    item_trait(Generics, ~[trait_ref], ~[trait_method]),
    item_impl(Generics,
              Option<trait_ref>, // (optional) trait this impl implements
              Ty, // self
              ~[@method]),
    // a macro invocation (which includes macro definition)
    item_mac(mac),
pub struct struct_def {
    fields: ~[@struct_field], // fields, not including ctor 
    ctor_id: Option<node_id>
pub struct spanned<T> { node: T, span: span }
pub type struct_field = spanned<struct_field_>;
pub struct struct_field_ {
    kind: struct_field_kind,
    id: node_id,
    ty: Ty,
    attrs: ~[attribute],
}

*/
/*
impl MyAstNode {
	fn span(&self)->syntax::codemap::span {
		match self {
			&ViewItem(ref x)=> x.span,
			&Item(ref x)=> x.span,
			&Expr(ref x)=> x.span,
		}
	}
}

enum WhatNext {
	Break,
	Continue,
	Recurse
}

type MyAstFn<A> = @fn(a:A, node:@MyAstNode,depth:int)->(A,WhatNext);

priv fn v_ast_item<A:Clone>(mut acc:A, it:@ast::item,f:MyAstFn<A>,depth:int)->(A,WhatNext){
	match it.node {
	ast::item_mod(x)=> v_ast_module(acc.clone(),@x,f,depth+1),
//	item_struct(x)=>{}
	_=>(acc,Continue)
	}
}

priv fn v_ast_module<A:Clone>(mut acc:A, m:@ast::_mod,f:MyAstFn<A>,depth:int)->(A,WhatNext){
	for m.view_items.iter().advance |vi|{
		let bvi=@copy *vi;
		let (a,c)=f(acc.clone(),@ViewItem(bvi),depth);
		acc=a;
	}
	for m.items.iter().advance |it|{
		let (a,c)=f(acc.clone(),@Item(*it),depth);
		acc=a;
		v_ast_item(acc.clone(),*it,f,depth);
	}
	(acc,Continue)
}

pub fn my_visit_ast<A:Clone>(input:A, c:@ast::crate, f:MyAstFn<A>,depth:int)->(A,WhatNext) {
	let cm=@copy c.node.module;
	let (input1,_)=v_ast_module(input, cm,f,depth);
	(input1,Continue)
}

fn find_node_at(src_loc:int,c:@ast::crate)->~[@MyAstNode] 
{
	// my_visit_ast(~[], c, |...|{
	//     if span encloses this location
    //          push this node onto Accumulator
	//          recurse
	//     else
    //          continue
	//	}
	~[]
}
*/

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
	dump!(find_ast_node::find(ctxt.crate,25))
	dump!(find_ast_node::find(ctxt.crate,97))

}
