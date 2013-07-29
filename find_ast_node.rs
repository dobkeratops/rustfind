use syntax::ast;
use syntax::ast::*;
use syntax::ast_map;
use syntax::visit;
use syntax::visit::*;
use syntax::visit::{Visitor, fn_kind};
use syntax::codemap::*;
use rustc::{front, metadata, driver, middle};
use rustc::middle::mem_categorization::ast_node;
use syntax::*;
use syntax::abi::AbiSet;
use rustc::middle::*;
use std::hashmap::HashMap;


// TODO: code here only depends on ty::ctxt, sess:Session is held in there aswell.
pub struct DocContext {
    crate: @ast::crate,
    tycx: middle::ty::ctxt,
    sess: driver::session::Session
}


macro_rules! dump{ ($($a:expr),*)=>
	(	{	let mut txt=file!()+":"+line!().to_str()+": " ; 
			$( txt=txt.append(
				fmt!("%s=%?",stringify!($a),$a)+",") 
			);*; 
			println(txt); 
		}
	)
}

/// main
pub fn find(c:@crate,_location:uint)->~[AstNode] {
	// TODO: Now that we have a sepereate 'node-spans table'
	// would it be more efficient to use that?
	// if we encoded hrc information in the node-spans-table,
	// we wouldn't need all this iterator malarchy again.
	let mut s= @mut FindAstNodeSt{
		result:~[astnode_root], location:_location, stop:false
			
	};
	let vt=mk_vt(@Visitor{
		//visit_mod
		visit_view_item:find_view_item,
		//visit_foreign_item
		visit_item:find_item,
		visit_local:find_local,
		visit_block:find_block,
		visit_stmt:find_stmt,
		visit_arm:find_arm,
		visit_pat:find_pat,
		visit_decl:find_decl,
		visit_expr:find_expr,
		//visit_expr_post:f_expr,--called after visit
		visit_ty:find_ty,
		//visit_method
		//visit_trait_method
//		visit_struct_def:f_struct_def,
		visit_struct_field:find_struct_field,

		.. *default_visitor::<@mut FindAstNodeSt>()
		}
	);
	visit_crate(c, (s,vt));
	s.result.clone()
}

pub type NodeSpans= HashMap<ast::node_id,codemap::span>;

pub fn build_node_spans_table(c:@crate)->@mut NodeSpans {
	// todo-lambdas, big fcuntion but remove the extraneous symbols
	let node_spans=@mut HashMap::new();
	let vt=mk_vt(@Visitor{
		//visit_mod
		visit_view_item:fcns_view_item,
		//visit_foreign_item
		visit_item:fcns_item,
		visit_local:fcns_local,
		visit_block:fcns_block,
		visit_stmt:fcns_stmt,
		visit_arm:fcns_arm,
		visit_pat:fcns_pat,
		visit_decl:fcns_decl,
		visit_expr:fcns_expr,
		//visit_expr_post:f_expr,--called after visit
		visit_ty:fcns_ty,
		//visit_method
		//visit_trait_method
		visit_struct_def:fcns_struct_def,
		visit_struct_field:fcns_struct_field,

		.. *default_visitor::<@mut NodeSpans>()
		}
	);
	visit_crate(c,(node_spans,vt));
	node_spans
}

pub fn dump_node_spans_table(ns:&NodeSpans) {
	for ns.iter().advance |(k,v)| {
		println(fmt!("nodeid=%i span=%?",*k,v));
		
	}
}

// TODO - is there an official wrapper like this for all nodes in libsyntax::ast?
// is there a way of acheiving this without one?
#[deriving(Clone)]
pub enum AstNode 
{
	astnode_mod(@_mod),
	astnode_view_item(@view_item),
	astnode_item(@item),
	astnode_local(@local),
	astnode_block(@blk),
	astnode_stmt(@stmt),
	astnode_arm(@arm),
	astnode_pat(@pat),
	astnode_decl(@decl),
	astnode_expr(@expr),
	astnode_expr_post(@expr),
	astnode_ty(@Ty),
	astnode_ty_method(@ty_method),
	astnode_trait_method(@trait_method),
	astnode_struct_def(@struct_def),
	astnode_struct_field(@struct_field),
	astnode_root	
}

pub trait AstNodeAccessors {
	pub fn get_id(&self)->Option<ast::node_id>;
}
impl AstNodeAccessors for ast::item {
	pub fn get_id(&self)->Option<ast::node_id> {
		Some(self.id)
	}
}
impl AstNodeAccessors for ast::local_ {
	pub fn get_id(&self)->Option<ast::node_id> {
		Some(self.id)
	}
}

impl AstNodeAccessors for ast::item_ {
	pub fn get_id(&self)->Option<ast::node_id> {
		match *self {
		item_static(_,_,ref e) => Some(e.id),
		item_fn(ref decl,_,_,_,ref b)=>Some(b.node.id),
		item_mod(_)=>None,
		item_foreign_mod(_)=>None,
		item_ty(ref ty,_)=>Some(ty.id),
		item_enum(_,_)=>None,
		item_struct(_,_)=>None,
		item_trait(_,_,_)=>None,
		item_impl(_,_,ref ty,_)=>Some(ty.id), //TODO, is this wrong, just node_id of a component?
		item_mac(_)=>None,
		}
	}
}

impl AstNodeAccessors for ast::decl_ {
	pub fn get_id(&self)->Option<node_id> {
		match *self{
			decl_local(ref x)=>Some(x.node.id),
			decl_item(ref x)=>Some(x.id)
		}
	}
}
impl<T:AstNodeAccessors> AstNodeAccessors for codemap::spanned<T> {
	pub fn get_id(&self)->Option<node_id> {
		self.node.get_id()
	}
}
impl AstNodeAccessors for ty_method {
	pub fn get_id(&self)->Option<node_id> {
		Some(self.id)
	}
}
impl AstNodeAccessors for blk_ {
	pub fn get_id(&self)->Option<node_id> {
		Some(self.id)
	}
}
impl AstNodeAccessors for stmt_ {
	pub fn get_id(&self)->Option<node_id> {
		match *self {
			stmt_decl(_,x)=>Some(x),
			stmt_expr(_,x)=>Some(x),
			stmt_semi(_,x)=>Some(x),
			stmt_mac(_,_)=>None
		}
	}
}

impl AstNodeAccessors for view_item_ {
	pub fn get_id(&self)->Option<node_id> {
		match *self {
			view_item_extern_mod(_,_,node_id)=>Some(node_id),
			view_item_use(_)=>None
		}
	}
}
impl AstNodeAccessors for struct_field_ {
	pub fn get_id(&self)->Option<node_id> {
		Some(self.id)
	}
}

impl AstNodeAccessors for trait_method {
	pub fn get_id(&self)->Option<node_id> {
		match(*self) {
			required(ref m)=>Some(m.id),
			provided(ref m)=>None
		}
	}
}

impl AstNodeAccessors for AstNode {
	pub fn get_id(&self)->Option<node_id> {
		// todo - should be option<node_id> really..
		match *self {
			astnode_mod(ref x) => None,
			astnode_view_item(ref x) =>x.node.get_id(),
			astnode_item(ref x) =>Some(x.id),
			astnode_local(ref x) =>Some(x.node.id),
			astnode_block(ref x)=>None,
			astnode_stmt(ref x)=>None,
			astnode_arm(ref x)=>None,
			astnode_pat(ref x)=>Some(x.id),
			astnode_decl(ref x)=>x.get_id(),
			astnode_expr(ref x)=>Some(x.id),
			astnode_expr_post(ref x)=>Some(x.id),
			astnode_ty(ref x)=>Some(x.id),
			astnode_ty_method(ref x)=>Some(x.id),
			astnode_trait_method(ref x)=>x.get_id(),
			astnode_struct_def(ref x)=>None,
			astnode_struct_field(ref x)=>Some(x.node.id),
			astnode_root=>None,
		}
	}
}


pub struct FindAstNodeSt {
	result: ~[AstNode],		// todo - full tree path, all the parent nodes.
	location: uint,
	stop: bool,
//	node_spans: HashMap<ast::node_id,codemap::span>
}

pub fn push_span(spt:&mut NodeSpans,n:ast::node_id, s:codemap::span) {
	spt.insert(n,s);
}

pub fn push_spanned<T:AstNodeAccessors>(spt:&mut NodeSpans,s:&codemap::spanned<T>) {
	match s.node.get_id() {
		Some(id)=>{spt.insert(id,s.span);}
		None=>{}
	}
}

pub fn span_contains(l:uint,s:span)->bool {
	let BytePos(lo)=s.lo;
	let BytePos(hi)=s.hi;
	l>=lo && l<hi
}
type NodeSpansSV = (@mut NodeSpans, vt<@mut NodeSpans>);
fn fcns_view_item(a:&view_item, (s,v):NodeSpansSV) {
	visit_view_item(a,(s,v))
}
fn fcns_item(a:@item, (s,v):NodeSpansSV) {
	push_span(s,a.id,a.span);
	visit_item(a,(s,v))
}
fn fcns_local(a:@local, (s,v):NodeSpansSV) {
	push_spanned(s,a);
	visit_local(a,(s,v))
}
fn fcns_block(a:&blk, (s,v):NodeSpansSV) {
	push_spanned(s,a);
	visit_block(a,(s,v))
}
fn fcns_stmt(a:@stmt, (s,v):NodeSpansSV) {
	push_spanned(s,a);
	visit_stmt(a,(s,v))
}
fn fcns_arm(a:&arm, (s,v):NodeSpansSV) {
	push_spanned(s,&a.body);
	visit_arm(a,(s,v))
}
fn fcns_pat(a:@pat, (s,v):NodeSpansSV) {
	push_span(s,a.id,a.span);
	visit_pat(a,(s,v))
}
fn fcns_decl(a:@decl, (s,v):NodeSpansSV) {
		push_spanned(s,a);
	visit_decl(a,(s,v))
}
fn fcns_struct_def(sd:@struct_def, ide:ident, g:&Generics, id:node_id, (s,b):NodeSpansSV) {
	println(fmt!("visiting struct-def %i",id as int));
	visit_struct_def(sd,ide,g,id,(s,b))
}


// struct Visitor<E>.visit_expr: @fn(@expr, (E, vt<E>)),
fn fcns_expr(a:@expr, (s,v):NodeSpansSV) {
	push_span(s,a.id,a.span);
	visit_expr(a,(s,v))
}

// struct Visitor<E>.visit_expr_post: @fn(@expr, (E, vt<E>)),
fn fcns_expr_post(a:@expr, (s,v):NodeSpansSV) {
	visit_expr(a,(s,v))
}

// struct Visitor<E>.visit_ty: @fn(&Ty, (E, vt<E>)),
fn fcns_ty(a:&Ty, (s,v):NodeSpansSV) {
	push_span(s,a.id,a.span);
	visit_ty(a,(s,v))
}
fn fcns_fn(fk:&fn_kind, fd:&fn_decl, body:&blk, sp:span, nid:node_id, (s,v):NodeSpansSV) {
	visit_fn(fk,fd,body,sp,nid,(s,v))
}
fn fcns_struct_field(a:@struct_field, (s,v):NodeSpansSV) {
	push_spanned(s,a);
	visit_struct_field(a,(s,v))
}


// struct Visitor<E>.visit_mod: @fn(&_mod, span, node_id, (E, vt<E>)),
 //fn f_mod(a:&_mod, (s,v):(@mut State, vt<@mut State>)) {
	//if span_contains(s.location, a.span) {
	//	s.result.push(astnode_mod(@copy *a));
	//}
	//visit_struct_field(a,(s,v))
   //}
// struct Visitor<E>.visit_view_item: @fn(&view_item, (E, vt<E>)),
type FindAstNodeSV = (@mut FindAstNodeSt, vt<@mut FindAstNodeSt>);
fn find_view_item(a:&view_item, (s,v):FindAstNodeSV) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_view_item(@ copy *a));
	}
	visit_view_item(a,(s,v))
}// struct Visitor<E>.visit_foreign_item: @fn(@foreign_item, (E, vt<E>)),
// struct Visitor<E>.visit_item: @fn(@item, (E, vt<E>)),
fn find_item(a:@item, (s,v):FindAstNodeSV) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_item(a));
	}
	visit_item(a,(s,v))
}
// struct Visitor<E>.visit_local: @fn(@local, (E, vt<E>)),
fn find_local(a:@local, (s,v):FindAstNodeSV) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_local(a));
	}
	visit_local(a,(s,v))
}

// struct Visitor<E>.visit_block: @fn(&blk, (E, vt<E>)),
fn find_block(a:&blk, (s,v):FindAstNodeSV) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_block(@ copy *a));
	}
	visit_block(a,(s,v))
}

// struct Visitor<E>.visit_stmt: @fn(@stmt, (E, vt<E>)),
fn find_stmt(a:@stmt, (s,v):FindAstNodeSV) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_stmt(a));
	}
	visit_stmt(a,(s,v))
}

// struct Visitor<E>.visit_arm: @fn(&arm, (E, vt<E>)),
fn find_arm(a:&arm, (s,v):FindAstNodeSV) {
// the whole arm doesn't have a span 
//	if span_contains(s.location, a.span) {
//		s.result.push(astnode_arm(@copy *a));
//	}
	visit_arm(a,(s,v))
}
// struct Visitor<E>.visit_pat: @fn(@pat, (E, vt<E>)),
fn find_pat(a:@pat, (s,v):FindAstNodeSV) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_pat(a));
	}
	visit_pat(a,(s,v))
}


// struct Visitor<E>.visit_decl: @fn(@decl, (E, vt<E>)),
fn find_decl(a:@decl, (s,v):FindAstNodeSV) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_decl(a));
	}
	visit_decl(a,(s,v))
}


// struct Visitor<E>.visit_expr: @fn(@expr, (E, vt<E>)),
fn find_expr(a:@expr, (s,v):FindAstNodeSV) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_expr(a));
	}
	visit_expr(a,(s,v))
}

// struct Visitor<E>.visit_expr_post: @fn(@expr, (E, vt<E>)),
fn find_expr_post(a:@expr, (s,v):FindAstNodeSV) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_expr_post(a));
	}
	visit_expr(a,(s,v))
}

// struct Visitor<E>.visit_ty: @fn(&Ty, (E, vt<E>)),
fn find_ty(a:&Ty, (s,v):FindAstNodeSV) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_ty(@copy *a));
	}
	visit_ty(a,(s,v))
}

// struct Visitor<E>.visit_generics: @fn(&Generics, (E, vt<E>)),
// struct Visitor<E>.visit_fn: @fn(&fn_kind, &fn_decl, &blk, span, node_id, (E, vt<E>)),
fn find_fn(fk:&fn_kind, fd:&fn_decl, body:&blk, sp:span, nid:node_id, (s,v):FindAstNodeSV) {
	//if span_contains(s.location, span) {
		//s.result.push(astnode_fn2(a));
	//}
	visit_fn(fk,fd,body,sp,nid,(s,v))
}
// struct Visitor<E>.visit_ty_method: @fn(&ty_method, (E, vt<E>)),
// struct Visitor<E>.visit_trait_method: @fn(&trait_method, (E, vt<E>)),
// struct Visitor<E>.visit_struct_def: @fn(@struct_def, ident, &Generics, node_id, (E, vt<E>)),
 // fn f_struct_def(a:@struct_def, (s,v):(@mut State, vt<@mut State>)) {
 //	if span_contains(s.location, a.span) {
 //		s.result.push(astnode_struct_def(a));
 //	}
 //	visit_struct_field(a,(s,v))
 //}
// struct Visitor<E>.visit_struct_field: @fn(@struct_field, (E, vt<E>)),
fn find_struct_field(a:@struct_field, (s,v):FindAstNodeSV) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_struct_field(a));
	}
	visit_struct_field(a,(s,v))
}


pub fn get_node_info_str(dc:&DocContext,node:&[AstNode])->~str
{
	fn path_to_str(dc:&DocContext, path:&ast::Path)->~str
	{
		let mut acc=~"";
		let mut first=true;
		for path.idents.iter().advance |x|{
			if !first {acc=acc.append(~"::");}
			acc=acc.append(dc.sess.str_of(*x));
			first=false
		}
		acc
		// typeparams too... path.types?
	}
	fn pat_to_str(dc:&DocContext,p:&ast::pat)->~str{
		// todo -factor out and recurse
		match p.node {
			ast::pat_ident(bind_mode,ref path, opt)=>~"pat_ident:"+path_to_str(dc,path),
			ast::pat_enum(ref path,ref efields)=>~"pat_enum:"+path_to_str(dc,path),//	`todo-fields..
			ast::pat_struct(ref path,ref sfields,b)=>~"pat_struct:"+path_to_str(dc,path)+~"{"+sfields.map(|x|pat_to_str(dc,x.pat)+~",").to_str()+~"}",
			ast::pat_tup(ref elems)=>~"pat_tupl:"+elems.map(|&x|pat_to_str(dc,x)).to_str(),
			//ast::pat_box(ref box)=>~"box",
			ast::pat_uniq(ref u)=>~"uniq",
			ast::pat_region(ref p)=>~"rgn",
			ast::pat_lit(ref e)=>~"literal",
			ast::pat_range(ref e_start,ref e_end)=>~"range",
		
			_=>~"?"
		}
	};
	fn ty_to_str(dc:&DocContext,t:&ast::Ty)->~str{
		match t.node{
			ast::ty_nil=> ~"nil",
			ast::ty_bot=>~"bottomtype",
			ast::ty_box(ref mt)=>~"box",
			ast::ty_vec(ref mt)=>~"vec",
			ast::ty_fixed_length_vec(ref mt,ref expr)=>~"[T,..N]",
			ast::ty_ptr(ref mt)=>~"*",
			ast::ty_rptr(ref lifetime,ref mt)=>~"&",
			ast::ty_tup(ref types)=>~"("+types.map(|x|ty_to_str(dc,x)).to_str()+")", //todo: factor this out, map..
			ast::ty_path(ref path,ref params,node_id)=>~"path:id="+node_id.to_str()+" "+path_to_str(dc,path)
			,
		
			ast::ty_infer=>~"infered",
			_ =>~"?"
		}
	}
	fn expr_to_str(dc:&DocContext, x:&expr_)->~str {
		match *x {
			expr_struct(ref p,_,_)=>~"(expr_struct "+ path_to_str(dc,p) +")",
			expr_call(ref e,ref args,_)=>~"(expr_call("+expr_to_str(dc,&e.node)+args.map(|x|expr_to_str(dc,&x.node)).to_str()+~")",
			expr_field(ref e, ref i, ref tys)=>~"(expr_field("+expr_to_str(dc,&e.node)+")"+dc.sess.str_of(*i)+tys.map(|x|ty_to_str(dc,x)).to_str()+~")",
			_=>~"expr"
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
			dc.sess.str_of(x.ident)+
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
				ast::named_field(nf,vis)=>"struct named_field: "+dc.sess.str_of(nf)+" ",
				_=>~"struct anon_field"
			}+
			~":"+ty_to_str(dc,&sf.node.ty)/*sf.node.ty ..parse it.. */,
		&astnode_pat(p)=>~"pattern: "+
			~"id="+p.id.to_str()+~" "+
			pat_to_str(dc,p)
		,
		&astnode_decl(x)=>~"decl: ?",
		&astnode_ty(x)=>~"type: "+ty_to_str(dc,x),
		&astnode_struct_def(sf)=>~"struct def",
		&astnode_expr(x)=>expr_to_str(dc,&x.node),
		_=>	~"unknown"
	}
}

pub fn safe_node_id_to_type(cx: ty::ctxt, id: ast::node_id) -> Option<ty::t> {
    //io::println(fmt!("%?/%?", id, cx.node_types.len()));
    match cx.node_types.find(&(id as uint)) {
       Some(&t) => Some(t),
       None => None    
	}
}

pub fn get_def_id(curr_crate:crate_num,src_def:def)->Option<def_id> {
	let mk=|x|{Some(def_id{crate:curr_crate, node:x})}; // todo,mmaybe this is best 'None'..
	// todo-'definition' can be at multiple locations. we should return [def_id] really..
	match (src_def) {
		def_fn(d,_)=>Some(d),
		def_static_method(d,_,_)=>Some(d),
		def_self(id,_)=>mk(id),
		def_self_ty(id)=>mk(id),
		def_mod(d)=>Some(d),
		def_foreign_mod(d)=>Some(d),
		def_static(d,_)=>Some(d),
		def_arg(id,_)=>mk(id),
		def_local(id,_)=>mk(id),
		def_variant(d1,d2)=>Some(d2),
		def_ty(d)=>Some(d),
		def_trait(d)=>Some(d),
		def_prim_ty(pt)=>None,
		def_ty_param(d,_)=>Some(d),
		def_binding(d,_)=>mk(d),
		def_use(d)=>Some(d),
		def_upvar(_,d,_,_)=>get_def_id(curr_crate,*d),
		def_struct(d)=>Some(d),
		def_typaram_binder(id)=>mk(id),
		def_region(id)=>mk(id),
		def_label(id)=>mk(id),
		def_method(d,_)=>Some(d)
	}
}




