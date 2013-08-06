use syntax::ast;
use syntax::ast::*;
use syntax::ast_map;
use syntax::visit;
use syntax::oldvisit::*;
use syntax::oldvisit::{Visitor, fn_kind};
use syntax::codemap::*;
use rustc::{front, metadata, driver, middle};
use rustc::middle::mem_categorization::ast_node;
use syntax::*;
use syntax::ast_util;
use syntax::abi::AbiSet;
use rustc::middle::*;
use rustc::metadata::*;
use rustc::middle::trans::context::*;
use std::hashmap::HashMap;


// TODO Check with rust people what here can be replaced with existing code in from the compiler libs..
// if ctxt_ had accessors for everything indexed by node_id we could remove most of this.
// (AstNode here could be replaced with a refernce to ctxt_ keeping the interface..)

// TODO: code here only depends on ty::ctxt, sess:Session is held in there aswell.
pub struct DocContext {
    crate: @ast::Crate,
    tycx: middle::ty::ctxt,
    sess: driver::session::Session,
	ca: driver::driver::CrateAnalysis
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
pub fn find_node_at_byte_pos(c:@Crate,_location:codemap::BytePos)->AstNode {
	let tree_loc=find_node_tree_loc_at_byte_pos(c,_location);
	return tree_loc.last().clone();
}

pub type NodeTreeLoc = ~[AstNode];

pub fn find_node_tree_loc_at_byte_pos(c:@Crate,_location:codemap::BytePos)->NodeTreeLoc {
	// TODO: Now that we have a sepereate 'node-spans table'
	// would it be more efficient to use that?
	// if we encoded hrc information in the node-spans-table,
	// we wouldn't need all this iterator malarchy again.
	let mut s= @mut FindAstNodeSt{
		result:~[astnode_root], location:*_location, stop:false
			
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

pub struct NodeInfo {
	//name:ident, .. TODO - does it make sense to cache an ident here? not all nodes have..
	kind:~str,
	span:codemap::span,
	node:AstNode,
}
pub type NodeInfoMap= HashMap<ast::NodeId,NodeInfo>;

pub fn build_node_info_map(c:@Crate)->@mut NodeInfoMap {
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
		visit_ty_method:fcns_type_method,
		//visit_trait_method
		visit_struct_def:fcns_struct_def,
		visit_struct_field:fcns_struct_field,

		.. *default_visitor::<@mut NodeInfoMap>()
		}
	);
	visit_crate(c,(node_spans,vt));
	node_spans
}


pub trait ToJsonStr {
	fn to_json_str(&self)->~str;
}

pub fn node_spans_table_to_json_sub(ns:&NodeInfoMap)->~str {
	// TODO - is there a cleaner functional way,
	// map (|x| fmt...).flatten_to_str() or something like that..
	let mut r=~"";
//	for ns.iter().advance |(k,v)| {
	for (k,v) in ns.iter() {
		//let (_,line,_)=byte_pos_to_file_line_col(c,*v.span.lo);
		r.push_str(fmt!("\t{node_id:%i,\tkind:\"%s\",\tspan:{lo:%u,\thi:%u}},\n",*k,v.kind,*v.span.lo,*v.span.hi));
	}
	r
}

impl ToJsonStr for NodeInfoMap {
	pub fn to_json_str(&self)->~str {
		~"[\n"+node_spans_table_to_json_sub(self)+~"]\n"
	}
}

impl ToJsonStr for HashMap<ast::NodeId,ast::def_id> {
	pub fn to_json_str(&self)->~str {
		let mut r=~"[\n";
//		for self.iter().advance|(&key,&value)| {
		for (&key,&value) in self.iter() {
			r.push_str(fmt!("\t{node_id:%?,\tdef_id:{crate:%?,node:%?}},\n", key, value.crate,value.node));
		}
		r.push_str("]\n");
		r
	}
}

// TODO - is there an official wrapper like this for all nodes in libsyntax::ast?
// is there a way of acheiving this without one?
// TODO: TRY USING ast_map::ast_node INSTEAD
#[deriving(Clone)]

// TODO - include heirachical location in wrappertype? node.parent.. node.node
// Currently we use a [AstNode] to represent a location in the node-tree with all parent nodes.
// but this looks confusing, eg are we passing a node list?
// struct HrcNode{ parent:Option<HrcNode>, node:AstNode}
// or even AstNode{ .../* each type has (sParent,content)*/}

pub enum AstNode 
{
	astnode_mod(@_mod),
	astnode_view_item(@view_item),
	astnode_item(@item),
	astnode_local(@Local),
	astnode_block(@Block),
	astnode_stmt(@stmt),
	astnode_arm(@arm),
	astnode_pat(@pat),
	astnode_decl(@decl),
	astnode_expr(@expr),
	astnode_ty(@Ty),
	astnode_ty_method(@TypeMethod),
	astnode_trait_method(@trait_method),
	astnode_method(@method),
	astnode_struct_def(@struct_def),
	astnode_struct_field(@struct_field),
	astnode_root,
	astnode_none
}

//

pub trait KindToStr {
	fn kind_to_str(&self)->&'static str;
}
impl KindToStr for ast::decl {
	fn kind_to_str(&self)->&'static str {
		match self.node {
		decl_local(l)=>"decl_local",
		decl_item(x)=>x.kind_to_str(),
		}
	}	
}
impl KindToStr for ast::item {
	fn kind_to_str(&self)->&'static str {
		match (self.node) {
		item_static(*)=>"static",
		item_fn(*)=>"fn",
		item_mod(*)=>"mod",
		item_foreign_mod(*)=>"foreign_mod",
		item_ty(*)=>"ty",
		item_enum(*)=>"enum",
		item_struct(*)=>"struct",
		item_trait(*)=>"trait",
		item_impl(*)=>"impl",
		item_mac(*)=>"mac",
		}
	}
}
impl KindToStr for ast::expr {
	fn kind_to_str(&self)->&'static str {
		match self.node {
		expr_vstore(_,_)=>"vstore",
		expr_vec(_,_)=>"vec",
		expr_call(_,_,_)=>"call",
		expr_method_call(_,_,_,_,_,_)=>"method_call",
		expr_tup(_)=>"tup",
	    expr_binary(_, binop, _,_)=>match binop {
//			ast_util::binop_to_*(binop) todo - we donnt use this because of ambiguity
			add=>"add",
			subtract=>"sub",
			mul=>"mul",
			div=>"div",
			rem=>"rem",
			and=>"and",
			or=>"or",
			bitxor=>"bitxor",
			bitand=>"bitand",
			bitor=>"bitor",
			shl=>"shl",
			shr=>"shr",
			eq=>"eq",
			lt=>"lt",
			le=>"le",
			ne=>"ne",
			ge=>"ge",
			gt=>"gt",

		},
	    expr_unary(_, unop, _)=>match unop {
			box(mt)=>"box",
			uniq=>"uniq",
			deref=>"deref",
			not=>"not",
			neg=>"neg"
		},
	    expr_lit(_)=>"lit",
	    expr_cast(_, _)=>"cast",
    	expr_if(_,_,_)=>"if",

	    expr_while(_, _)=>"while",
	    expr_for_loop(_, _,_)=>"for_loop",
	    expr_loop(_, _)=>"loop",
	    expr_match(_, _)=>"match",
	    expr_fn_block(_, _)=>"fn_blk",
	    expr_do_body(_)=>"do_body",
	    expr_block(_)=>"blk",
	    expr_assign(_,_)=>"assign",
	    expr_assign_op(_, binop, _, _)=>match binop {
			add=>"assign_add",
			subtract=>"assign_sub",
			mul=>"assign_mul",
			div=>"assign_div",
			rem=>"assign_rem",
			and=>"assign_and",
			or=>"assign_or",
			bitxor=>"assign_bitxor",
			bitand=>"assign_bitand",
			bitor=>"assign_bitor",
			shl=>"assign_shl",
			shr=>"assign_shr",
			eq=>"assign_eq",
			lt=>"assign_lt",
			le=>"assign_le",
			ne=>"assign_ne",
			ge=>"assign_ge",
			gt=>"assign_gt"
		},
	    expr_field(_, _, _)=>"field",
	    expr_index(_,_,_)=>"index",
	    expr_path(_)=>"path",
	    expr_self=>"self",
	    expr_addr_of(_, _)=>"addr_of",
	    expr_break(_)=>"break",
		expr_again(_)=>"again",
	    expr_ret(ret)=>"ret",
	    expr_log(_,_)=>"log",
	    expr_inline_asm(_)=>"inline_asm",
	    expr_mac(_)=>"mac",
	    expr_struct(_,_,_)=>"struct",
	    expr_repeat(_,_,_)=>"repeat",
	    expr_paren(_)=>"paren",
		}
	}
}

impl AstNode {
	// Accessor for the node_id to use for getting definition
	// TODO - clarify by wrapping access to def_map here?
	pub fn ty_node_id(&self)->Option<ast::NodeId> {
		match *self {
			astnode_ty(ty)=>
				match(ty.node) {
					ty_path(_,_,NodeId)=>Some(NodeId),
					_ => self.get_id()
				},
			_ => self.get_id()
		}
	}
}
impl KindToStr for AstNode {
	pub fn kind_to_str(&self)->&'static str {
		//TODO subsets of view_item?
		match *self {
			astnode_mod(_)=>"mod",
			astnode_view_item(_)=>"view_item",
			astnode_item(x)=>x.kind_to_str(),
			astnode_local(_)=>"local", 
			astnode_block(_)=>"block",
			astnode_stmt(_)=>"stmt",
			astnode_arm(_)=>"arm",
			astnode_pat(_)=>"pat",
			astnode_decl(x)=>x.kind_to_str(),
			astnode_expr(x)=>x.kind_to_str(),
			astnode_ty(_)=>"ty",
			astnode_ty_method(_)=>"ty_method",
			astnode_method(_)=>"method",
			astnode_trait_method(_)=>"trait_method",
			astnode_struct_def(_)=>"struct_def",
			astnode_struct_field(_)=>"struct_field",
			astnode_root=>"root",
			astnode_none=>"none"
		}
	}
}

pub trait AstNodeAccessors {
	pub fn get_id(&self)->Option<ast::NodeId>;
}
impl AstNodeAccessors for ast::item {
	pub fn get_id(&self)->Option<ast::NodeId> {
		Some(self.id)
	}
}
impl AstNodeAccessors for ast::Local {
	pub fn get_id(&self)->Option<ast::NodeId> {
		Some(self.id)
	}
}

impl AstNodeAccessors for ast::item_ {
	pub fn get_id(&self)->Option<ast::NodeId> {
		match *self {
		item_static(_,_,ref e) => Some(e.id),
		item_fn(ref decl,_,_,_,ref b)=>Some(b.id),
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
	pub fn get_id(&self)->Option<NodeId> {
		match *self{
			decl_local(ref x)=>Some(x.id),
			decl_item(ref x)=>Some(x.id)
		}
	}
}
impl<T:AstNodeAccessors> AstNodeAccessors for codemap::spanned<T> {
	pub fn get_id(&self)->Option<NodeId> {
		self.node.get_id()
	}
}
//impl AstNodeAccessors for ty_method {
//	pub fn get_id(&self)->Option<NodeId> {
//		Some(self.id)
//	}
//}
impl AstNodeAccessors for ast::Block {
	pub fn get_id(&self)->Option<NodeId> {
		Some(self.id)
	}
}
impl AstNodeAccessors for stmt_ {
	pub fn get_id(&self)->Option<NodeId> {
		match *self {
			stmt_decl(_,x)=>Some(x),
			stmt_expr(_,x)=>Some(x),
			stmt_semi(_,x)=>Some(x),
			stmt_mac(_,_)=>None
		}
	}
}

impl AstNodeAccessors for view_item_ {
	pub fn get_id(&self)->Option<NodeId> {
		match *self {
			view_item_extern_mod(_,_,node_id)=>Some(node_id),
			view_item_use(_)=>None
		}
	}
}
impl AstNodeAccessors for struct_field_ {
	pub fn get_id(&self)->Option<NodeId> {
		Some(self.id)
	}
}

impl AstNodeAccessors for trait_method {
	pub fn get_id(&self)->Option<NodeId> {
		match(*self) {
			required(ref m)=>Some(m.id),
			provided(ref m)=>None
		}
	}
}

impl AstNodeAccessors for AstNode {
	pub fn get_id(&self)->Option<NodeId> {
		// todo - should be option<node_id> really..
		match *self {
			astnode_mod(ref x) => None,
			astnode_view_item(ref x) =>x.node.get_id(),
			astnode_item(ref x) =>Some(x.id),
			astnode_local(ref x) =>Some(x.id),
			astnode_block(ref x)=>None,
			astnode_stmt(ref x)=>None,
			astnode_arm(ref x)=>None,
			astnode_pat(ref x)=>Some(x.id),
			astnode_decl(ref x)=>x.get_id(),
			astnode_expr(ref x)=>Some(x.id),
//			astnode_expr_post(ref x)=>Some(x.id),
			astnode_ty(ref x)=>Some(x.id),
			astnode_ty_method(ref x)=>Some(x.id),
			astnode_trait_method(ref x)=>x.get_id(),
			astnode_method(ref m)=>Some(m.id),
			astnode_struct_def(ref x)=>None,
			astnode_struct_field(ref x)=>Some(x.node.id),
			astnode_none|astnode_root=>None,
			
		}
	}
}
fn item_get_ident(a:&item)->Option<ident> { Some(a.ident) }

fn decl_get_ident(a:&decl)->Option<ident> {
	match a.node {
		decl_local(ref l)=> None, // todo - will we need the ident ?a local isn't always 1, due to tuples
		decl_item(i)=> item_get_ident(i)
	}
}
fn expr_get_ident(a:&expr)->Option<ident> {
	None
}
 

pub struct FindAstNodeSt {
	result: NodeTreeLoc,		// todo - full tree path, all the parent nodes.
	location: uint,
	stop: bool,
//	node_spans: HashMap<ast::node_id,codemap::span>
}

pub fn get_ast_node_of_node_id(info:&NodeInfoMap,id:ast::NodeId)->Option<AstNode> {
	match info.find(&id) {
		None=>None,
		Some(node_info)=>Some(node_info.node)
	}
}

pub fn push_span(spt:&mut NodeInfoMap,n:ast::NodeId,idt:Option<ident>,k:&str, s:codemap::span,nd:AstNode) {
	spt.insert(n,NodeInfo{kind:k.to_str(),span:s,node:nd});
}

pub fn push_spanned<T:AstNodeAccessors>(spt:&mut NodeInfoMap,k:&str,s:&codemap::spanned<T>,nd:AstNode) {
	match s.node.get_id() {
		Some(id)=>{spt.insert(id,NodeInfo{kind:k.to_str(),span:s.span,node:nd});}
		None=>{}
	}
}

pub fn span_contains(l:uint,s:span)->bool {
	let BytePos(lo)=s.lo;
	let BytePos(hi)=s.hi;
	l>=lo && l<hi
}
type NodeInfoMapSV = (@mut NodeInfoMap, vt<@mut NodeInfoMap>);
fn fcns_view_item(a:&view_item, (s,v):NodeInfoMapSV) {
	visit_view_item(a,(s,v))
}
fn fcns_item(a:@item, (s,v):NodeInfoMapSV) {
	push_span(s,a.id,item_get_ident(a),a.kind_to_str(),a.span,astnode_item(a));
	match (a.node) {
		item_impl(ref typarams,ref traitref,ref self_ty, ref methods)=> {
			for m in methods.iter() {
				push_span(s,m.id,Some(a.ident),"method",m.span,astnode_method(*m));
				// iterate sub??
			}
		}
		_ => {}
	}
	visit_item(a,(s,v))
}
fn fcns_local(a:@Local, (s,v):NodeInfoMapSV) {
//	push_spanned(s,"local",a);
	// local var? pattern not ident..
	push_span(s, a.id, None,"local",a.span,astnode_local(a));
	visit_local(a,(s,v))
}
fn fcns_block(a:&Block, (s,v):NodeInfoMapSV) {
//	push_spanned(s,"block",a);
	push_span(s, a.id, None, "block",a.span,astnode_none);
	visit_block(a,(s,v))
}
fn fcns_stmt(a:@stmt, (s,v):NodeInfoMapSV) {
	push_spanned(s,"stmt",a,astnode_stmt(a));
	visit_stmt(a,(s,v))
}
fn fcns_arm(a:&arm, (s,v):NodeInfoMapSV) {
//	push_spanned(s,"arm",&a.body);
	// todo - does an arm even have a node id?
//	push_span(s, a.body.id, None,"arm", a.span);
// ERRORTODO..
	visit_arm(a,(s,v))
}
fn fcns_pat(a:@pat, (s,v):NodeInfoMapSV) {
	push_span(s,a.id,None,"pat",a.span,astnode_pat(a));
	visit_pat(a,(s,v))
}
fn fcns_decl(a:@decl, (s,v):NodeInfoMapSV) {
	push_spanned(s,"decl",a,astnode_decl(a));
	visit_decl(a,(s,v))
}
fn fcns_struct_def(sd:@struct_def, ide:ident, g:&Generics, id:NodeId, (s,b):NodeInfoMapSV) {
	visit_struct_def(sd,ide,g,id,(s,b))
}

// struct Visitor<E>.visit_expr: @fn(@expr, (E, vt<E>)),
fn fcns_expr(a:@expr, (s,v):NodeInfoMapSV) {
	push_span(s,a.id,expr_get_ident(a),a.kind_to_str(),a.span,astnode_expr(a));
	visit_expr(a,(s,v))
}

// struct Visitor<E>.visit_expr_post: @fn(@expr, (E, vt<E>)),
fn fcns_expr_post(a:@expr, (s,v):NodeInfoMapSV) {
	visit_expr(a,(s,v))
}

// struct Visitor<E>.visit_ty: @fn(&Ty, (E, vt<E>)),
fn fcns_ty(a:&Ty, (s,v):NodeInfoMapSV) {
	push_span(s,a.id,None,"ty",a.span,astnode_none);
	visit_ty(a,(s,v))
}
fn fcns_fn(fk:&fn_kind, fd:&fn_decl, body:&Block, sp:span, nid:NodeId, (s,v):NodeInfoMapSV) {
	visit_fn(fk,fd,body,sp,nid,(s,v))
}
fn fcns_struct_field(a:@struct_field, (s,v):NodeInfoMapSV) {
	push_spanned(s,"struct_field",a,astnode_struct_field(a));
	visit_struct_field(a,(s,v))
}

fn fcns_type_method(a:&TypeMethod, (s,v):NodeInfoMapSV) {
	push_span(s,a.id,Some(a.ident),"type_method",a.span,astnode_none);
	visit_ty_method(a,(s,v))
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
		s.result.push(astnode_view_item(@a.clone()));
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
fn find_local(a:@Local, (s,v):FindAstNodeSV) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_local(a));
	}
	visit_local(a,(s,v))
}

// struct Visitor<E>.visit_block: @fn(&blk, (E, vt<E>)),
fn find_block(a:&Block, (s,v):FindAstNodeSV) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_block(@a.clone()));
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
	// aparently this is just a hook, not a node type.
	//if span_contains(s.location, a.span) {
		//s.result.push(astnode_expr_post(a));
	//}
	visit_expr(a,(s,v))
}

// struct Visitor<E>.visit_ty: @fn(&Ty, (E, vt<E>)),
fn find_ty(a:&Ty, (s,v):FindAstNodeSV) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_ty(@a.clone()));
	}
	visit_ty(a,(s,v))
}

// struct Visitor<E>.visit_generics: @fn(&Generics, (E, vt<E>)),
// struct Visitor<E>.visit_fn: @fn(&fn_kind, &fn_decl, &blk, span, node_id, (E, vt<E>)),


fn find_fn(fk:&fn_kind, fd:&fn_decl, body:&Block, sp:span, nid:NodeId, (s,v):FindAstNodeSV) {
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


pub fn get_node_info_str(dc:&DocContext,node:&NodeTreeLoc)->~str
{
	fn path_to_str(dc:&DocContext, path:&ast::Path)->~str
	{
		let mut acc=~"";
		let mut first=true;
//		for path.idents.iter().advance |x|{
		for x in path.idents.iter() {
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

pub fn safe_node_id_to_type(cx: ty::ctxt, id: ast::NodeId) -> Option<ty::t> {
    //io::println(fmt!("%?/%?", id, cx.node_types.len()));
    match cx.node_types.find(&(id as uint)) {
       Some(&t) => Some(t),
       None => None    
	}
}

pub fn get_def_id(curr_crate:CrateNum,src_def:def)->Option<def_id> {
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








