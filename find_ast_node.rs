use rf_common::*;
use syntax::ast;
use syntax::visit;
// use syntax::oldvisit;
// use syntax::visit::*;
use syntax::visit::{Visitor};
use syntax::codemap;
use rustc::middle::mem_categorization::ast_node;
use rustc::middle::ty;
use rfindctx::{RFindCtx,};
use codemaput::{ZTextFilePos,ToZIndexFilePos,dump_span,get_span_str};

/*
todo .. this wants to be split more, eg visitor dependancies in a sepearate file,
ast accessors/helpers in a seperate file
but for the timebeing, we are using it to
move code out of the crate root where things are tangled up  much worse
*/
pub macro_rules! logi{
	($($a:expr),*)=>(println(""$(+$a.to_str())*) )
}

#[deriving(Clone)]
pub enum AstNode {
	astnode_mod(@ast::_mod),
	astnode_view_item(@ast::view_item),
	astnode_item(@ast::item),
	astnode_local(@ast::Local),
	astnode_block(@ast::Block),
	astnode_stmt(@ast::Stmt),
	astnode_arm(@ast::Arm),
	astnode_pat(@ast::Pat),
	astnode_decl(@ast::Decl),
	astnode_expr(@ast::Expr),
	astnode_ty(@ast::Ty),
	astnode_ty_method(@ast::TypeMethod),
	astnode_trait_method(@ast::trait_method),
	astnode_method(@ast::method),
	astnode_struct_def(@ast::struct_def),
	astnode_struct_field(@ast::struct_field),
	astnode_trait_ref(@ast::trait_ref),
	astnode_variant(@ast::variant),
	astnode_root,
	astnode_none
}



pub struct FNodeInfo {
	ident:Option<ast::Ident>, 		//.. TODO - does it make sense to cache an ident here? not all nodes have..
	kind:~str,
	span:codemap::Span,
	node:AstNode,
	parent_id:ast::NodeId,
}
pub type FNodeInfoMap= hashmap::HashMap<ast::NodeId,FNodeInfo>;

pub type NodeTreeLoc = ~[AstNode];
pub fn dump_node_tree_loc(ndt:&NodeTreeLoc) {
//	for ndt.iter().advance |x|
	for x in ndt.iter()
	 {print(x.kind_to_str()+".");} print("\n");
}


pub trait AstNodeAccessors {
	fn get_id(&self)->Option<ast::NodeId>;
	fn get_ident(&self)->Option<ast::Ident>;
}
pub trait KindToStr {
	fn kind_to_str(&self)->&'static str;
}
pub trait ToJsonStr {fn to_json_str(&self)->~str;}


// TODO Check with rust people what here can be replaced with existing code in from the compiler libs..
// if ctxt_ had accessors for everything indexed by node_id we could remove most of this.
// (AstNode here could be replaced with a refernce to ctxt_ keeping the interface..)

// TODO: code here only depends on ty::ctxt, sess:Session is held in there aswell.

pub macro_rules! if_some {
	($b:ident in $a:expr then $c:expr)=>(
		match $a {
			Some($b)=>$c,
			None=>{}
		}
	);
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
pub fn find_node_at_byte_pos(c:@ast::Crate,_location:codemap::BytePos)->AstNode {
	let tree_loc=find_node_tree_loc_at_byte_pos(c,_location);
	return tree_loc.last().clone();
}


pub fn find_node_tree_loc_at_byte_pos(c:@ast::Crate,_location:codemap::BytePos)->NodeTreeLoc {
	use syntax::visit::*;
	// TODO: Now that we have a sepereate 'node-spans table'
	// would it be more efficient to use that?
	// if we encoded hrc information in the node-spans-table,
	// we wouldn't need all this iterator malarchy again.
	let s = @mut FindAstNodeSt{
		result:~[astnode_root], location:*_location, stop:false

	};

	let mut vt = Finder;

	visit::walk_crate(&mut vt, c, s);

	s.result.clone()
}


pub fn build_node_info_map(c:@ast::Crate)->@mut FNodeInfoMap {
	// todo-lambdas, big fcuntion but remove the extraneous symbols
	let node_spans=@mut hashmap::HashMap::new();

	let mut vt = FncsThing;

	visit::walk_crate(&mut vt, c, (node_spans,0/*0=root*/));
	node_spans
}


pub trait ToJsonStrFc {fn to_json_str(&self,c:&RFindCtx)->~str;}

pub fn node_spans_table_to_json_sub(dc:&RFindCtx,ns:&FNodeInfoMap)->~str {
	// TODO - is there a cleaner functional way,
	// map (|x| fmt...).flatten_to_str() or something like that..

	// emit in a form more useable by external tools.
	// not a serialization of the data used here.
	let mut r=~"";
//	for ns.iter().advance |(k,v)| {
	for (k,v) in ns.iter() {
		//let (_,line,_)=byte_pos_to_file_line_col(c,*v.span.lo);

		let oifps=v.span.lo.to_index_file_pos(dc.tycx);
		let oifpe=v.span.hi.to_index_file_pos(dc.tycx);
		if oifps.is_some() && oifpe.is_some() {
			let ifps=oifps.unwrap();
			let ifpe=oifpe.unwrap();
			// local node:-
			//assert!(ifps.file_index==ifpe.file_index);
			r.push_str(format!("\t\\{node_id:{:u},\tkind:\"{:s}\",\tlspan:\\{ lo:\\{file:{:u},line:{:u} ,ofs:{:u}\\},\thi:\\{file:{:u},line:{:u}, ofs:{:u}\\}\\}\\},\n",*k,v.kind,
				ifps.file_index, ifps.line, ifps.col,ifpe.file_index, ifpe.line, ifpe.col));
		} else {
			// external node:-
			r.push_str(format!("\t\\{node_id:{:u},\tkind:\"{:s}\"\trspan\\{lo:{:u},hi:{:u}\\}\\}\n",*k,v.kind, *v.span.lo,*v.span.hi));
		}
	}
	r
}

impl ToJsonStrFc for FNodeInfoMap {
	fn to_json_str(&self,dc:&RFindCtx)->~str {
		~"[\n"+node_spans_table_to_json_sub(dc,self)+"]\n"
	}
}

impl ToJsonStr for hashmap::HashMap<ast::NodeId,ast::DefId> {
	fn to_json_str(&self)->~str {
		let mut r=~"[\n";
//		for self.iter().advance|(&key,&value)| {
		for (&key,&value) in self.iter() {
			r.push_str(format!("\t\\{node_id:{:?},\tdef_id:\\{crate:{:?},node:{:?}\\}\\},\n", key, value.crate,value.node));
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

//

impl KindToStr for ast::Decl {
	fn kind_to_str(&self)->&'static str {
		match self.node {
		ast::DeclLocal(_)=>"decl_local",
		ast::DeclItem(x)=>x.kind_to_str(),
		}
	}
}
impl KindToStr for ast::item {
	fn kind_to_str(&self)->&'static str {
		match (self.node) {
		ast::item_static(..)=>"static",
		ast::item_fn(..)=>"fn",
		ast::item_mod(_)=>"mod",
		ast::item_foreign_mod(_)=>"foreign_mod",
		ast::item_ty(..)=>"ty",
		ast::item_enum(..)=>"enum",
		ast::item_struct(..)=>"struct",
		ast::item_trait(..)=>"trait",
		ast::item_impl(..)=>"impl",
		ast::item_mac(_)=>"mac",
		}
	}
}
impl KindToStr for ast::Expr {
	fn kind_to_str(&self)->&'static str {
		match self.node {
		ast::ExprVstore(_,_)=>"vstore",
		ast::ExprVec(_,_)=>"vec",
		ast::ExprCall(_,_,_)=>"call",
		ast::ExprMethodCall(_,_,_,_,_,_)=>"method_call",
		ast::ExprTup(_)=>"tup",
		ast::ExprBinary(_, _binop, _,_)=>match _binop {
//			ast_util::binop_to_*(binop) todo - we donnt use this because of ambiguity
			ast::BiAdd=>"add",
			ast::BiSub=>"sub",
			ast::BiMul=>"mul",
			ast::BiDiv=>"div",
			ast::BiRem=>"rem",
			ast::BiAnd=>"and",
			ast::BiOr=>"or",
			ast::BiBitXor=>"bitxor",
			ast::BiBitAnd=>"bitand",
			ast::BiBitOr=>"bitor",
			ast::BiShl=>"shl",
			ast::BiShr=>"shr",
			ast::BiEq=>"eq",
			ast::BiLt=>"lt",
			ast::BiLe=>"le",
			ast::BiNe=>"ne",
			ast::BiGe=>"ge",
			ast::BiGt=>"gt",

		},
		ast::ExprUnary(_, unop, _)=>match unop {
			ast::UnBox(_)=>"box",
			ast::UnUniq=>"uniq",
			ast::UnDeref=>"deref",
			ast::UnNot=>"not",
			ast::UnNeg=>"neg"
		},
		ast::ExprLit(_)=>"lit",
		ast::ExprCast(_, _)=>"cast",
		ast::ExprIf(_,_,_)=>"if",

		ast::ExprWhile(_, _)=>"while",
		ast::ExprForLoop(_, _,_, _)=>"for_loop",
		ast::ExprLoop(_, _)=>"loop",
		ast::ExprMatch(_, _)=>"match",
		ast::ExprFnBlock(_, _)=>"fn_blk",
		ast::ExprProc(..) => "proc",
		ast::ExprDoBody(_)=>"do_body",
		ast::ExprBlock(_)=>"blk",
		ast::ExprAssign(_,_)=>"assign",
		ast::ExprAssignOp(_, binop, _, _)=>match binop {
			ast::BiAdd=>"assign_add",
			ast::BiSub=>"assign_sub",
			ast::BiMul=>"assign_mul",
			ast::BiDiv=>"assign_div",
			ast::BiRem=>"assign_rem",
			ast::BiAnd=>"assign_and",
			ast::BiOr=>"assign_or",
			ast::BiBitXor=>"assign_bitxor",
			ast::BiBitAnd=>"assign_bitand",
			ast::BiBitOr=>"assign_bitor",
			ast::BiShl=>"assign_shl",
			ast::BiShr=>"assign_shr",
			ast::BiEq=>"assign_eq",
			ast::BiLt=>"assign_lt",
			ast::BiLe=>"assign_le",
			ast::BiNe=>"assign_ne",
			ast::BiGe=>"assign_ge",
			ast::BiGt=>"assign_gt"
		},
		ast::ExprField(_, _, _)=>"field",
		ast::ExprIndex(_,_,_)=>"index",
		ast::ExprPath(_)=>"path",
		ast::ExprSelf=>"self",
		ast::ExprAddrOf(_, _)=>"addr_of",
		ast::ExprBreak(_)=>"break",
		ast::ExprAgain(_)=>"again",
		ast::ExprRet(_)=>"ret",
		ast::ExprLogLevel => "log",
		ast::ExprInlineAsm(_)=>"inline_asm",
		ast::ExprMac(_)=>"mac",
		ast::ExprStruct(_,_,_)=>"expr_struct",
		ast::ExprRepeat(_,_,_)=>"repeat",
		ast::ExprParen(_)=>"paren",
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
					ast::ty_path(_,_,NodeId)=>Some(NodeId),
					_ => self.get_id()
				},
			_ => self.get_id()
		}
	}
}
impl KindToStr for AstNode {
	fn kind_to_str(&self)->&'static str {
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
			astnode_trait_ref(_)=>"trait_ref",
			astnode_variant(_)=>"variant",
			astnode_root=>"root",
			astnode_none=>"none"
		}
	}
}

impl AstNodeAccessors for ast::item_ {
	fn get_id(&self)->Option<ast::NodeId> {
		match *self {
		ast::item_static(_,_,ref e) => Some(e.id),
		ast::item_fn(_, _, _, _, ref b)=>Some(b.id),
		ast::item_mod(_)=>None,
		ast::item_foreign_mod(_)=>None,
		ast::item_ty(ref ty,_)=>Some(ty.id),
		ast::item_enum(_,_)=>None,
		ast::item_struct(_,_)=>None,
		ast::item_trait(_,_,_)=>None,
		ast::item_impl(_,_,ref ty,_)=>Some(ty.id), //TODO, is this wrong, just node_id of a component?
		ast::item_mac(_)=>None,
		}
	}
	fn get_ident(&self)->Option<ast::Ident> {
		match *self {
		_=>None
		}
	}
}

impl AstNodeAccessors for ast::item {
	fn get_id(&self)->Option<ast::NodeId> {	Some(self.id)}
	fn get_ident(&self)->Option<ast::Ident> {	Some(self.ident) }
}

impl AstNodeAccessors for ast::Expr {
	fn get_id(&self)->Option<ast::NodeId> {	Some(self.id)}
	fn get_ident(&self)->Option<ast::Ident> {	None }
}


impl AstNodeAccessors for ast::Pat {
	fn get_id(&self)->Option<ast::NodeId> {	Some(self.id)}
	fn get_ident(&self)->Option<ast::Ident> { None
/*		match *self {
		pat_ident=>None,
		pat_enum(_,_)=>None,
		pat_tup(_)=>None,
		pat_box(x)=>None,
		pat_uniq(x)=>None,
		pat_region(x)=>None,
		pat_lit(e)=>N
		_=>None,
		}
*/
	}

}

impl AstNodeAccessors for ast::Local {
	fn get_id(&self)->Option<ast::NodeId> {	Some(self.id)}
	fn get_ident(&self)->Option<ast::Ident> {	None }
}

impl AstNodeAccessors for ast::Decl_ {
	fn get_id(&self)->Option<ast::NodeId> {
		match *self{
			ast::DeclLocal(ref x)=>Some(x.id),
			ast::DeclItem(ref x)=>Some(x.id)
		}
	}
	fn get_ident(&self)->Option<ast::Ident> {
		match *self {
			ast::DeclLocal(l)=>l.get_ident(),
			ast::DeclItem(l)=>l.get_ident()
		}
	}
}

impl<T:AstNodeAccessors> AstNodeAccessors for codemap::Spanned<T> {
	fn get_id(&self)->Option<ast::NodeId> {
		self.node.get_id()
	}
	fn get_ident(&self)->Option<ast::Ident> { self.node.get_ident() }
}
//impl AstNodeAccessors for ty_method {
//	pub fn get_id(&self)->Option<NodeId> {
//		Some(self.id)
//	}
//}
impl AstNodeAccessors for ast::Block {
	fn get_id(&self)->Option<ast::NodeId> {
		Some(self.id)
	}
	fn get_ident(&self)->Option<ast::Ident> { None }
}
impl AstNodeAccessors for ast::Stmt_ {
	fn get_id(&self)->Option<ast::NodeId> {
		match *self {
			ast::StmtDecl(_,x)=>Some(x),
			ast::StmtExpr(_,x)=>Some(x),
			ast::StmtSemi(_,x)=>Some(x),
			ast::StmtMac(_,_)=>None
		}
	}
	fn get_ident(&self)->Option<ast::Ident> { None }
}

impl AstNodeAccessors for ast::view_item {
	fn get_id(&self)->Option<ast::NodeId> { self.node.get_id() }
	fn get_ident(&self)->Option<ast::Ident> { self.node.get_ident() }
}
impl AstNodeAccessors for ast::ty_ {
	fn get_id(&self)->Option<ast::NodeId> { None }
	fn get_ident(&self)->Option<ast::Ident> { None }
}
impl AstNodeAccessors for ast::Ty {
	fn get_id(&self)->Option<ast::NodeId> { Some(self.id) }
	fn get_ident(&self)->Option<ast::Ident> { self.node.get_ident() }
}

impl AstNodeAccessors for ast::view_item_ {
	fn get_id(&self)->Option<ast::NodeId> {
		match *self {
			ast::view_item_extern_mod(_,_,_,node_id)=>Some(node_id),
			ast::view_item_use(_)=>None
		}
	}
	fn get_ident(&self)->Option<ast::Ident> {
		match *self {
			ast::view_item_extern_mod(ident,_,_,_)=>Some(ident),
			ast::view_item_use(_)=>None
		}
	}
}
impl AstNodeAccessors for ast::variant_ {
	fn get_id(&self)->Option<ast::NodeId> { Some(self.id) }
	fn get_ident(&self)->Option<ast::Ident> { Some(self.name) }
}
impl AstNodeAccessors for ast::TypeMethod {
	fn get_id(&self)->Option<ast::NodeId> { Some(self.id) }
	fn get_ident(&self)->Option<ast::Ident> { Some(self.ident) }
}

impl AstNodeAccessors for ast::method {
	fn get_id(&self)->Option<ast::NodeId> { Some(self.id) }
	fn get_ident(&self)->Option<ast::Ident> { Some(self.ident) }
}
impl AstNodeAccessors for ast::struct_def {
	fn get_id(&self)->Option<ast::NodeId> { None }
	fn get_ident(&self)->Option<ast::Ident> { None }
}
impl AstNodeAccessors for ast::trait_ref {
	fn get_id(&self)->Option<ast::NodeId> { None }
	fn get_ident(&self)->Option<ast::Ident> { None }
}


impl AstNodeAccessors for ast::struct_field_ {
	fn get_id(&self)->Option<ast::NodeId> {
		Some(self.id)
	}
	fn get_ident(&self)->Option<ast::Ident> {
		match self.kind{
			ast::named_field(ident,_)=>Some(ident),
			ast::unnamed_field => None
		}
	}
}

impl AstNodeAccessors for ast::trait_method {
	fn get_id(&self)->Option<ast::NodeId> {
		match(*self) {
			ast::required(ref m)=>Some(m.id),
			ast::provided(_)=>None
		}
	}
	fn get_ident(&self)->Option<ast::Ident> {
		match *self {
		ast::required(ref tym)=> tym.get_ident(),
		ast::provided(ref m)=>m.get_ident()
		}
	}
}
impl AstNodeAccessors for ast::_mod  {
	fn get_id(&self)->Option<ast::NodeId>{ None }
	fn get_ident(&self)->Option<ast::Ident>{ None }
}

impl AstNodeAccessors for AstNode {
	fn get_id(&self)->Option<ast::NodeId> {
		// todo - should be option<node_id> really..
		match *self {
			astnode_mod(_) => None,
			astnode_view_item(ref x) =>x.node.get_id(),
			astnode_item(ref x) =>Some(x.id),
			astnode_local(ref x) =>Some(x.id),
			astnode_block(_)=>None,
			astnode_stmt(_)=>None,
			astnode_arm(_)=>None,
			astnode_pat(ref x)=>Some(x.id),
			astnode_decl(ref x)=>x.get_id(),
			astnode_expr(ref x)=>Some(x.id),
//			astnode_expr_post(ref x)=>Some(x.id),
			astnode_ty(ref x)=>Some(x.id),
			astnode_ty_method(ref x)=>Some(x.id),
			astnode_trait_method(ref x)=>x.get_id(),
			astnode_method(ref m)=>Some(m.id),
			astnode_struct_def(_)=>None,
			astnode_struct_field(ref x)=>Some(x.node.id),
			astnode_trait_ref(ref x)=>Some(x.ref_id),
			astnode_variant(ref x)=>Some(x.node.id),
			astnode_none|astnode_root=>None,
		}
	}
	fn get_ident(&self)->Option<ast::Ident> {
		// todo - should be option<node_id> really..
		match *self {
			astnode_mod(ref x) => x.get_ident(),
			astnode_view_item(ref x) =>x.get_ident(),
			astnode_item(ref x) =>x.get_ident(),
			astnode_local(ref x) =>x.get_ident(),
			astnode_block(_)=>None,
			astnode_stmt(_)=>None,
			astnode_arm(_)=>None,
			astnode_pat(ref x)=>x.get_ident(),
			astnode_decl(ref x)=>x.get_ident(),
			astnode_expr(ref x)=>x.get_ident(),
//			astnode_expr_post(ref x)=>Some(x.id),
			astnode_ty(ref x)=>x.get_ident(),
			astnode_ty_method(ref x)=>x.get_ident(),
			astnode_trait_method(ref x)=>x.get_ident(),
			astnode_method(ref m)=>m.get_ident(),
			astnode_struct_def(ref x)=>x.get_ident(),
			astnode_struct_field(ref x)=>x.get_ident(),
			astnode_trait_ref(ref x)=>x.get_ident(),
			astnode_variant(ref x)=>x.get_ident(),
			astnode_none|astnode_root=>None,
		}
	}
}
fn item_get_ident(a:&ast::item)->Option<ast::Ident> { Some(a.ident) }

fn decl_get_ident(a:&ast::Decl)->Option<ast::Ident> {
	match a.node {
		ast::DeclLocal(_)=> None, // todo - will we need the ident ?a local isn't always 1, due to tuples
		ast::DeclItem(i)=> item_get_ident(i)
	}
}
fn expr_get_ident(_ :&ast::Expr)->Option<ast::Ident> {
	None
}


struct FindAstNodeSt {
	result: NodeTreeLoc,		// todo - full tree path, all the parent nodes.
	location: u32,
	stop: bool,
//	node_spans: HashMap<ast::node_id,codemap::span>
}

pub fn get_ast_node_of_node_id(info:&FNodeInfoMap,id:ast::NodeId)->Option<AstNode> {
	match info.find(&id) {
		None=>None,
		Some(node_info)=>Some(node_info.node)
	}
}

pub fn push_span(spt:&mut FNodeInfoMap,n:ast::NodeId,p:ast::NodeId, _:Option<ast::Ident>,k:&str, s:codemap::Span,nd:AstNode) {
	spt.insert(n,FNodeInfo{ident:nd.get_ident(), kind:k.to_str(),span:s,node:nd, parent_id:p});
}

pub fn push_spanned<T:AstNodeAccessors>(spt:&mut FNodeInfoMap,k:&str,s:&codemap::Spanned<T>,nd:AstNode,p:ast::NodeId) {
	match s.node.get_id() {
		Some(id)=>{spt.insert(id,FNodeInfo{ident:nd.get_ident(), kind:k.to_str(),span:s.span,node:nd, parent_id:p});}
		None=>{}
	}
}

pub fn span_contains(x: u32, s: codemap::Span)->bool {
	x>=*s.lo && x<*s.hi
}

pub struct FncsThing;

impl FncsThing {
	pub fn trait_ref(&mut self, tr:&ast::trait_ref, (s, p):FncsState) {
		push_span(s, tr.ref_id, p,None, "trait_ref", tr.path.span, astnode_trait_ref(@tr.clone()));
	}

	pub fn variant(&mut self, va:&ast::variant, (s, p):FncsState) {
		push_span(s, va.node.id,p, Some(va.node.name),"variant", va.span, astnode_variant(@va.clone()))
//		 visit_item(va,(s,va.node.id,v)) - TODO , are we actually suppoed to iterate here? why was't it done
	}
}

type FncsState =(@mut FNodeInfoMap, ast::NodeId);

impl Visitor<FncsState> for FncsThing {
	// use default impl
//	 fn visit_view_item(&mut self, a:&ast::view_item, (s, p):FncsState) {
//		 walk_view_item(self, a, s);
//	 }

	fn visit_generics(&mut self, g:&ast::Generics, (s, p):FncsState) {
		for typ in g.ty_params.iter() {
			// unfortunately no span for type param
//			 push_span(s, g.def_id.id, Some(g.ident), "ty_param_def", g.def_id.span, astnode_ty_param_def(tp))
			for type_bound in typ.bounds.iter() {
				match (type_bound) {
					&ast::TraitTyParamBound(ref tr) => {
						self.trait_ref(tr, (s, p));
					}
					_ => {}
				}
			}
		}

		visit::walk_generics(self, g, (s, p));
	}

	fn visit_item(&mut self, a:@ast::item, (s, p):FncsState) {
		push_span(s,a.id,p,item_get_ident(a),a.kind_to_str(),a.span,astnode_item(a));

		// TODO: Push nodes for type-params... since we want to click on their defs...
		match a.node {
			ast::item_impl(_, ref o_traitref, _, ref methods) => {
//				 self.visit_generics(g, (s, p));
				match *o_traitref {
					None => {}
					Some(ref tr) => self.trait_ref(tr, (s, p)),
				}

				for m in methods.iter() {
					push_span(s, m.id, p, Some(a.ident), "method", m.span, astnode_method(*m));
				}
			}
			ast::item_enum(ref ed, _) => {
				for v in ed.variants.iter() {
					self.variant(*v, (s,p));
				}
			}
			ast::item_trait(_, ref tr, _) => {
				for t in tr.iter() {
					self.trait_ref(t, (s, p));
				}
			}
			_ => {}
		}

		visit::walk_item(self, a, (s, a.id));
	}

	fn visit_local(&mut self, a:@ast::Local, (s, p):FncsState) {
		push_span(s, a.id, p, None, "local", a.span, astnode_local(a));

		visit::walk_local(self, a, (s, a.id));
	}

	fn visit_block(&mut self, a: @ast::Block, (s, p): FncsState) {
		push_span(s, a.id, p, None, "block", a.span, astnode_none);

		visit::walk_block(self, a, (s, a.id));
	}

	fn visit_stmt(&mut self, a:@ast::Stmt, (s, p):FncsState) {
		push_spanned(s, "stmt", a, astnode_stmt(a), p);

		visit::walk_stmt(self, a, (s, p));
	}

	// we do nothing yet, use default impl
//	 fn visit_arm(&mut self, a:&ast::Arm, (s, p):FncsState) {}

	fn visit_pat(&mut self, a: &ast::Pat, (s, p):FncsState) {
		push_span(s, a.id, p, None, "pat", a.span, astnode_pat(@a.clone()));

		visit::walk_pat(self, a, (s, a.id));
	}

	fn visit_decl(&mut self, a:@ast::Decl, (s, p):FncsState) {
		push_spanned(s, "decl", a, astnode_decl(a), p);

		visit::walk_decl(self, a, (s, p));
	}
	// we do nothing, use default for now
//	 fn visit_struct_def(&mut self, s)

	fn visit_expr(&mut self, a:@ast::Expr, (s, p):FncsState) {
		push_span(s, a.id, p, expr_get_ident(a), a.kind_to_str(), a.span, astnode_expr(a));

		visit::walk_expr(self, a, (s, a.id));
	}

	// default, we do nothing
//	 fn visit_expr_post()

	fn visit_ty(&mut self, a:&ast::Ty, (s, p):FncsState) {
		push_span(s, a.id, p, None, "ty", a.span, astnode_ty(@a.clone()));

		visit::walk_ty(self, a, (s, a.id));
	}

	// default, we do nothing
//	 fn visit_fn()

	fn visit_struct_field(&mut self, a: &ast::struct_field, (s, p):FncsState) {
		push_spanned(s, "struct_field", a, astnode_struct_field(@a.clone()), p);

		visit::walk_struct_field(self, a, (s, p));
	}

	fn visit_ty_method(&mut self, a:&ast::TypeMethod, (s, p):FncsState) {
		push_span(s, a.id, p, Some(a.ident), "type_method", a.span, astnode_ty_method(@a.clone()));

		visit::walk_ty_method(self, a, (s, a.id));
	}
}

pub struct Finder;

impl Visitor<@mut FindAstNodeSt> for Finder {
	fn visit_view_item(&mut self, a:&ast::view_item, s:@mut FindAstNodeSt) {
		if span_contains(s.location, a.span) {
			s.result.push(astnode_view_item(@a.clone()));;
		}
		visit::walk_view_item(self, a, s);
	}

	fn visit_item(&mut self, a:@ast::item, s:@mut FindAstNodeSt) {
		if span_contains(s.location, a.span) {
			s.result.push(astnode_item(a));
		}
		visit::walk_item(self, a, s);
	}

	fn visit_local(&mut self, a:@ast::Local, s:@mut FindAstNodeSt) {
		if span_contains(s.location, a.span) {
			s.result.push(astnode_local(a));
		}

		visit::walk_local(self, a, s);
	}

	fn visit_block(&mut self, a:@ast::Block, s:@mut FindAstNodeSt) {
		if span_contains(s.location, a.span) {
			s.result.push(astnode_block(a));
		}

		visit::walk_block(self, a, s);
	}

	fn visit_stmt(&mut self, a:@ast::Stmt, s:@mut FindAstNodeSt) {
		if span_contains(s.location, a.span) {
			s.result.push(astnode_stmt(a));
		}

		visit::walk_stmt(self, a, s);
	}

	fn visit_arm(&mut self, a:&ast::Arm, s:@mut FindAstNodeSt) {
		   // The whole arm doesn't have a span.
//		 if span.contains(s.location, a.span) {
//			 s.result.push(astnode_pat(a));
//		 }
		visit::walk_arm(self, a, s);
	}

	fn visit_pat(&mut self, a: &ast::Pat, s:@mut FindAstNodeSt) {
		if span_contains(s.location, a.span) {
			s.result.push(astnode_pat(@a.clone()));
		}

		visit::walk_pat(self, a, s);
	}

	fn visit_decl(&mut self, a:@ast::Decl, s:@mut FindAstNodeSt) {
		if span_contains(s.location, a.span) {
			s.result.push(astnode_decl(a));
		}

		visit::walk_decl(self, a, s);
	}

	fn visit_expr(&mut self, a:@ast::Expr, s:@mut FindAstNodeSt) {
		if span_contains(s.location, a.span) {
			s.result.push(astnode_expr(a));
		}

		visit::walk_expr(self, a, s);
	}

	fn visit_ty(&mut self, a:&ast::Ty, s:@mut FindAstNodeSt) {
		if span_contains(s.location, a.span) {
			s.result.push(astnode_ty(@a.clone()));
		}

		visit::walk_ty(self, a, s);
	}

	// use default impl for now as we don't do anything here
//	 fn visit_fn(fk:&vist::fn_kind, fd:&as::fn_decl, body:&ast::Block,
//		 sp:codemap::Span, nid:ast::NodeId, s: @mut FindAstNodeSt) {}

	fn visit_struct_field(&mut self, a: &ast::struct_field, s:@mut FindAstNodeSt) {
		if span_contains(s.location, a.span) {
			s.result.push(astnode_struct_field(@a.clone()));
		}

		visit::walk_struct_field(self, a, s);
	}
}

pub fn get_node_info_str(dc:&RFindCtx,node:&NodeTreeLoc)->~str
{
	fn path_to_str(dc:&RFindCtx, path:&ast::Path)->~str {
		let mut acc=~"";
		let mut first=true;
//		for path.idents.iter().advance |x|{
		for x in path.segments.iter() {
			if !first {acc=acc.append("::");}
			acc=acc.append(dc.sess.str_of(x.identifier));
			first=false
		}
		acc
		// typeparams too... path.types?
	}
	fn pat_to_str(dc:&RFindCtx,p:&ast::Pat)->~str{
		// todo -factor out and recurse
		match p.node {
			ast::PatIdent(_, ref path, _)=>~"pat_ident:"+path_to_str(dc,path),
			ast::PatEnum(ref path, _)=>~"pat_enum:"+path_to_str(dc,path),//	`todo-fields..
			ast::PatStruct(ref path,ref sfields,_)=>~"pat_struct:"+path_to_str(dc,path)+"{"+sfields.map(|x|pat_to_str(dc,x.pat)+",").to_str()+"}",
			ast::PatTup(ref elems)=>~"pat_tupl:"+elems.map(|&x|pat_to_str(dc,x)).to_str(),
			//ast::pat_box(ref box)=>~"box",
			ast::PatUniq(..)=>~"uniq",
			ast::PatRegion(..)=>~"rgn",
			ast::PatLit(..)=>~"literal",
			ast::PatRange(..)=>~"range",

			_=>~"?"
		}
	};
	fn ty_to_str(dc:&RFindCtx,t:&ast::Ty)->~str{
		match t.node{
			ast::ty_nil=> ~"nil",
			ast::ty_bot=>~"bottomtype",
			ast::ty_box(..)=>~"box",
			ast::ty_vec(..)=>~"vec",
			ast::ty_fixed_length_vec(..)=>~"[T,..N]",
			ast::ty_ptr(..)=>~"*",
			ast::ty_rptr(..)=>~"&",
			ast::ty_tup(ref types)=>~"("+types.map(|x|ty_to_str(dc,*x)).to_str()+")", //todo: factor this out, map..
			ast::ty_path(ref path, _, node_id)=>~"path:id="+node_id.to_str()+" "+path_to_str(dc,path)
			,

			ast::ty_infer=>~"infered",
			_ =>~"?"
		}
	}
	fn expr_to_str(dc:&RFindCtx, x:&ast::Expr_)->~str {
		match *x {
			ast::ExprStruct(ref p,_,_)=>~"(expr_struct "+ path_to_str(dc,p) +")",
			ast::ExprCall(ref e,ref args,_)=>~"(expr_call("+expr_to_str(dc,&e.node)+args.map(|x|expr_to_str(dc,&x.node)).to_str()+")",
			ast::ExprField(ref e, ref i, ref tys)=>~"(expr_field("+expr_to_str(dc,&e.node)+")"+dc.sess.str_of(*i)+tys.map(|x|ty_to_str(dc,*x)).to_str()+")",
			_=>~"expr"
		}
	}


	match node.last() {
//			TODO -factor out repeatedly used functions here..
//			fn astnode_pat_to_str(&astnode_pat(x))->~str
//			fn path_to_str(&astnode_pat(x))->~str
//			fn expr_to_str(&astnode_pat(x))->~str

		&astnode_view_item(_)=>~"view_item: ?",
		&astnode_item(x)=>~"item: "+
			"id="+x.id.to_str()+" "+
			dc.sess.str_of(x.ident)+
			match x.node {
				ast::item_fn(_,_,_,_,_) =>~" fn_decl",
				ast::item_struct(_, _) =>~" struct_def",
				_=>~"item_unknown"
			},

		&astnode_local(_)=>~"local: ?",
		&astnode_block(_)=>~"block: ?",
		&astnode_stmt(_)=>~"stmt: ?",
		&astnode_arm(_)=>~"arm: ?",
		&astnode_struct_field(sf)=>
			"id="+sf.node.id.to_str()+" "+
			match(sf.node.kind){
				ast::named_field(nf, _)=>"struct named_field: "+dc.sess.str_of(nf)+" ",
				_=>~"struct anon_field"
			}+
			":"+ty_to_str(dc, sf.node.ty)/*sf.node.ty ..parse it.. */,
		&astnode_pat(p)=>"pattern: "+
			"id="+p.id.to_str()+" "+
			pat_to_str(dc,p)
		,
		&astnode_decl(_)=>~"decl: ?",
		&astnode_ty(x)=>~"type: "+ty_to_str(dc,x),
		&astnode_struct_def(_)=>~"struct def",
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

pub fn get_def_id(curr_crate:ast::CrateNum,src_def:ast::Def)->Option<ast::DefId> {
	let mk=|x|{Some(ast::DefId{crate:curr_crate, node:x})}; // todo,mmaybe this is best 'None'..
	// todo-'definition' can be at multiple locations. we should return [def_id] really..
	match (src_def) {
		ast::DefFn(d,_)=>Some(d),
		ast::DefStaticMethod(d,_,_)=>Some(d),
		ast::DefSelf(id, _)=>mk(id),
		ast::DefSelfTy(id)=>mk(id),
		ast::DefMod(d)=>Some(d),
		ast::DefForeignMod(d)=>Some(d),
		ast::DefStatic(d,_)=>Some(d),
		ast::DefArg(id,_)=>mk(id),
		ast::DefLocal(id,_)=>mk(id),
		ast::DefVariant(_, d2, _)=>Some(d2),
		ast::DefTy(d)=>Some(d),
		ast::DefTrait(d)=>Some(d),
		ast::DefPrimTy(_)=>None,
		ast::DefTyParam(d,_)=>Some(d),
		ast::DefBinding(d,_)=>mk(d),
		ast::DefUse(d)=>Some(d),
		ast::DefUpvar(_,d,_,_)=>get_def_id(curr_crate,*d),
		ast::DefStruct(d)=>Some(d),
		ast::DefTyParamBinder(id)=>mk(id),
		ast::DefRegion(id)=>mk(id),
		ast::DefLabel(id)=>mk(id),
		ast::DefMethod(d,_)=>Some(d)
	}
}

/*
impl ToJsonStr for JumpToDefMap {
	pub fn to_json_str(&self)->~str {
		let mut acc=~"";
		for (&k,&v) in self.iter() {
			acc.push_str("\t\t{ node_id:"+k.to_str()+", def_node_id:"+ v.to_str()+" },\n");
		}
		acc
	}
}
*/

pub fn byte_pos_from_text_file_pos_str(dc:&RFindCtx,filepos:&str)->Option<codemap::BytePos> {
	let toks=filepos.split(':').to_owned_vec();
	if toks.len()<3 { return None; }
//	let t0:()=toks[0];

//	let line:Option<uint> = FromStr::from_str(toks[1]);
//	if_some!(line in FromStr::from_str(toks[1]) then {
//		if_some!(col in FromStr::from_str(toks[2]) then {
	let line: Option<u32> = from_str(toks[1]);
	let col:Option<u32> = from_str(toks[2]);
	if line.is_some() && col.is_some() {
		//todo - if no column specified, just lookup everything on that line!
		let l0 = line.unwrap()-1;
		let c0= col.unwrap()-1;
		let foo= ZTextFilePos::new(toks[0],l0,c0).to_byte_pos(dc.tycx);
		return foo;
	}
	return None;
}


pub fn build_node_def_node_table(dc:&RFindCtx)->~HashMap<ast::NodeId, ast::DefId>
{
	let mut r=~HashMap::new();
	let curr_crate_id_hack=0;	// TODO WHAT IS CRATE ID REALLY?!
	// todo .. for range(0,c.next_id) || ??
	let mut id: ast::NodeId=0;
	while id<*(dc.tycx.next_id) as ast::NodeId {
		if_some!(t in safe_node_id_to_type(dc.tycx,id) then {
			if_some!(def in dc.tycx.def_map.find(&(id)) then { // finds a def..
				if_some!(did in get_def_id(curr_crate_id_hack,*def) then {
					r.insert(id as ast::NodeId,did);
				})
			});
		});
		id+=1;
	}
	r
}


pub fn def_node_id_from_node_id(dc:&RFindCtx, id:ast::NodeId)->ast::NodeId {
	let crate_num=0;	// TODO - whats crate Id really???
	match dc.tycx.def_map.find(&id) { // finds a def..
		Some(a)=>{
			match get_def_id(crate_num,*a) {
				Some(b)=>b.node,
				None=>id
			}
		},
		None=>(id)	// no definition? say its its own definition
	}
}


pub fn def_of_symbol_to_str(_:&RFindCtx, _:&FNodeInfoMap, _:&HashMap<ast::NodeId, ast::DefId>, _:&str)->~str {
	~"TODO"
}


// TODO- this should return a slice?
pub fn get_node_source(c:ty::ctxt, nim:&FNodeInfoMap, did:ast::DefId)->~str {
	if did.crate==0{
		match (nim.find(&did.node)){
			None=>~"",
			Some(info)=>{
				get_span_str(c,&info.span)
			}
		}
	} else {
		"{out of crate def:"+did.to_str()+"}"
	}
}


pub fn dump_node_source_for_single_file_only(text:&[u8], ns:&FNodeInfoMap, id:ast::NodeId) {
	match(ns.find(&id)) {None=>logi!("()"),
		Some(info)=>{
			dump_span(text, &info.span);
		}
	}
}
