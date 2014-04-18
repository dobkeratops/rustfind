use rf_common::*;
pub use syntax::{ast,abi};
pub use syntax::visit;
pub use syntax::parse::token;
pub use syntax::visit::{Visitor};
pub use syntax::codemap;
pub use syntax::codemap::BytePos;
pub use rfindctx::*;
//use rustc::middle::mem_categorization::ast_node;
use rustc::middle::ty;
use rfindctx::{RustFindCtx,};
use codemaput::{ZTextFilePos,ToZIndexFilePos,dump_span,get_span_str};
//pub use visit_rust_ast::FNodeInfoMapBuilder;
//use visit_rust_ast;
pub use visit_rust_ast::{FNodeInfoMapBuilder,push_span,push_spanned,FNodeInfoMapBuilder_new,Finder};

// find_ast_node

/*
 * Builds a mirror of the rust compiler AST with node wrappers
 * with extra conviniences oriented toward tools -
 * wrapper enum for universal node type, maps to get node from Id or spans.
 *
 * setup for convinience not efficiency, and isolates rustfind tool from
 * changes in the compiler sourcecode.
 */

pub macro_rules! logi{
    ($($a:expr),*)=>(println!("{}", $($a.to_str())*) )
}

pub type AstSPtr<T> =ast::P<T>;

// Wrappers to prepare for de-@
#[inline]
pub fn 	mkAstSPtr<T>(a:@T)->AstSPtr<T> {
	a
}
// Wrappers to prepare for de-@
#[inline]
pub fn 	mkAstSPtrClone<T: 'static + Clone>(a:&T)->AstSPtr<T> {
	ast::P((*a).clone())
}
fn 	mkAstSPtr2<T: 'static>(a:T)->AstSPtr<T> {
	ast::P(a)
}



// todo: looks like RustC does have this, "syntax::ast_map::Node" ?
// refactor ou code to use that.
/// wrapper enum for the varios ast:: node types.

#[deriving(Clone,Eq,TotalEq,Hash)]
pub enum CG_Kind {	// typedef this, so we can swap in a rust identifier. its *not* the ast object itself.
	CG_Trait,CG_Struct,CG_Enum,CG_Fn,CG_Mod, CG_None
}

#[deriving(Clone)]
pub enum AstNode_ { 
	// todo: CamelCase
    astnode_mod(AstSPtr<ast::Mod>),
    astnode_view_item(AstSPtr<ast::ViewItem>),
    astnode_item(AstSPtr<ast::Item>),
    astnode_local(AstSPtr<ast::Local>),
    astnode_block(AstSPtr<ast::Block>),
    astnode_stmt(AstSPtr<ast::Stmt>),
    astnode_arm(AstSPtr<ast::Arm>),
    astnode_pat(AstSPtr<ast::Pat>),
    astnode_decl(AstSPtr<ast::Decl>),
    astnode_expr(AstSPtr<ast::Expr>),
    astnode_ty(AstSPtr<ast::Ty>),
    astnode_ty_method(AstSPtr<ast::TypeMethod>),
    astnode_trait_method(AstSPtr<ast::TraitMethod>),
    astnode_method(AstSPtr<ast::Method>),
    astnode_struct_def(AstSPtr<ast::StructDef>),
    astnode_struct_field(AstSPtr<ast::StructField>),
    astnode_trait_ref(AstSPtr<ast::TraitRef>),
    astnode_variant(AstSPtr<ast::Variant_>),
    astnode_root,
    astnode_none
}

/// Master 'universal node' structure. Contains references to the
/// AST's own nodes via 'AstNode_' wrapper enum.
/// allows stepping back through heirachy via parent links.
/// Allows unified access to various properties that many nodes share, 
/// through acessor functions returning options, eg. ident ..

/// This will be refactored into something like clang 'Cursor', more like an iterator.
#[deriving(Clone)]
pub struct FNodeInfo {
//	pub id:	ast::NodeId,					// id of this node.
//    pub ident:Option<ast::Ident>,
    pub kind:~str,
    pub span:codemap::Span,
    pub node:AstNode_,			// todo- get rid of this and just make this a cache of spans for linking.
    pub parent_id:ast::NodeId,	// todo: vector of child nodes aswell?
	pub children:Vec<ast::NodeId>,
}

//pub struct FNodeInfoMapBuilder {
//	pub all_nodes: FNodeInfoMap
//}






//.. TODO - does it make sense to cache an ident here? not all nodes have one

type SPtr<T> =@T;	// temporary to be replaced later.

/// implementations for FNodeInfo, convinience accessors.
/// refactor rustfinds' accessors to our AST wrappers
/// we suspect either these are already available, or they will be added in time.
/// use rf_ prefix for methods for ease of search replace when we know what the
/// best way is !
impl FNodeInfo {
/*
	pub fn visit_children(&self, all_nodes:&FNodeInfoMap, f:|all_nodes:&FNodeInfoMap,node:&FNodeInfo|) {
		for id in all_nodes.children_of(self.id).iter() {
			all_nodes.find(id).map(	|x|{ f(all_nodes, x)} );
		}
	}
*/

	pub fn rf_get_ident(&self)->Option<ast::Ident> { self.node.rf_get_ident()}
	pub fn rf_get_id(&self)->ast::NodeId {
		// hmm, this starts to look wrong.
		let id=self.node.rf_get_id();
		if !id.is_some() {fail!("tried to get ident on node without ident, code design is wrong");}
		return id.unwrap();
	}
	pub fn rf_kind<'a> (&'a self)->&'a str { self.kind.as_slice() }
	pub fn rf_span(&self)->codemap::Span { self.span }
	pub fn rf_node(&self)->AstNode_ { self.node }
	pub fn rf_get_parent_id(&self)->Option<ast::NodeId> { Some(self.parent_id) } // surely the root node doesn't have a parent. Is the root node in the same system ?
	pub fn rf_visit_children(&self, nim:&FNodeInfoMap,  f:|node_id:ast::NodeId, node_info:&FNodeInfo|) {
		for nid in self.children.iter() {
			match nim.find(nid) {
				Some(node)=> f(*nid, node),
				_=>{},
			}
			
		}
	}


//	pub fn rf_get_ident(&self)->Option<ast::Ident> { self.ident}

	// TODO: Macro to write these acessors, its just a simple pattern.
	// suggestion for rustc.. macros to switch idents from CamelCase to snake_case and back..

	/// Check if the contained node is a function declaretion, return it or not.
	pub fn rf_as_item<'a>(&'a self)->Option<AstSPtr<ast::Item>> {
		match self.node
		{
			astnode_item(item)=>Some(item),
			_=>None,
		}
	}
	pub fn rf_as_ast_node<'a>(&'a self)->&'a AstNode_ {
		&self.node
	}

	/// Single wrapper to acess as function declaration.
	/// this is a lot of boilerplate, probably more code overall,
	/// but seems to make some tasks easier elsewhere.

	pub fn rf_as_fn_decl<'a>(&'a self)->
			Option<(AstSPtr<ast::Item>,
				(ast::P<ast::FnDecl>, ast::Purity, abi::Abi, ast::Generics, ast::P<ast::Block>)
				)>
	{
		let x=self.rf_as_item().map(
			|item|{
				match item.node{
					ast::ItemFn(ref decl,ref purity,ref abi,ref generics,ref block)
					=>{
						Some( (item,(decl.clone(),purity.clone(),abi.clone(),generics.clone(),block.clone())) )
					},
					_=>None,
				}
			}
		); 
		// we have Some<Some<WhatWeWant>> | None ... unwrap it ..
		// seems this was actually inelegant, should we have just stuck with the two matchs'.
		match x {
			Some(Some(y))=>Some(y),
			_=>None
		}
		
	}
	pub fn rf_is_fn_decl(&self)->bool { self.rf_as_fn_decl().is_some()}
	pub fn rf_as_expr<'a>(&'a self)->Option<(AstSPtr<ast::Expr>)> {
		match self.node {
			astnode_expr(expr)=>Some(expr),
			_=>None
		}
	}
	pub fn rf_is_expr(&self)->bool { self.rf_as_expr().is_some() }
/*
	pub fn as_fn_call<'a>(&'a self)->Option<_> {
		match self.as_expr() {
			Some(expr) {
				match (src
			}
			None=>None
		}
	}
*/
}

//pub type FNodeInfoMap= HashMap<ast::NodeId,FNodeInfo>;

/// Wrapper for holder of nodes, TODO - refactoring away from our local copy of the ast, wrap a cursor around theirs.
pub struct FNodeInfoMap {
	pub fni_hashmap:HashMap<ast::NodeId,FNodeInfo>
}


impl FNodeInfoMap {
	pub fn new()->FNodeInfoMap{
		FNodeInfoMap{ fni_hashmap: HashMap::new() }
	}
	pub fn find<'a>(&'a self,id:&ast::NodeId)->Option<&'a FNodeInfo> {
		self.fni_hashmap.find(id)
	}
	pub fn find_mut<'a> (&'a mut self,id:&ast::NodeId)->Option<&'a mut FNodeInfo> {
		self.fni_hashmap.find_mut(id)
	}
	pub fn insert(&mut self, id:ast::NodeId, v:FNodeInfo) {
		self.fni_hashmap.insert(id,v);
	}
	pub fn find_or_insert<'a>(&'a mut self, id:ast::NodeId, v:FNodeInfo)->&'a mut FNodeInfo {
		self.fni_hashmap.find_or_insert(id,v)
	}
	pub fn len(&self)->uint { self.fni_hashmap.len() }
	pub fn iter<'a>(&'a self)->::collections::hashmap::Entries<'a,ast::NodeId, FNodeInfo>  {
		self.fni_hashmap.iter()
	}

}


static mut g_root_node:Option<ast::NodeId> =None;
pub fn rf_set_root_node<'a> (nim:&'a FNodeInfoMap, root_node: ast::NodeId) {
	unsafe {
		// todo -lock!
		match g_root_node {
			Some(node)=> {
				fail!("only one root node, its a global hack till we refactor out");
				 
			}
			None=> { g_root_node = Some(root_node);
			}
		}
	}
}
pub fn rf_clear_root_node<'a> (nim:&'a FNodeInfoMap) {
	unsafe { g_root_node=None }
}

pub fn rf_get_root_node<'a> (nim:&'a FNodeInfoMap)->Option<(ast::NodeId,&'a FNodeInfo)> {
	unsafe {
		match g_root_node {
			None=>{ fail!("must have a root node set")},
			Some(nid)=>Some( (nid, nim.find(&nid).unwrap()) )
		}		
	}
}
/*
	match nim.iter().nth(0) {
		None=> None,
		Some(( ref mut id, ref mut v))=>{
			while true {
				match nim.find(v.parent_id) {
					None=> return Some((id,v)),
					Some(ref node) =>{
						id=v.parent_id; v=node;
					}
				}
			}
		}
	}
*/


pub type NodeTreeLoc = ~[AstNode_];
pub fn dump_node_tree_loc(ndt:&NodeTreeLoc) {
//  for ndt.iter().advance |x|
    for x in ndt.iter()
     {println!("{}.", x.kind_to_str());}
}


pub trait AstNodeAccessors {
    fn rf_get_id(&self)->Option<ast::NodeId>;
    fn rf_get_ident(&self)->Option<ast::Ident>;
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
    (   {   let mut txt=file!()+":"+line!().to_str()+": " ;
            $( txt=txt.append(
                fmt!("%s=%?",stringify!($a),$a)+",")
            );*;
            println!(txt);
        }
    )
}

/// main
pub fn find_node_at_byte_pos(c:AstSPtr<ast::Crate>,_location:codemap::BytePos)->AstNode_ {
	fail!();
    let tree_loc=find_node_tree_loc_at_byte_pos(c,_location);
    return tree_loc.last().expect("Unable to find node").clone();
}


pub fn find_node_tree_loc_at_byte_pos(c:AstSPtr<ast::Crate>,_location:codemap::BytePos)->NodeTreeLoc {
    // TODO: Now that we have a sepereate 'node-spans table'
    // would it be more efficient to use that?
    // if we encoded hrc information in the node-spans-table,
    // we wouldn't need all this iterator malarchy again.
    let codemap::BytePos(location) = _location;

    let mut vt = Finder::new(location);

    visit::walk_crate(&mut vt, c, ());

    vt.env.result
}

pub fn build_node_info_map(c:AstSPtr<ast::Crate>)-> FNodeInfoMap {
    // todo-lambdas, big fcuntion but remove the extraneous symbols
	let prof=::timer::Profiler::new("build_node_info_map");

    let mut vt = FNodeInfoMapBuilder_new();

    visit::walk_crate(&mut vt, c, 0);
    vt.all_nodes
}

pub trait ToJsonStrFc {fn to_json_str(&self,c:&RustFindCtx)->~str;}

pub fn node_spans_table_to_json_sub(dc:&RustFindCtx,ns:&FNodeInfoMap)->~str {
    // TODO - is there a cleaner functional way,
    // map (|x| fmt...).flatten_to_str() or something like that..

    // emit in a form more useable by external tools.
    // not a serialization of the data used here.
    let mut r=~"";
//  for ns.iter().advance |(k,v)| {
    for (k,v) in ns.iter() {
        //let (_,line,_)=byte_pos_to_file_line_col(c,*v.span.lo);

        let oifps=v.span.lo.to_index_file_pos(dc.tycx_ref());
        let oifpe=v.span.hi.to_index_file_pos(dc.tycx_ref());
        if oifps.is_some() && oifpe.is_some() {
            let ifps=oifps.unwrap();
            let ifpe=oifpe.unwrap();
            // local node:-
            //assert!(ifps.file_index==ifpe.file_index);
            r.push_str(format!("\t\\{node_id:{:u},\tkind:\"{:s}\",\tlspan:\\{ lo:\\{file:{:u},line:{:u} ,ofs:{:u}\\},\thi:\\{file:{:u},line:{:u}, ofs:{:u}\\}\\}\\},\n",*k,v.kind,
                ifps.file_index, ifps.line, ifps.col,ifpe.file_index, ifpe.line, ifpe.col));
        } else {
            // external node:-
            let codemap::BytePos(lo) = v.span.lo;
            let codemap::BytePos(hi) = v.span.hi;
            r.push_str(format!("\t\\{node_id:{:u},\tkind:\"{:s}\"\trspan\\{lo:{:u},hi:{:u}\\}\\}\n",*k,v.kind, lo, hi));
        }
    }
    r
}

impl ToJsonStrFc for FNodeInfoMap {
    fn to_json_str(&self,dc:&RustFindCtx)->~str {
        ~"[\n"+node_spans_table_to_json_sub(dc,self)+"]\n"
    }
}

impl ToJsonStr for HashMap<ast::NodeId,ast::DefId> {
    fn to_json_str(&self)->~str {
        let mut r=~"[\n";
//      for self.iter().advance|(&key,&value)| {
        for (&key,&value) in self.iter() {
            r.push_str(format!("\t\\{node_id:{:?},\tdef_id:\\{crate_:{:?},node:{:?}\\}\\},\n", key, value.krate,value.node));
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
impl KindToStr for ast::Item {
    fn kind_to_str(&self)->&'static str {
        match self.node {
        ast::ItemStatic(..)=>"static",
        ast::ItemFn(..)=>"fn",
        ast::ItemMod(_)=>"mod",
        ast::ItemForeignMod(_)=>"foreign_mod",
        ast::ItemTy(..)=>"ty",
        ast::ItemEnum(..)=>"enum",
        ast::ItemStruct(..)=>"struct",
        ast::ItemTrait(..)=>"trait",
        ast::ItemImpl(..)=>"impl",
        ast::ItemMac(_)=>"mac",
        }
    }
}
impl KindToStr for ast::Expr {
    fn kind_to_str(&self)->&'static str {
        match self.node {
        ast::ExprVstore(_,_)=>"vstore",
        ast::ExprVec(_)=>"vec",
        ast::ExprCall(_,_)=>"call",
        ast::ExprMethodCall(_,_,_)=>"method_call",
        ast::ExprTup(_)=>"tup",
        ast::ExprBinary(binop, _,_)=>match binop {
//          ast_util::binop_to_*(binop) todo - we donnt use this because of ambiguity
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
        ast::ExprUnary(unop, _)=>match unop {
            ast::UnBox=>"box",
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
        ast::ExprBlock(_)=>"blk",
        ast::ExprAssign(_,_)=>"assign",
        ast::ExprAssignOp(binop, _, _)=>match binop {
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
        ast::ExprIndex(_,_)=>"index",
        ast::ExprPath(_)=>"path",
        ast::ExprAddrOf(_, _)=>"addr_of",
        ast::ExprBreak(_)=>"break",
        ast::ExprAgain(_)=>"again",
        ast::ExprRet(_)=>"ret",
//        ast::ExprLogLevel => "log",
        ast::ExprInlineAsm(_)=>"inline_asm",
        ast::ExprMac(_)=>"mac",
        ast::ExprStruct(_,_,_)=>"expr_struct",
        ast::ExprRepeat(_,_)=>"repeat",
        ast::ExprParen(_)=>"paren",
        ast::ExprBox(_, _) => "box",
        }
    }
}

impl AstNode_ {
    // Accessor for the node_id to use for getting definition
    // TODO - clarify by wrapping access to def_map here?
    pub fn rf_ty_node_id(&self)->Option<ast::NodeId> {
        match *self {
            astnode_ty(ty)=>
                match ty.node {
                    ast::TyPath(_,_,NodeId)=>Some(NodeId),
                    _ => self.rf_get_id()
                },
            _ => self.rf_get_id()
        }
    }
}
impl KindToStr for AstNode_ {
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

impl AstNodeAccessors for ast::Item_ {
    fn rf_get_id(&self)->Option<ast::NodeId> {
        match *self {
        ast::ItemStatic(_,_,ref e) => Some(e.id),
        ast::ItemFn(_, _, _, _, ref b)=>Some(b.id),
        ast::ItemMod(_)=>None,
        ast::ItemForeignMod(_)=>None,
        ast::ItemTy(ref ty,_)=>Some(ty.id),
        ast::ItemEnum(_,_)=>None,
        ast::ItemStruct(_,_)=>None,
        ast::ItemTrait(_,_,_)=>None,
        ast::ItemImpl(_,_,ref ty,_)=>Some(ty.id), //TODO, is this wrong, just node_id of a component?
        ast::ItemMac(_)=>None,
        }
    }
    fn rf_get_ident(&self)->Option<ast::Ident> {
        match *self {
        _=>None
        }
    }
}

impl AstNodeAccessors for ast::Item {
    fn rf_get_id(&self)->Option<ast::NodeId> { Some(self.id)}
    fn rf_get_ident(&self)->Option<ast::Ident> {   Some(self.ident) }
}

impl AstNodeAccessors for ast::Expr {
    fn rf_get_id(&self)->Option<ast::NodeId> { Some(self.id)}
    fn rf_get_ident(&self)->Option<ast::Ident> {   None }
}


impl AstNodeAccessors for ast::Pat {
    fn rf_get_id(&self)->Option<ast::NodeId> { Some(self.id)}
    fn rf_get_ident(&self)->Option<ast::Ident> { None
/*      match *self {
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
    fn rf_get_id(&self)->Option<ast::NodeId> { Some(self.id)}
    fn rf_get_ident(&self)->Option<ast::Ident> {   None }
}

impl AstNodeAccessors for ast::Decl_ {
    fn rf_get_id(&self)->Option<ast::NodeId> {
        match *self{
            ast::DeclLocal(ref x)=>Some(x.id),
            ast::DeclItem(ref x)=>Some(x.id)
        }
    }
    fn rf_get_ident(&self)->Option<ast::Ident> {
        match *self {
            ast::DeclLocal(l)=>l.rf_get_ident(),
            ast::DeclItem(l)=>l.rf_get_ident()
        }
    }
}

impl<T:AstNodeAccessors> AstNodeAccessors for codemap::Spanned<T> {
    fn rf_get_id(&self)->Option<ast::NodeId> {
        self.node.rf_get_id()
    }
    fn rf_get_ident(&self)->Option<ast::Ident> { self.node.rf_get_ident() }
}
//impl AstNodeAccessors for ty_method {
//  pub fn get_id(&self)->Option<NodeId> {
//      Some(self.id)
//  }
//}
impl AstNodeAccessors for ast::Block {
    fn rf_get_id(&self)->Option<ast::NodeId> {
        Some(self.id)
    }
    fn rf_get_ident(&self)->Option<ast::Ident> { None }
}
impl AstNodeAccessors for ast::Stmt_ {
    fn rf_get_id(&self)->Option<ast::NodeId> {
        match *self {
            ast::StmtDecl(_,x)=>Some(x),
            ast::StmtExpr(_,x)=>Some(x),
            ast::StmtSemi(_,x)=>Some(x),
            ast::StmtMac(_,_)=>None
        }
    }
    fn rf_get_ident(&self)->Option<ast::Ident> { None }
}

impl AstNodeAccessors for ast::ViewItem {
    fn rf_get_id(&self)->Option<ast::NodeId> { self.node.rf_get_id() }
    fn rf_get_ident(&self)->Option<ast::Ident> { self.node.rf_get_ident() }
}
impl AstNodeAccessors for ast::Ty_ {
    fn rf_get_id(&self)->Option<ast::NodeId> { None }
    fn rf_get_ident(&self)->Option<ast::Ident> { None }
}
impl AstNodeAccessors for ast::Ty {
    fn rf_get_id(&self)->Option<ast::NodeId> { Some(self.id) }
    fn rf_get_ident(&self)->Option<ast::Ident> { self.node.rf_get_ident() }
}

impl AstNodeAccessors for ast::ViewItem_ {
    fn rf_get_id(&self)->Option<ast::NodeId> {
        match *self {
            ast::ViewItemExternCrate(_,_,node_id)=>Some(node_id),
            ast::ViewItemUse(_)=>None
        }
    }
    fn rf_get_ident(&self)->Option<ast::Ident> {
        match *self {
            ast::ViewItemExternCrate(ident,_,_)=>Some(ident),
            ast::ViewItemUse(_)=>None
        }
    }
}
impl AstNodeAccessors for ast::Variant_ {
    fn rf_get_id(&self)->Option<ast::NodeId> { Some(self.id) }
    fn rf_get_ident(&self)->Option<ast::Ident> { Some(self.name) }
}
impl AstNodeAccessors for ast::TypeMethod {
    fn rf_get_id(&self)->Option<ast::NodeId> { Some(self.id) }
    fn rf_get_ident(&self)->Option<ast::Ident> { Some(self.ident) }
}

impl AstNodeAccessors for ast::Method {
    fn rf_get_id(&self)->Option<ast::NodeId> { Some(self.id) }
    fn rf_get_ident(&self)->Option<ast::Ident> { Some(self.ident) }
}
impl AstNodeAccessors for ast::StructDef {
    fn rf_get_id(&self)->Option<ast::NodeId> { None }
    fn rf_get_ident(&self)->Option<ast::Ident> { None }
}
impl AstNodeAccessors for ast::TraitRef {
    fn rf_get_id(&self)->Option<ast::NodeId> { None }
    fn rf_get_ident(&self)->Option<ast::Ident> { None }
}


impl AstNodeAccessors for ast::StructField_ {
    fn rf_get_id(&self)->Option<ast::NodeId> {
        Some(self.id)
    }
    fn rf_get_ident(&self)->Option<ast::Ident> {
        match self.kind{
            ast::NamedField(ident,_)=>Some(ident),
            ast::UnnamedField(_) => None
        }
    }
}

impl AstNodeAccessors for ast::TraitMethod {
    fn rf_get_id(&self)->Option<ast::NodeId> {
        match *self {
            ast::Required(ref m)=>Some(m.id),
            ast::Provided(_)=>None
        }
    }
    fn rf_get_ident(&self)->Option<ast::Ident> {
        match *self {
        ast::Required(ref tym)=> tym.rf_get_ident(),
        ast::Provided(ref m)=>m.rf_get_ident()
        }
    }
}
impl AstNodeAccessors for ast::Mod  {
    fn rf_get_id(&self)->Option<ast::NodeId>{ None }
    fn rf_get_ident(&self)->Option<ast::Ident>{ None }
}

impl AstNodeAccessors for AstNode_ {
    fn rf_get_id(&self)->Option<ast::NodeId> {
        // todo - should be option<node_id> really..
        match *self {
            astnode_mod(_) => None,
            astnode_view_item(ref x) =>x.node.rf_get_id(),
            astnode_item(ref x) =>Some(x.id),
            astnode_local(ref x) =>Some(x.id),
            astnode_block(_)=>None,
            astnode_stmt(_)=>None,
            astnode_arm(_)=>None,
            astnode_pat(ref x)=>Some(x.id),
            astnode_decl(ref x)=>x.rf_get_id(),
            astnode_expr(ref x)=>Some(x.id),
//          astnode_expr_post(ref x)=>Some(x.id),
            astnode_ty(ref x)=>Some(x.id),
            astnode_ty_method(ref x)=>Some(x.id),
            astnode_trait_method(ref x)=>x.rf_get_id(),
            astnode_method(ref m)=>Some(m.id),
            astnode_struct_def(_)=>None,
            astnode_struct_field(ref x)=>Some(x.node.id),
            astnode_trait_ref(ref x)=>Some(x.ref_id),
            astnode_variant(ref x)=>Some(x.id),
            astnode_none|astnode_root=>None,
        }
    }
    fn rf_get_ident(&self)->Option<ast::Ident> {
        // todo - should be option<node_id> really..
        match *self {
            astnode_mod(ref x) => x.rf_get_ident(),
            astnode_view_item(ref x) =>x.rf_get_ident(),
            astnode_item(ref x) =>x.rf_get_ident(),
            astnode_local(ref x) =>x.rf_get_ident(),
            astnode_block(_)=>None,
            astnode_stmt(_)=>None,
            astnode_arm(_)=>None,
            astnode_pat(ref x)=>x.rf_get_ident(),
            astnode_decl(ref x)=>x.rf_get_ident(),
            astnode_expr(ref x)=>x.rf_get_ident(),
//          astnode_expr_post(ref x)=>Some(x.id),
            astnode_ty(ref x)=>x.rf_get_ident(),
            astnode_ty_method(ref x)=>x.rf_get_ident(),
            astnode_trait_method(ref x)=>x.rf_get_ident(),
            astnode_method(ref m)=>m.rf_get_ident(),
            astnode_struct_def(ref x)=>x.rf_get_ident(),
            astnode_struct_field(ref x)=>x.rf_get_ident(),
            astnode_trait_ref(ref x)=>x.rf_get_ident(),
            astnode_variant(ref x)=>x.rf_get_ident(),
            astnode_none|astnode_root=>None,
        }
    }
}




pub fn get_ast_node_of_node_id(info:&FNodeInfoMap,id:ast::NodeId)->Option<AstNode_> {
    match info.find(&id) {
        None=>None,
        Some(node_info)=>Some(node_info.node)
    }
}

pub fn get_node_info_str(dc:&RustFindCtx,node:&NodeTreeLoc)->~str
{
    fn path_to_str(path:&ast::Path)->~str {
        let mut acc=~"";
        let mut first=true;
//      for path.idents.iter().advance |x|{
        for x in path.segments.iter() {
            if !first {acc=acc.append("::");}
            acc=acc.append(token::get_ident(x.identifier).get());
            first=false
        }
        acc
        // typeparams too... path.types?
    }
    fn pat_to_str(dc:&RustFindCtx,p:&ast::Pat)->~str{
        // todo -factor out and recurse
        match p.node {
            ast::PatIdent(_, ref path, _)=>~"pat_ident:"+path_to_str(path),
            ast::PatEnum(ref path, _)=>~"pat_enum:"+path_to_str(path),//    `todo-fields..
            ast::PatStruct(ref path,ref sfields,_) => {
                ~"pat_struct:" + path_to_str(path) + "{" + 
                    sfields.iter().map(|x| pat_to_str(dc,x.pat) + ",").collect::<Vec<~str>>().to_str() + "}"
            },
            ast::PatTup(ref elems) => {
                ~"pat_tupl:" + elems.iter().map(|&x| pat_to_str(dc, x)).collect::<Vec<~str>>().to_str()
            },
            //ast::pat_box(ref box)=>~"box",
            ast::PatUniq(..)=>~"uniq",
            ast::PatRegion(..)=>~"rgn",
            ast::PatLit(..)=>~"literal",
            ast::PatRange(..)=>~"range",

            _=>~"?"
        }
    };
    fn ty_to_str(dc:&RustFindCtx,t:&ast::Ty)->~str{
        match t.node{
            ast::TyNil=> ~"nil",
            ast::TyBot=>~"bottomtype",
            ast::TyBox(..)=>~"box",
            ast::TyVec(..)=>~"vec",
            ast::TyFixedLengthVec(..)=>~"[T,..N]",
            ast::TyPtr(..)=>~"*",
            ast::TyRptr(..)=>~"&",
            ast::TyTup(ref types) => {
                ~"(" + types.iter().map(|x| ty_to_str(dc, *x)).collect::<Vec<~str>>().to_str() + ")" //todo: factor this out, map..
            },
            ast::TyPath(ref path, _, node_id)=>~"path:id="+node_id.to_str()+" "+path_to_str(path)
            ,

            ast::TyInfer=>~"infered",
            _ =>~"?"
        }
    }
    fn expr_to_str(dc:&RustFindCtx, x:&ast::Expr_)->~str {
        match *x {
            ast::ExprStruct(ref p,_,_)=>~"(expr_struct "+ path_to_str(p) +")",
            ast::ExprCall(ref e,ref args) => {
                ~"(expr_call(" + expr_to_str(dc,&e.node) +
                    args.iter().map(|x| expr_to_str(dc, &x.node)).collect::<Vec<~str>>().to_str() + ")"
            },
            ast::ExprField(ref e, ref i, ref tys) => {
                ~"(expr_field(" + expr_to_str(dc, &e.node) + ")" +
                    token::get_ident(*i).get() + 
                    tys.iter().map(|x| ty_to_str(dc, *x)).collect::<Vec<~str>>().to_str() + ")"
            },
            _=>~"expr"
        }
    }

    match node.last().expect("No last node available") {
//          TODO -factor out repeatedly used functions here..
//          fn astnode_pat_to_str(&astnode_pat(x))->~str
//          fn path_to_str(&astnode_pat(x))->~str
//          fn expr_to_str(&astnode_pat(x))->~str

        &astnode_view_item(_)=>~"view_item: ?",
        &astnode_item(x)=>~"item: "+
            "id="+x.id.to_str()+" "+
            token::get_ident(x.ident).get()+
            match x.node {
                ast::ItemFn(_,_,_,_,_) =>~" fn_decl",
                ast::ItemStruct(_, _) =>~" struct_def",
                _=>~"item_unknown"
            },

        &astnode_local(_)=>~"local: ?",
        &astnode_block(_)=>~"block: ?",
        &astnode_stmt(_)=>~"stmt: ?",
        &astnode_arm(_)=>~"arm: ?",
        &astnode_struct_field(sf)=>
            "id="+sf.node.id.to_str()+" "+
            match sf.node.kind {
                ast::NamedField(nf, _)=>"struct named_field: "+token::get_ident(nf).get()+" ",
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
        _=> ~"unknown"
    }
}

pub fn safe_node_id_to_type(cx: &ty::ctxt, id: ast::NodeId) -> Option<ty::t> {
    //io::println!(fmt!("%?/%?", id, cx.node_types.len()));
    match cx.node_types.borrow().find(&(id as uint)) {
       Some(&t) => Some(t),
       None => None
    }
}

pub fn get_def_id(curr_crate:ast::CrateNum,src_def:ast::Def)->Option<ast::DefId> {
    let mk=|x|{Some(ast::DefId{krate:curr_crate, node:x})}; // todo,mmaybe this is best 'None'..
    // todo-'definition' can be at multiple locations. we should return [def_id] really..
    match src_def {
        ast::DefFn(d,_)=>Some(d),
        ast::DefStaticMethod(d,_,_)=>Some(d),
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

pub fn def_node_id_from_node_id(dc:&RustFindCtx, id:ast::NodeId)->ast::NodeId {
    let crate_num=0;    // TODO - whats crate Id really???
    match dc.tycx_ref().def_map.borrow().find(&id) { // finds a def..
        Some(a)=>{
            match get_def_id(crate_num,*a) {
                Some(b)=>b.node,
                None=>id
            }
        },
        None=>(id)  // no definition? say its its own definition
    }
}


pub fn def_of_symbol_to_str(_:&RustFindCtx, _:&FNodeInfoMap, _:&HashMap<ast::NodeId, ast::DefId>, _:&str)->~str {
    ~"TODO"
}


// TODO- this should return a slice?
pub fn get_node_source(tc:&ty::ctxt, nim:&FNodeInfoMap, did:ast::DefId)->~str {
    if did.krate==0{
        match nim.find(&did.node) {
            None=>~"",
            Some(info)=>{
                get_span_str(tc,&info.span)
            }
        }
    } else {
        "{out of crate def:"+did.to_str()+"}"
    }
}

pub fn dump_node_source_for_single_file_only(text:&[u8], ns:&FNodeInfoMap, id:ast::NodeId) {
    match ns.find(&id) {
        None=>logi!("()"),
        Some(info)=>{
            dump_span(text, &info.span);
        }
    }
}

// Step thru everything here...
fn verify_parent_links(infomap:&FNodeInfoMap) {
	for (id,info) in infomap.iter() {
//		let parent_info = infomap.find(&info.parent)
	}
}
