use rf_common::*;
pub use syntax::{ast,abi};
pub use syntax::visit;
pub use syntax::parse::token;
pub use syntax::visit::{Visitor};
pub use syntax::codemap;
pub use syntax::codemap::BytePos;
use rf_ast_ut::*;
pub use rustfindctx::*;
//use rustc::middle::mem_categorization::ast_node;
use rustc::middle::ty;
use rustfindctx::{RustFindCtx,};
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

// Single enum for rust nodes.
// if we could extract a lone discriminant from an enum, or wrap their creation in a macro to enumerate this aswell, that would be nice. something with an x-macro in the compiler.. but we're unlikely to get changes like that in.
#[deriving(Clone,Eq,TotalEq,Hash)]
pub enum NodeKind {	// typedef this, so we can swap in a rust identifier. its *not* the ast object itself.
	NK_Trait, NK_Struct, NK_Enum,NK_Fn,NK_Mod,NK_ForeignMod, NK_Type, NK_Static,  NK_FnBlock, /* todo - check if there really is a difference between fnblock and block?! or is one a lambda?*/
	NK_Add,NK_Sub,NK_Mul,NK_Div,NK_Rem, NK_Assign,NK_Eq,NK_Le,NK_Lt,NK_Gt,NK_Ge,NK_Ne,NK_BinOp,NK_AssignOp,NK_BitAnd,NK_BitXor,NK_BitOr,NK_Shl,NK_Shr,NK_Not,NK_And,NK_Or,NK_Neg,NK_Box,NK_Uniq,NK_Deref,NK_AddrOf,NK_De,NK_TypeParam,NK_Ty,NK_StructField,NK_Path,NK_Call,NK_Variant,NK_MethodCall,NK_Lit,NK_Stmt,NK_Local,NK_Pat,NK_Block,NK_Method,NK_TyMethod,NK_TraitRef,NK_TraitMethod,NK_Tup,NK_Arm,NK_Index,NK_VStore,NK_Impl,NK_While,NK_Break,NK_ForLoop,NK_Match,NK_Loop,NK_Do,NK_Cast,NK_If,NK_Return,NK_Unsafe,NK_Extern,NK_Crate,NK_As,NK_In,NK_For, NK_Vec,NK_Proc, NK_AssignAdd,NK_AssignSub,NK_AssignMul,NK_AssignDiv,NK_AssignRem,NK_AssignAnd,NK_AssignOr,NK_AssignBitXor,NK_AssignBitAnd,NK_AssignBitOr,NK_AssignShl,NK_AssignShr,NK_Field,NK_InlineAsm,NK_Repeat,NK_Keyword,NK_Again,/*??*/NK_Paren,NK_Mac,NK_ViewItem,/*todo- check if we ever really should have this or rather drill down and always get the type of viewitem. */NK_ErrorShouldNeverHaveThis,NK_Decl,NK_Root,NK_Unknown,NK_None,
}

/// Unified AST node, wraps any ast node accessible from a NodeId
/// TODO - add a lifetime, we suspect this will become a borrowedptr
/// THIS IS REALLY AN AST NODE POINTER, DONT PANIC
/// - its currently a @ptr, but will possily changed to a borrowed reference?
#[deriving(Clone,TotalEq,Hash,Eq)]
pub enum AstNode_<'a> { 
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
pub struct FNodeInfo<'a> {
//	pub id:	ast::NodeId,					// id of this node.
//    pub ident:Option<ast::Ident>,
	
    pub kind:NodeKind,			// TODO: This will just be an enum!
    pub span:codemap::Span,
    pub node:AstNode_<'a>,			// todo- get rid of this and just make this a cache of spans for linking.
    pub parent_id:ast::NodeId,
	pub children:Vec<ast::NodeId>,
}






//.. TODO - does it make sense to cache an ident here? not all nodes have one

type SPtr<T> =@T;	// temporary to be replaced later.

/// implementations for FNodeInfo, convinience accessors.
/// refactor rustfinds' accessors to our AST wrappers
/// we suspect either these are already available, or they will be added in time.
/// use rf_ prefix for methods for ease of search replace when we know what the
/// best way is !
impl<'a> FNodeInfo<'a> {
	pub fn rf_get_ident(&self)->Option<ast::Ident> { self.node.rf_get_ident()}
	pub fn rf_get_id(&self)->ast::NodeId {
		// hmm, this starts to look wrong.
		let id=self.node.rf_get_id();
		if !id.is_some() {fail!("tried to get ident on node without ident, code design is wrong");}
		return id.unwrap();
	}
	pub fn rf_kind<'a> (&'a self)->NodeKind { self.kind }
	pub fn rf_span(&self)->codemap::Span { self.span }
	pub fn rf_node(&self)->AstNode_<'a> { self.node }
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
	pub fn rf_as_ast_node<'a>(&'a self)->&'a AstNode_<'a> {
		&self.node
	}

	/// Single wrapper to acess as function declaration.
	/// this is a lot of boilerplate, probably more code overall,
	/// but seems to make some tasks easier elsewhere.

	pub fn rf_as_fn_decl<'a>(&'a self)->
			Option<(AstSPtr<ast::Item>,
				(ast::P<ast::FnDecl>, ast::FnStyle, abi::Abi, ast::Generics, ast::P<ast::Block>)
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

impl NodeKind{
	// todo - make a macro to emit enum NK_... and this 'as_str'
	pub fn as_str(self)->&'static str {
		match self {
			NK_Path=>"path",
			NK_Trait=>"trait",
			NK_Struct=>"struct",
			NK_StructField=>"struct_field",
			NK_Field=>"field",
			NK_Enum=>"enum",
			NK_Variant=>"variant",
			NK_Fn=>"fn",
			NK_Method=>"method",
			NK_TyMethod=>"ty_method",
			NK_TraitMethod=>"trait_method",
			NK_TraitRef=>"trait_ref",
			NK_Mod=>"mod",
			NK_ForeignMod=>"foreign_mod",
			NK_Static=>"static",
			NK_Type=>"type",
			NK_Decl=>"decl",
			NK_None=>"",
			NK_Impl=>"impl",
			NK_Tup=>"tup",
			NK_TypeParam=>"type_param",
			NK_Lit=>"lit",
			_=>"NK_AS_STR_TODO",
		}
	}
	// todo - macro / enum generates this for all..
	pub fn from_str(s:&str)->NodeKind {
		match s {
			"path"=>NK_Path,
			"trait"=>NK_Trait,
			"struct"=>NK_Struct,
			"struct_field"=>NK_StructField,
			"field"=>NK_Field,
			"enum"=>NK_Enum,
			"variant"=>NK_Variant,
			"fn"=>NK_Fn,
			"method"=>NK_Method,
			"ty_method"=>NK_TyMethod,
			"trait_method"=>NK_TraitMethod,
			"trait_ref"=>NK_TraitRef,
			"mod"=>NK_Mod,
			"foreign_mod"=>NK_ForeignMod,
			"type_param"=>NK_TypeParam,
			"type"=>NK_Type,
			"tup"=>NK_Tup,
			"static"=>NK_Static,
			"lit"=>NK_Lit,

			_=>NK_Unknown,
		}
	}
}

/// Wrapper for holder of nodes, TODO - refactoring away from our local copy of the ast, wrap a cursor around theirs.
pub struct FNodeInfoMap<'ast_lifetime> {
	pub fni_hashmap:HashMap<ast::NodeId,FNodeInfo<'ast_lifetime>>
}


impl<'ast> FNodeInfoMap<'ast> {
	pub fn new()->FNodeInfoMap<'ast>{
		FNodeInfoMap{ fni_hashmap: HashMap::new() }
	}
	pub fn find<'a>(&'a self,id:&ast::NodeId)->Option<&'a FNodeInfo<'ast>> {
		self.fni_hashmap.find(id)
	}
	pub fn find_mut<'a> (&'a mut self,id:&ast::NodeId)->Option<&'a mut FNodeInfo<'ast>> {
		self.fni_hashmap.find_mut(id)
	}
	pub fn insert(&mut self, id:ast::NodeId, v:FNodeInfo<'ast>) {
		self.fni_hashmap.insert(id,v);
	}
	pub fn find_or_insert<'a>(&'a mut self, id:ast::NodeId, v:FNodeInfo)->&'a mut FNodeInfo<'ast> {
		self.fni_hashmap.find_or_insert(id,v)
	}
	pub fn len(&self)->uint { self.fni_hashmap.len() }
	pub fn iter<'a>(&'a self)->::collections::hashmap::Entries<'a,ast::NodeId, FNodeInfo<'ast> >  {
		self.fni_hashmap.iter()
	}

}

// TODO - find a way to eliminate this hell!
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


pub type NodeTreeLoc<'a> = Vec<AstNode_<'a>>;
pub fn dump_node_tree_loc<'a>(ndt:&NodeTreeLoc<'a>) {
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
    fn get_kind(&self)->NodeKind;
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

pub fn node_spans_table_to_json_sub(dc:&RustFindCtx,ns:&FNodeInfoMap)->StrBuf {
    // TODO - is there a cleaner functional way,
    // map (|x| fmt...).flatten_to_str() or something like that..

    // emit in a form more useable by external tools.
    // not a serialization of the data used here.
    let mut r=StrBuf::with_capacity(1024);
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
            r.push_str(format!("\t\\{node_id:{:u},\tkind:\"{:s}\",\tlspan:\\{ lo:\\{file:{:u},line:{:u} ,ofs:{:u}\\},\thi:\\{file:{:u},line:{:u}, ofs:{:u}\\}\\}\\},\n",*k,v.kind.as_str(),
                ifps.file_index, ifps.line, ifps.col,ifpe.file_index, ifpe.line, ifpe.col));
        } else {
            // external node:-
            let codemap::BytePos(lo) = v.span.lo;
            let codemap::BytePos(hi) = v.span.hi;
            r.push_str(format!("\t\\{node_id:{:u},\tkind:\"{:s}\"\trspan\\{lo:{:u},hi:{:u}\\}\\}\n",*k,v.kind.as_str(), lo, hi));
        }
    }
    r
}

impl<'astl> ToJsonStrFc for FNodeInfoMap<'astl> {
    fn to_json_str(&self,dc:&RustFindCtx)->~str {
        let mut ret=StrBuf::from_str("[\n");
		ret.push_str(node_spans_table_to_json_sub(dc,self).as_slice());
		ret.push_str("]\n");
		ret.as_slice().to_owned()
    }
}

impl ToJsonStr for HashMap<ast::NodeId,ast::DefId> {
    fn to_json_str(&self)->~str {
        let mut r=StrBuf::from_str("[\n");
//      for self.iter().advance|(&key,&value)| {
        for (&key,&value) in self.iter() {
            r.push_str(format!("\t\\{node_id:{:?},\tdef_id:\\{crate_:{:?},node:{:?}\\}\\},\n", key, value.krate,value.node));
        }
        r.push_str("]\n");
        r.as_slice().to_owned()
    }
}

// TODO - is there an official wrapper like this for all nodes in libsyntax::ast?
// is there a way of acheiving this without one?
// TODO: TRY USING ast_map::ast_node INSTEAD
//#[deriving(Clone)]

// TODO - include heirachical location in wrappertype? node.parent.. node.node
// Currently we use a [AstNode] to represent a location in the node-tree with all parent nodes.
// but this looks confusing, eg are we passing a node list?
// struct HrcNode{ parent:Option<HrcNode>, node:AstNode}
// or even AstNode{ .../* each type has (sParent,content)*/}

//

impl KindToStr for ast::Decl {
    fn kind_to_str(&self)->&'static str {
		self.get_kind().as_str()
    }
	fn get_kind(&self)->NodeKind {
        match self.node {
        ast::DeclLocal(_)=>NK_Local,
        ast::DeclItem(x)=>x.get_kind(),
        }
	}
}
impl KindToStr for ast::Item {
    fn kind_to_str(&self)->&'static str {
		self.get_kind().as_str()
    }
	fn get_kind(&self)->NodeKind {
        match self.node {
        ast::ItemStatic(..)=>NK_Static,
        ast::ItemFn(..)=>NK_Fn,
        ast::ItemMod(_)=>NK_Mod,
        ast::ItemForeignMod(_)=>NK_ForeignMod,
        ast::ItemTy(..)=>NK_Ty,
        ast::ItemEnum(..)=>NK_Enum,
        ast::ItemStruct(..)=>NK_Struct,
        ast::ItemTrait(..)=>NK_Trait,
        ast::ItemImpl(..)=>NK_Impl,
        ast::ItemMac(_)=>NK_Mac,
        }
	}
}
impl KindToStr for ast::Expr {
    fn kind_to_str(&self)->&'static str {
		self.get_kind().as_str()
	}

    fn get_kind(&self)->NodeKind {
        match self.node {
        ast::ExprVstore(_,_)=>NK_VStore,
        ast::ExprVec(_)=>NK_Vec,
        ast::ExprCall(_,_)=>NK_Call,
        ast::ExprMethodCall(_,_,_)=>NK_MethodCall,
        ast::ExprTup(_)=>NK_Tup,
        ast::ExprBinary(binop, _,_)=>match binop {

            ast::BiAdd=>NK_Add,
            ast::BiSub=>NK_Sub,
            ast::BiMul=>NK_Mul,
            ast::BiDiv=>NK_Div,
            ast::BiRem=>NK_Rem,
            ast::BiAnd=>NK_And,
            ast::BiOr=>NK_Or,
            ast::BiBitXor=>NK_BitXor,
            ast::BiBitAnd=>NK_BitAnd,
            ast::BiBitOr=>NK_BitOr,
            ast::BiShl=>NK_Shl,
            ast::BiShr=>NK_Shr,
            ast::BiEq=>NK_Eq,
            ast::BiLt=>NK_Lt,
            ast::BiLe=>NK_Le,
            ast::BiNe=>NK_Ne,
            ast::BiGe=>NK_Ge,
            ast::BiGt=>NK_Gt,

        },
        ast::ExprUnary(unop, _)=>match unop {
            ast::UnBox=>NK_Box,
            ast::UnUniq=>NK_Uniq,
            ast::UnDeref=>NK_Deref,
            ast::UnNot=>NK_Not,
            ast::UnNeg=>NK_Neg
        },
        ast::ExprLit(_)=>NK_Lit,
        ast::ExprCast(_, _)=>NK_Cast,
        ast::ExprIf(_,_,_)=>NK_If,

        ast::ExprWhile(_, _)=>NK_While,
        ast::ExprForLoop(_, _,_, _)=>NK_ForLoop,
        ast::ExprLoop(_, _)=>NK_Loop,
        ast::ExprMatch(_, _)=>NK_Match,
        ast::ExprFnBlock(_, _)=>NK_FnBlock,
        ast::ExprProc(..) => NK_Proc,
        ast::ExprBlock(_)=>NK_Block,
        ast::ExprAssign(_,_)=>NK_Assign,
        ast::ExprAssignOp(binop, _, _)=>match binop {
            ast::BiAdd=>NK_AssignAdd,
            ast::BiSub=>NK_AssignSub,
            ast::BiMul=>NK_AssignMul,
            ast::BiDiv=>NK_AssignDiv,
            ast::BiRem=>NK_AssignRem,
            ast::BiAnd=>NK_AssignAnd,
            ast::BiOr=>NK_AssignOr,
            ast::BiBitXor=>NK_AssignBitXor,
            ast::BiBitAnd=>NK_AssignBitAnd,
            ast::BiBitOr=>NK_AssignBitOr,
            ast::BiShl=>NK_AssignShl,
            ast::BiShr=>NK_AssignShr,
			_=>NK_ErrorShouldNeverHaveThis,
        },
        ast::ExprField(_, _, _)=>NK_Field,
        ast::ExprIndex(_,_)=>NK_Index,
        ast::ExprPath(_)=>NK_Path,
        ast::ExprAddrOf(_, _)=>NK_AddrOf,
        ast::ExprBreak(_)=>NK_Break,
        ast::ExprAgain(_)=>NK_Again,
        ast::ExprRet(_)=>NK_Return,
        ast::ExprInlineAsm(_)=>NK_InlineAsm,
        ast::ExprMac(_)=>NK_Mac,
        ast::ExprStruct(_,_,_)=>NK_Struct,
        ast::ExprRepeat(_,_)=>NK_Repeat,
        ast::ExprParen(_)=>NK_Paren,
        ast::ExprBox(_, _) => NK_Box,
        }
    }


}

impl<'a> AstNode_<'a> {
    // Accessor for the node_id to use for getting definition
    // TODO - clarify by wrapping access to def_map here?
    pub fn rf_ty_node_id(&self)->Option<ast::NodeId> {
        match *self {
            astnode_ty(ty)=>
                match ty.node {
                    ast::TyPath(_,_,node_id)=>Some(node_id),
                    _ => self.rf_get_id()
                },
            _ => self.rf_get_id()
        }
    }
}
impl<'astl> KindToStr for AstNode_<'astl> {
    fn kind_to_str(&self)->&'static str {
		self.get_kind().as_str()
    }
    fn get_kind(&self)->NodeKind {
        //TODO subsets of view_item?
        match *self {
            astnode_mod(_)=>NK_Mod,
            astnode_view_item(_)=>NK_ViewItem,
            astnode_item(x)=>x.get_kind(),
            astnode_local(_)=>NK_Local,
            astnode_block(_)=>NK_Block,
            astnode_stmt(_)=>NK_Stmt,
            astnode_arm(_)=>NK_Arm,
            astnode_pat(_)=>NK_Pat,
            astnode_decl(x)=>x.get_kind(),
            astnode_expr(x)=>x.get_kind(),
            astnode_ty(_)=>NK_Type,
            astnode_ty_method(_)=>NK_TyMethod,
            astnode_method(_)=>NK_Method,
            astnode_trait_method(_)=>NK_TraitMethod,
            astnode_struct_def(_)=>NK_Struct,
            astnode_struct_field(_)=>NK_StructField,
            astnode_trait_ref(_)=>NK_TraitRef,
            astnode_variant(_)=>NK_Variant,
            astnode_root=>NK_Root,
            astnode_none=>NK_None
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
        ast::ItemTrait(_,_,_,_)=>None,
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

impl<'astl> AstNodeAccessors for AstNode_<'astl> {
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




pub fn get_ast_node_of_node_id<'astl>(info:&FNodeInfoMap<'astl>,id:ast::NodeId)->Option<AstNode_<'astl>> {
    match info.find(&id) {
        None=>None,
        Some(node_info)=>Some(node_info.node)
    }
}

pub fn get_node_info_str<'astl>(dc:&RustFindCtx,node:&NodeTreeLoc<'astl>)->StrBuf
{
    fn path_to_str(path:&ast::Path)->StrBuf {
        let mut acc=StrBuf::new();
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
    fn pat_to_str(dc:&RustFindCtx,p:&ast::Pat)->StrBuf{
        // todo -factor out and recurse
        match p.node {
            ast::PatIdent(_, ref path, _)=>StrBuf::from_str("pat_ident:").append(path_to_str(path).as_slice()),
            ast::PatEnum(ref path, _)=>StrBuf::from_str("pat_enum:").append(path_to_str(path).as_slice()),//    `todo-fields..
            ast::PatStruct(ref path,ref sfields,_) => {
                StrBuf::from_str("pat_struct:").append(path_to_str(path).as_slice()).append("{").append(sfields.iter().map(|x| pat_to_str(dc,x.pat).append(",")).collect::<Vec<StrBuf>>().to_str().as_slice()).append("}")
            },
            ast::PatTup(ref elems) => {
                StrBuf::from_str("pat_tupl:").append( elems.iter().map(|&x| pat_to_str(dc, x)).collect::<Vec<StrBuf>>().to_str().as_slice())
            },
            //ast::pat_box(ref box)=>~"box",
            ast::PatUniq(..)=>StrBuf::from_str("uniq"),
            ast::PatRegion(..)=>StrBuf::from_str("rgn"),
            ast::PatLit(..)=>StrBuf::from_str("literal"),
            ast::PatRange(..)=>StrBuf::from_str("range"),

            _=>StrBuf::from_str("?")
        }
    };
    fn ty_to_str(dc:&RustFindCtx,t:&ast::Ty)->StrBuf{
        match t.node{
            ast::TyNil=> StrBuf::from_str("nil"),
            ast::TyBot=>StrBuf::from_str("bottomtype"),
            ast::TyBox(..)=>StrBuf::from_str("box"),
            ast::TyVec(..)=>StrBuf::from_str("vec"),
            ast::TyFixedLengthVec(..)=>StrBuf::from_str("[T,..N]"),
            ast::TyPtr(..)=>StrBuf::from_str("*"),
            ast::TyRptr(..)=>StrBuf::from_str("&"),
            ast::TyTup(ref types) => {
				StrBuf::from_str("(").append(
                types.iter().map(|x| ty_to_str(dc, *x)).collect::<Vec<StrBuf>>().to_str().as_slice()).append(")") //todo: factor this out, map..
            },
            ast::TyPath(ref path, _, node_id)=>StrBuf::from_str("path:id=").append(node_id.to_str().as_slice()).append(" ").append(path_to_str(path).as_slice())
            ,

            ast::TyInfer=>StrBuf::from_str("infered"),
            _ =>StrBuf::from_str("?")
        }
    }
    fn expr_to_str(dc:&RustFindCtx, x:&ast::Expr_)->StrBuf {
        match *x {
            ast::ExprStruct(ref p,_,_)=>StrBuf::from_str("(expr_struct ").append(path_to_str(p).as_slice()).append(")"),
            ast::ExprCall(ref e,ref args) => {
                StrBuf::from_str("(expr_call(").append(expr_to_str(dc,&e.node).as_slice()).append(args.iter().map(|x| expr_to_str(dc, &x.node)).collect::<Vec<StrBuf>>().to_str().as_slice()).append(")")
            },
            ast::ExprField(ref e, ref i, ref tys) => {
                StrBuf::from_str("(expr_field(").append(expr_to_str(dc, &e.node).as_slice()).append(")").append(token::get_ident(*i).get().as_slice()).append(tys.iter().map(|x| ty_to_str(dc, *x)).collect::<Vec<StrBuf>>().to_str().as_slice()).append(")")
            },
            _=>StrBuf::from_str("expr")
        }
    }

    match node.last().expect("No last node available") {
//          TODO -factor out repeatedly used functions here..
//          fn astnode_pat_to_str(&astnode_pat(x))->~str
//          fn path_to_str(&astnode_pat(x))->~str
//          fn expr_to_str(&astnode_pat(x))->~str

        &astnode_view_item(_)=>StrBuf::from_str("view_item: ?"),
        &astnode_item(x)=>StrBuf::from_str("item: ").append("id=").append(x.id.to_str()).append(" ").append(token::get_ident(x.ident).get()).append(
            match x.node {
                ast::ItemFn(_,_,_,_,_) =>" fn_decl",
                ast::ItemStruct(_, _) =>" struct_def",
                _=>"item_unknown"
            }),

        &astnode_local(_)=>StrBuf::from_str("local: ?"),
        &astnode_block(_)=>StrBuf::from_str("block: ?"),
        &astnode_stmt(_)=>StrBuf::from_str("stmt: ?"),
        &astnode_arm(_)=>StrBuf::from_str("arm: ?"),
        &astnode_struct_field(sf)=>
            StrBuf::from_str("id=").append(sf.node.id.to_str().as_slice()).append(" ").append(
            match sf.node.kind {
                ast::NamedField(nf, _)=>StrBuf::from_str("struct named_field: ").append(token::get_ident(nf).get()).append(" "),
                _=>StrBuf::from_str("struct anon_field")
            }.as_slice()).append(":").append(ty_to_str(dc, sf.node.ty).as_slice())/*sf.node.ty ..parse it.. */,
        &astnode_pat(p)=>StrBuf::from_str("pattern: ").append("id=").append(
            p.id.to_str()).append(" ").append(
            pat_to_str(dc,p).as_slice())
        ,
        &astnode_decl(_)=>StrBuf::from_str("decl: ?"),
        &astnode_ty(x)=>StrBuf::from_str("type: ").append(ty_to_str(dc,x).as_slice()),
        &astnode_struct_def(_)=>StrBuf::from_str("struct def"),
        &astnode_expr(x)=>expr_to_str(dc,&x.node),
        _=> StrBuf::from_str("unknown")
    }
}

pub fn safe_node_id_to_type(cx: &ty::ctxt, id: ast::NodeId) -> Option<ty::t> {
    //io::println!(fmt!("%?/%?", id, cx.node_types.len()));
    match cx.node_types.borrow().find(&(id as uint)) {
       Some(&t) => Some(t),
       None => None
    }
}



pub fn def_of_symbol_to_str<'astl>(_:&RustFindCtx, _:&FNodeInfoMap<'astl>, _:&HashMap<ast::NodeId, ast::DefId>, _:&str)->StrBuf {
    StrBuf::from_str("TODO")
}


// TODO- this should return a slice?
pub fn get_node_source<'astl>(tc:&ty::ctxt, nim:&FNodeInfoMap<'astl>, did:ast::DefId)->StrBuf {
    if did.krate==0{
        match nim.find(&did.node) {
            None=>StrBuf::from_str(""),
            Some(info)=>{
                get_span_str(tc,&info.span)
            }
        }
    } else {
        (StrBuf::from_str("{out of crate def:").append(did.to_str().as_slice()).append("}"))
    }
}

pub fn dump_node_source_for_single_file_only<'astl>(text:&[u8], ns:&FNodeInfoMap<'astl>, id:ast::NodeId) {
    match ns.find(&id) {
        None=>logi!("()"),
        Some(info)=>{
            dump_span(text, &info.span);
        }
    }
}

// Step thru everything here...
fn verify_parent_links<'astl>(infomap:&FNodeInfoMap<'astl>) {
	for (id,info) in infomap.iter() {
//		let parent_info = infomap.find(&info.parent)
	}
}
