use syntax::codemap;
use syntax::ast;
use syntax::ast::DefId;
use syntax::codemap::Pos;
use rustc::middle::ty;
use std::io::fs;
use std::path::posix;
use std::hash::Hash;
use collections::{HashMap,HashSet};
use std::slice;
use std::cmp;
use std::io;
use std::io::fs;
use codemaput::{ZIndexFilePos,ToZIndexFilePos};
use find_ast_node::{FNodeInfoMap,FNodeInfo,AstNode_};
use rfindctx::{RustFindCtx};
use crosscratemap::{CrossCrateMap,CrossCrateMapItem};
pub use super::NodeMaps;
use rsfind::MyOption;
use timer::Profiler;
use find_ast_node::*;
use rfindctx::*;

// build a call graph.
// we also want to build a graph of users of types - including types.

/*

todo: Use the mangled symbols as symbol idents, and get proper paths showable,
write proper module heirachy
write links between modules

find a graph layout engine that can use the submodules intelligently.

*/

pub fn dump_functions(nmaps:&NodeMaps) {
	for (node_id, info) in nmaps.node_info_map.iter() {
		info.rf_as_fn_decl().map(|(ref item, fn_decl)|{
			println!("fn {}()", str_of_ident(item.ident));
		});
	}
//	fail!();
}

/// Options for callgraph generation
pub struct CG_Options {	
	// TODO - when we have a solid IDE, we can stop Clike namespacing
	pub local_only:bool,
}
impl CG_Options {
	pub fn new()->CG_Options {
		CG_Options{local_only:true}
	}	
}
/// Generate callgraph file.
/// Todo - seperate into callgraph iterator and file writer passing a closure..
#[deriving(Clone,Eq,TotalEq,Hash)]
type RefCCMItem<'a> =(DefId,&'a CrossCrateMapItem,CG_Kind);
type SetOfItems<'a> =HashSet<RefCCMItem<'a>>;


// TODO: Use mangled symbols for the nodes.
pub fn write_call_graph<'a>(nmaps:&'a NodeMaps, outdirname:&str, filename:&str,opts:&CG_Options) {

	gather_use_graph( nmaps, &|x,y|{});

//	println!("Writing callgraph {} {}..",outdirname, filename);
	match fs::File::create(&posix::Path::new(outdirname+filename.to_owned()+~".dot")) {
		Ok(mut dotf)=>{ write_call_graph_sub(nmaps,outdirname, filename,opts, &mut dotf);},
		_ => println!("can't write callgraph {}", filename),
	}
}

fn write_call_graph_sub<'a>(nmaps:&'a NodeMaps, outdirname:&str, filename:&str,opts:&CG_Options, dotf:&mut fs::File) {
	println!("writing callgraph file ..\n");
	dotf.write_line("digraph "+ filename +" {");
	let mut items_per_module:HashMap<&str,HashSet<RefCCMItem>> =HashMap::new();
	let mut all_calls:HashSet<(RefCCMItem<'a>,RefCCMItem<'a>)> =HashSet::new();
	dotf.write_line("\tnode [style=filled, color=\"#f0f0f0\", fontsize=12 ];");
//	dotf.write_line("\tedge [color=\"#000000f0\", fontsize=12 ]");
	dotf.write_line("\tedge [color=\"#00000020\"];");

	// Gather items with call-graph links,
	gather_use_graph(nmaps, // We miss do notation :( but maybe we could encapsulate the traversal in an iterator?
		&|caller,callee| {
			for x in [caller,callee].iter() {
				items_per_module.insert_or_update_with(x.val1().file_name.as_slice(), HashSet::new(), |k,v|{v.insert(*x);});
			}
			if !(opts.local_only && callee.val0().krate!=0) {
				all_calls.insert( (caller, callee) );
			}
		}
	);

	// Gather definitions.
	// can't we write submodules directly?
	for (&node_id,info) in nmaps.node_info_map.iter() {
		let ccmitem = nmaps.rf_find_local_node(node_id);
		let kind=match info.rf_node() {
			
			astnode_item(item)=>match item.node{
				ast::ItemEnum(_,_)=>CG_Enum,
				ast::ItemStruct(_,_)=>CG_Struct,
				ast::ItemTrait(_,_,_)=>CG_Trait,
				_=>CG_None,
			},
			_=>CG_None,
		};

		match (ccmitem,kind) {
			(_,CG_None)|(None,_)=>{},
			(Some(ccmitem),kind)=>{items_per_module.insert_or_update_with(
				ccmitem.file_name.as_slice(),
				HashSet::new(),
				|k,v|{v.insert((ast::DefId{krate:0,node:node_id},ccmitem,kind));}
				);
			},
		}
	}

	// Write out a cluster for all the functions in a particular sourcefile.
	// TODO - should be able to recursively cluster directories for this..
	// TODO: Generalize to modules.
	for (&modname,items) in items_per_module.iter() {
//				let modstr=::std::str::from_chars(modname.chars().map(|x|if x=='/'{'_'}else{x}));
		let modstr:~str=modname.chars().map(|x|match x{'/'|'.'=>'_',_=>x}).collect();
		if items.len()==0{ continue;}
		if modstr.chars().nth(0)==Some('<') {continue;} // things like <std macros>
		// todo: use mangled module name
		module_subgraph_begin(dotf,2, modstr, modstr.slice_to(modname.rfind('.').unwrap_or(modname.len())));
		for &(defid,xcmi,kind) in items.iter() {

			match to_dotfile_symbol((defid,xcmi,kind)) {
				None=>{},
				Some(symbol)=> {
					let url_name= xcmi.file_name+".html#"+(xcmi.line+1).to_str();
					dotf.write_line("\t\t"+symbol + "["+
						if is_main((defid,xcmi,kind)){
							"fontcolor=white, color=\"#00000040\", fontsize=32, "}
						else{
							match kind {
								CG_Trait=>"fontcolor=\"#ed9603\", fontsize=16, ",
								CG_Struct=>"fontcolor=\"#e53700\", fontsize=16, ",
								CG_Enum=>"fontcolor=\"#5e9766\", fontsize=16, ",
								CG_Fn=>"fontcolor=\"#8c6067\", fontsize=14, ",
								CG_Mod=>"fontcolor=\"#4d76ae\", fontsize=16, ",
								_=>" "
							}
						}+
						"label=\""+match kind{CG_Mod=>"mod", CG_Fn=>"fn",CG_Enum=>"enum",CG_Trait=>"trait",CG_Struct=>"struct",_=>""}
						+" "
						+xcmi.item_name+"\""
						+" URL=\""+ url_name  + "\"];"
					);
				}
			}
		}
		module_subgraph_end(dotf, 2);
	}
	// Write out all the calls..
	dotf.write_line("\tedge [len=4.0];");
	for &(f1,f2) in all_calls.iter() {
		match (to_dotfile_symbol(f1), to_dotfile_symbol(f2)) {
			
			(Some(fstr1),Some(fstr2))=>{
				let is_either=|(_,_,ref k0):RefCCMItem,(_,_,ref k1):RefCCMItem,k|->bool{*k0==k ||*k1==k};
				let edge_color=
					if is_either(f1,f2,CG_Struct){"\"#e5370020\""} else
					if is_either(f1,f2,CG_Trait){"\"#ed960320\""} else
					if is_either(f1,f2,CG_Enum){"\"#5e976620\""} else
					if is_either(f1,f2,CG_Mod){"\"#4d76ae20\""} else
					if is_either(f1,f2,CG_Fn){"\"#8c606710\""} else

					{"\"#00000020\""};
				dotf.write_line("\t"+ fstr1 +" -> "+ fstr2 + "[color="+edge_color+ "]" );
			}
			_=>{}
		}
	}
	dotf.write_line("}");
}
fn indent(depth:uint)->&'static str{
	let tabs=&'static "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t";
	if depth<tabs.len() {tabs.slice_to(depth)} else {tabs}
}
fn module_subgraph_begin(dotf:&mut fs::File,  depth:uint, mangled_name:&str,module_name:&str) {
	let indentstr=indent(depth);
	dotf.write_line(indent(depth)+"subgraph cluster_"+mangled_name+"{");
	let indentstr1=indent(depth+1);
	dotf.write_line(indentstr+"graph[");
	let indentstr2=indent(depth+2);
	dotf.write_line(indentstr2+"style=filled,");
	dotf.write_line(indentstr2+"color=\"#00000008\",");
	dotf.write_line(indentstr2+"label="+module_name);
	dotf.write_line(indentstr2+"];");
	dotf.write_line(indentstr+"node [style=filled, color=\"#00000010\"];");
	dotf.write_line(indentstr+"edge [color=\"#00000010\"];");
	
//				dotf.write_line("\t\tlabel=\""+modname+"\";");
//				dotf.write_line("\t\tcolor=darkgrey;");
}

fn  module_subgraph_end(dotf:&mut fs::File, depth:uint) {
	dotf.write_line(indent(depth)+"}");
}
fn to_dotfile_symbol((def_id,xcmi,kind):RefCCMItem)-> Option<~str> {
	// TODO: this should be the symbols' qualified module pathname
	// its only coincidentally correlated with the filename+symbol most of the time.
//	let pathname:~str=xcmi.file_name.chars().map(|x|match x{'/'|'<'|'>'|'.'=>'_',_=>x}).collect(); 
	let fname=xcmi.file_name +"_"+xcmi.item_name;
//	let cleaned_up_name:~str=
	if  xcmi.item_name.chars().filter(|&x|match x{'<'|'>'|'.'|':'=>true,_=>false}).len()>0 
		|| (xcmi.item_name.chars().nth(0)==Some('_') && xcmi.item_name.chars().nth(1)==Some('_'))
	{
		None // dont return if we had non symbol characters - its a generated item (eg deriving)
	}
	else {
		let filtered:~str=xcmi.file_name.chars().map(|x|match x{'/'|'<'|'>'|'.'=>'_',_=>x}).collect();
		let concat=filtered+"_"+ xcmi.item_name;
		if concat.len()>0 { Some(concat)} else {None}
	}
//	cleaned_up_name
}
fn is_main((def_id,xcmi,kind):RefCCMItem)->bool{
	xcmi.item_name.as_slice()=="main" && kind==CG_Fn
}



/// calls closure for each caller-callee pair encountered in the whole crates' static callgraph, including both function and method calls.
/// does so by traversing all the nodes, finding any that are function declarations, then calls a gather function that traverses the bodies looking for calls & method calls.
/// requires populated CrossCrateMap and NodeMaps

//static mut dbg1:HashSet<NodeId> =HashSet::new()

static debug_callgraph:bool=false;

//type EdgeFn<'a> = 'a|RefCCMItem<'a>, RefCCMItem<'a>|;

pub fn gather_use_graph<'a>(nmaps:&'a NodeMaps, edge_fn: &|RefCCMItem<'a>, RefCCMItem<'a>|)
{
	gather_use_graph_module(nmaps, edge_fn, &nmaps.krate.module);
}

pub fn gather_use_graph_module<'a>(nmaps:&'a NodeMaps, edge_fn: &|RefCCMItem<'a>, RefCCMItem<'a>|, module:&ast::Mod) {

	for &item in module.items.iter() {
//		let item_shared_ref = *item; // we know its valid/ lets unsafe transmute it :(
//		let item_ref:&'a ast::Item = unsafe {::std::cast::transmute(item_shared_ref)};
		gather_use_graph_item(nmaps,edge_fn, &*item);
	}

}

fn gather_use_graph_rec<'a>(edges:&mut SetOfItems<'a>,nmaps:&'a NodeMaps, edge_fn:&|RefCCMItem<'a>, RefCCMItem<'a>|, node_id:ast::NodeId)
{
	let node=nmaps.node_info_map.find(&node_id);
	if !node.is_some() {return;}
	let node=node.unwrap();


	node.rf_visit_children(
		nmaps.node_info_map,
		|child_id, child_node|{
			gather_use_graph_rec(edges, nmaps, edge_fn, child_id);
			let target=match child_node.rf_node() {
				// get the callee .. caution, its the fncalls's 'callee_expr'.id, not the caller node itself..
				astnode_expr(expr)=>{
					match expr.node {
						ast::ExprCall( ref call_expr,ref args)=>{
							(nmaps.jump_def_map.find(&call_expr.id),CG_Fn)
						},
						ast::ExprMethodCall(ref ident,ref typeargs,ref args)=>{
							(nmaps.jump_def_map.find(&child_id),CG_Fn)
						},
						_=>{(None,CG_None)}
					}
				},
				astnode_item(item)=>{
					gather_use_graph_item(nmaps, edge_fn, &(*item));
					(None,CG_None)
				},
				_=>{(None,CG_None)},
			};
			// todo: for clarity of flow, make this a sub called above - insert(calls,nmaps,target).
			match target{
				(None,_)=>{},
				(Some(x),kind)=>{
					match nmaps.rf_find_source(x) {
						Some(xcm_item)=>{edges.insert((*x,xcm_item,kind));},
						None=>{/*println!("call with no source \n", )*/} 
					}
				}
			}
		}
	);
}
fn gather_node_id_def<'s>(calls:&mut SetOfItems<'s>, nmaps:&'s NodeMaps, node_id:ast::NodeId) {
	match nmaps.jump_def_map.find(&node_id) { None=>{},
		Some(def_id)=>{
			match nmaps.xcmap.find(def_id) { None=>{},
				Some(xcm_item)=>{
					calls.insert((*def_id, xcm_item, CG_Struct));
				},
			}
		}
	}
}




// todo - walk generics..
fn gather_use_graph_item<'a>(nmaps:&'a NodeMaps<'a>, edge_fn: &|RefCCMItem<'a>, RefCCMItem<'a>|, item:&ast::Item) {
	// todo: VisitChildren can actually be done here, using the AST itself.
	// we dont traverse 'block' for the minute because its' done above.
	// using the ast directly is easier, but only when we have *everything* implemented.

	let caller_defid=ast::DefId{krate:0,node: item.id};
	let opt_caller=nmaps.rf_find_source(&caller_defid);

	let mut edge_ends:SetOfItems=HashSet::new();

	let src_kind:CG_Kind= match item.node {
		ast::ItemEnum(ref enum_def,ref generics)=>{
			for variant in enum_def.variants.iter() {
				match variant.node.kind {
					ast::TupleVariantKind(ref vec_variant_args)=> {
						for t in vec_variant_args.iter() {
							gather_type(&mut edge_ends, nmaps, t.ty);
						}
					},
					ast::StructVariantKind(struct_def)=> {
						for struct_field in struct_def.fields.iter() {
							gather_type(&mut edge_ends, nmaps, /*node, (child_id,child_node),*/&(*struct_field.node.ty));
						}
					},
//					_=>{}
				}
			}
			CG_Enum
		}
		ast::ItemImpl(ref generics,ref opt_trait_ref,ty,ref vec_method)=>{
//			println!("Impl Trait.. for {} for {}",str_of_ident(item.ident), ::syntax::print::pprust::ty_to_str(ty),  );
			// impl X for Y means an edge Y->X, (or the other way round? whatever, an edge.)
			match nmaps.jump_def_map.find(&ty.id) {None=>{},Some(ty_defid)=>
				match nmaps.xcmap.find(ty_defid) {None=>{},Some(ccmi)=>
					{edge_ends.insert( (*ty_defid,ccmi, CG_Struct) );}
				}
			};
			match *opt_trait_ref {
				Some(ref trait_ref)=>{
					gather_trait_ref(&mut edge_ends, nmaps,trait_ref);
				},
				None=>{}
			}
			gather_type(&mut edge_ends, nmaps, &(*ty));
			for method in vec_method.iter() {
				gather_fn_decl(&mut edge_ends, nmaps,edge_fn,  method.decl, Some(&(*method.body)));
			}
			CG_Struct
		}
		ast::ItemTrait(ref generics,ref vec_trait_ref,ref vec_trait_method)=>{
			for trait_ref in vec_trait_ref.iter() {
				gather_trait_ref(&mut edge_ends, nmaps, trait_ref);
			}
			for t in vec_trait_method.iter() {
				match *t {
					ast::Required(ref tm)=>{
						gather_fn_decl(&mut edge_ends, nmaps,edge_fn,  tm.decl, None);
					}
					ast::Provided(method)=>{
						gather_fn_decl(&mut edge_ends, nmaps,edge_fn,  method.decl, Some(&*method.body));
					}
				}
			}
			// handle trait function declarations..
			CG_Trait
		}
		ast::ItemFn(p_fn_decl, ref fn_style, ref abi, ref generics, p_block)=> {
			gather_fn_decl(&mut edge_ends, nmaps,edge_fn,  p_fn_decl, Some(&*p_block));
			CG_Fn
		}
		ast::ItemStruct(ref struct_def,ref generics)=>{
			for struct_field in struct_def.fields.iter() {
				gather_type(&mut edge_ends, nmaps, /*node, (child_id,child_node),*/&(*struct_field.node.ty));
			}
			CG_Struct
		}
		// nested module - todo: build a module-graph aswell - and do the collapse-logic here
		ast::ItemMod(ref mod_)=> {
			gather_use_graph_module(nmaps, edge_fn, mod_);
			CG_None
		}
		_=>{CG_None}
	};

	for &(ref other_defid,other_ccmitem,other_kind) in edge_ends.iter() {
		if debug_callgraph{
			println!("\tcalls  {}() {}:{}",other_ccmitem.item_name, other_ccmitem.file_name, other_ccmitem.line+1);
		}
		if opt_caller.is_some() {
			(*edge_fn)((caller_defid, opt_caller.unwrap(),src_kind), (*other_defid,other_ccmitem,other_kind));

		}
	}
}

fn gather_fn_decl<'a> (edge_ends:&mut SetOfItems<'a>, nmaps:&'a NodeMaps<'a>, edge_fn: &|RefCCMItem<'a>, RefCCMItem<'a>| ,  p_fn_decl:&ast::FnDecl, opt_block:Option<&ast::Block>){ 
	for arg in p_fn_decl.inputs.iter() {
		gather_type(edge_ends,nmaps, arg.ty);
	}
	gather_type(edge_ends,nmaps,p_fn_decl.output);
	match opt_block { None=>{},Some(block)=>
		gather_use_graph_rec(edge_ends, nmaps,edge_fn, block.id),	// get edges from function body.
	}
}

fn gather_trait_ref<'a>(edge_ends:&mut SetOfItems<'a>, nmaps:&'a NodeMaps<'a>, tr:&ast::TraitRef) {
//	println!(" trait..{}", ::syntax::print::pprust::path_to_str(&tr.path) );
	match nmaps.jump_def_map.find(&tr.ref_id) {None=>{},
		Some(defid)=>{
			match nmaps.xcmap.find(defid) {None=>{},
				Some(ref ccmitem) =>{
//					println!(" trait..{} .. ok", ::syntax::print::pprust::path_to_str(&tr.path) );
					edge_ends.insert( (*defid, *ccmitem,CG_Trait) );
				}
			}
		}
	}
}


fn gather_type<'s>(
		calls:&mut SetOfItems<'s>,
		nmaps:&'s NodeMaps, 
//		node:&FNodeInfo, 
//		(child_id,child_node):(ast::NodeId, &FNodeInfo),
		ty:&ast::Ty) 
{
	// iterate the type possibilities..
	match ty.node {
		ast::TyBox(t)=>gather_type(calls,nmaps,t),
		ast::TyUniq(t)=>gather_type(calls,nmaps,t),
		ast::TyVec(t)=>gather_type(calls,nmaps,t),
		ast::TyFixedLengthVec(t,expr)=>gather_type(calls,nmaps,t),
		ast::TyPtr(mut_ty)=>gather_type(calls,nmaps,mut_ty.ty),
		ast::TyRptr(opt_lifetime,mut_ty)=>gather_type(calls,nmaps,mut_ty.ty),
//		ast::TyRptr(mut_ty)=>gather_type(calls,nmaps,mut_ty),
		ast::TyTup(ref vty)=>for ty in vty.iter(){ gather_type(calls,nmaps, &(**(ty)));},
		ast::TyPath(ref path,ref opt_typarambound, ref node_id)=>{
			// node_id .. get its def ?
			match nmaps.jump_def_map.find(&ty.id) {
				None=>{
//					println!(" ty_path def not found {} :(\n", ty.id);
				},
				Some(def_id)=>{
					// pyramid of dooom..
					match nmaps.xcmap.find(def_id) {
						None=>{},
						Some(xcm_item)=>{
							let kind = CG_Struct;
//			let node_info=nmaps.node_info_map.find(def_id);
//							println!(" ty_path {}\n", xcm_item.item_name);
							calls.insert((*def_id, xcm_item, kind)   );
			
							match *opt_typarambound {None=>{},
								Some(ref owned_slice_ty_param_bound)=>{
									for ty_param_bound in owned_slice_ty_param_bound.iter(){	
										match *ty_param_bound {
											ast::TraitTyParamBound(ref trait_ref)=>{},//gather_trait_ref(calls,nmaps,trait_ref),
											_=>{}
										}
									}
								},
							}
						}
					}
				}
			}
		}
//		ast::Typeof(ref expr)=>{} /* gather_expr .. */
//		ast::TyBareFn(ref expr)=>{} /* gather_expr .. */
//		ast::TyProc(ref expr)=>{} /* gather_expr .. */
		_=>{},



	}
	
}











