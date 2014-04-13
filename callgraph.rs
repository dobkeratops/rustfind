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
use jumptodefmap::{NodeMaps};
use rsfind::MyOption;
use timer::Profiler;
use find_ast_node::*;
use rfindctx::*;

// build a call graph.
// we also want to build a graph of users of types - including types.

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
type RefCCMItem<'a> =(DefId,&'a CrossCrateMapItem);
type SetOfItems<'a> =HashSet<RefCCMItem<'a>>;
type TraitMap<'a> =HashMap<DefId,TraitInfo<'a>>;

pub fn write_call_graph(xcm:&CrossCrateMap, nmaps:&NodeMaps, outdirname:&str, filename:&str,opts:&CG_Options) {

	let mut tg:TraitMap =HashMap::new();
//	gather_trait_graph(&tg, (xcm,nmaps), rf_get_root_node(nmaps.node_info_map));

	println!("Writing callgraph {} {}..",outdirname, filename);
	match (	fs::File::create(&posix::Path::new(outdirname+filename.to_owned()+~".txt")),
			fs::File::create(&posix::Path::new(outdirname+filename.to_owned()+~".dot")),
		)
	{
		(Ok(mut outf),Ok(mut dotf))=>
		{
			println!("writing callgraph file ..\n");
			dotf.write_line("digraph "+ filename +" {");
			dotf.write_line("\toverlap=false; ");
			dotf.write_line("\tedge[length=5.0] ");
			let mut fns_per_module:HashMap<&str,HashSet<RefCCMItem>> =HashMap::new();
			let mut all_calls:HashSet<(RefCCMItem,RefCCMItem)> =HashSet::new();

			// Traverse and collect.. 
			for (node_id, info) in nmaps.node_info_map.iter() {
				let mut calls:SetOfItems =HashSet::new();
				info.rf_as_fn_decl().map(
					|(ref item, fn_decl)|{
						let fn_defid=DefId{krate:0,node:*node_id};
						let xcmi=xcm.find(&fn_defid).unwrap();

						fns_per_module.insert_or_update_with(
							xcmi.file_name.as_slice(),	// key,
							HashSet::new(),	// new value if not found,
							|k,v|{v.insert((fn_defid, xcmi));} // update the value if found.
						);
						let caller_str = str_of_ident(item.ident);
						outf.write_line(format!("fn {}() {}:{}", caller_str, xcmi.file_name,xcmi.line+1));
						gather_call_graph_rec(&mut calls, xcm ,nmaps,  *node_id);

			
						for &(ref defid,ref call_item) in calls.iter() {
							outf.write_line(format!("\tcalls {}() {}:{}",call_item.item_name, call_item.file_name, call_item.line+1));
							// TODO: actually 'local-only' should gather external nodes into one-
							// i.e. show dependancy on module/crate.

							if !(opts.local_only && defid.krate!=0) {
								all_calls.insert( ((fn_defid,xcmi), (*defid, *call_item)) );
							}
						}
					}
				);
			}
			// Write out a cluster for all the functions in a particular sourcefile.
			// TODO - should be able to recursively cluster directories for this..
			for (&modname,items) in fns_per_module.iter() {
//				let modstr=::std::str::from_chars(modname.chars().map(|x|if x=='/'{'_'}else{x}));
				let modstr:~str=modname.chars().map(|x|match x{'/'|'.'=>'_',_=>x}).collect();
				if items.len()==0{ continue;}
				if modstr.chars().nth(0)==Some('<') {continue;} // things like <std macros>
				dotf.write_line("\tsubgraph cluster_"+modstr+"{");
				dotf.write_line("\t\tlabel=\""+modname+"\"");
				dotf.write_line("\t\tlabel=\""+modname+"\"");
				for &(defid,xcmi) in items.iter() {
					let url_name= xcmi.file_name+".html#"+(xcmi.line+1).to_str();
					dotf.write_line("\t\t"+xcmi.item_name + "["+"label=\""+xcmi.item_name+"()\" URL=\""+ url_name  + "\"];");
				}
				dotf.write_line("\t}");
			}
			// Write out all the calls..
			for &(f1,f2) in all_calls.iter() {
				
				dotf.write_line("\t"+ *fn_to_str(f1)  +" -> "+ *fn_to_str(f2)+";");
			}

			fn fn_to_str<'a>((def_id,xcmi):RefCCMItem<'a>)->&'a ~str {
				&xcmi.item_name
			}

//			fn rank_within(items:&Set<RefCCMItem>, &Vec<(RefCCMItem,RefCCMItem>

			dotf.write_line("}");
		}
		_ => println!("can't write callgraph {}", filename),
	}
//	fail!();
}


/// calls closure for each caller-callee pair encountered in the whole crates' static callgraph, including both function and method calls.
/// does so by traversing all the nodes, finding any that are function declarations, then calls a gather function that traverses the bodies looking for calls & method calls.
/// requires populated CrossCrateMap and NodeMaps
pub fn visit_call_graph<'a>(xcm:&'a CrossCrateMap, nmaps:&NodeMaps, f:|caller:(DefId, &'a CrossCrateMapItem), callee:(DefId,&'a CrossCrateMapItem)|)
{
	for (node_id, info) in nmaps.node_info_map.iter() {
		info.rf_as_fn_decl().map(
			|(ref fn_item, fn_decl)|{
				let caller_defid=ast::DefId{krate:0,node:*node_id};
				let caller_ccmitem=xcm.find(&caller_defid).unwrap();

				let mut calls:SetOfItems=HashSet::new();

				gather_call_graph_rec(&mut calls, xcm ,nmaps,  *node_id);

				for &(ref defid,call_ccmitem) in calls.iter() {
					println!("\tcalls  {}() {}:{}",call_ccmitem.item_name, call_ccmitem.file_name, call_ccmitem.line+1);
					f((caller_defid, caller_ccmitem), (*defid,call_ccmitem));
				}
			}
		);
	}
}

// todo - build Vec<(DefId/*caller*/,DefId/*callee*/)> 
fn gather_call_graph_rec<'s>(calls:&mut SetOfItems<'s>,xcm:&'s CrossCrateMap, nmaps:&NodeMaps, node:ast::NodeId) 
{
	let node=nmaps.node_info_map.find(&node).unwrap();

	// todo 'for child in iter_children(nmaps,node)' {
//	for &child_id in node.children.iter() {
	node.rf_visit_children(nmaps.node_info_map, |child_id, child_node|{
//			let child_node = nmaps.node_info_map.find(&child_id).unwrap();
			gather_call_graph_rec(calls, xcm,nmaps, child_id);
			let target=match child_node.rf_node() {
				astnode_expr(expr)=>{
					match expr.node {
						ast::ExprCall(ref expr,ref args)=>{
							nmaps.jump_def_map.find(&child_id)
						},
						ast::ExprMethodCall(ref ident,ref typeargs,ref args)=>{
							nmaps.jump_def_map.find(&child_id)
						},
						_=>{None}
					}
				},
				_=>{None},
			};
			match target{
				None=>{},
				Some(x)=>{
					match xcm.find(x) {
						Some(xcm_item)=>{calls.insert((*x,xcm_item));},
						None=>{} 
					}
				}
			}
		}
	);
}

struct TraitInfo<'a> {
	pub ti_defid: RefCCMItem<'a>,
	pub ti_name:~str,
	pub ti_module:DefId,
	pub ti_inherits:HashSet<DefId>,
	pub ti_functions:HashSet<DefId>,
}

fn gather_trait_graph_rec<'a>(tg:&mut HashMap<DefId,TraitInfo<'a>>, (xcm,nmaps):(&'a CrossCrateMap,&NodeMaps), node_id:ast::NodeId) 
{
	// todo 'for child in iter_children(nmaps,node)' {
	let node=nmaps.node_info_map.find(&node_id).unwrap();
	node.rf_visit_children(
		nmaps.node_info_map,
		|child_id, child_node| {
//	for &child_id in node.children.iter() {
//			let child_node = nmaps.node_info_map.find(&child_id).unwrap();

			match child_node.rf_node() {
				// is it in decl, or item?!
				astnode_item(item)=> {
					match item.node {
						// to escape the pyramid of doom, we want to pass ::ItemTrait, but its not a type itself :( we hope rust gets this addition
						ast::ItemTrait(ref g,ref tr, ref tm)=>{
							gather_trait_graph_sub((g,tr,tm),tg,(xcm,nmaps),child_node);
						},
						_=>{}
					}
				}
				_=>{
				}
			}
			gather_trait_graph_rec(tg, (xcm,nmaps), child_id);
		}
	);
}

// we hope they will in future allow inference between functions in the same module-
// easier to break up and refactor.
// the only reason we're writing this function is to reduce indentation above.
fn gather_trait_graph_sub<'a>(
		(g,tr,rm):(&ast::Generics,&Vec<ast::TraitRef>,&Vec<ast::TraitMethod>),
		tg:&mut HashMap<DefId, TraitInfo<'a>>,
		(xcm,nmaps):(&'a CrossCrateMap,&NodeMaps),
		node:&FNodeInfo)
{
	println!("trait {}\n", str_of_opt_ident(node.rf_get_ident()));
}

// TODO: populate this 
pub struct FunctionUseGraph {
	pub fn_per_type: HashMap<DefId, DefId>,
	pub type_per_fn: HashMap<DefId, DefId>,
	pub common_pairs: HashSet<(DefId,DefId)>
}













