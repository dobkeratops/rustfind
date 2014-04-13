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
		info.as_fn_decl().map(|(ref item, fn_decl)|{
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
pub fn write_call_graph(xcm:&CrossCrateMap, nmaps:&NodeMaps, filename:&str,opts:&CG_Options) {
	println!("Writing callgraph {}..",filename);
	match (	fs::File::create(&posix::Path::new(filename.to_owned()+~".txt")),
			fs::File::create(&posix::Path::new(filename.to_owned()+~".dot")),
		)
	{
		(Ok(mut outf),Ok(mut dotf))=>
		{
			println!("writing callgraph file ..\n");
			dotf.write_line("digraph callgraph {");
			for (node_id, info) in nmaps.node_info_map.iter() {
				let mut calls:SetOfCalls =HashSet::new();
				info.as_fn_decl().map(
					|(ref item, fn_decl)|{
						let xcmi=xcm.find(&DefId{krate:0,node:*node_id}).unwrap();
						let caller_str = str_of_ident(item.ident);
						outf.write_line(format!("fn {}() {}:{}", caller_str, xcmi.file_name,xcmi.line+1));
						gather_call_graph_rec(&mut calls, xcm ,nmaps,  *node_id);
			
						for &(ref defid,ref call_item) in calls.iter() {
							outf.write_line(format!("\tcalls {}() {}:{}",call_item.item_name, call_item.file_name, call_item.line+1));
							// TODO: actually 'local-only' should gather external nodes into one-
							// i.e. show dependancy on module/crate.

							if !(opts.local_only && defid.krate!=0) {
								dotf.write_line("\t"+caller_str  +" -> "+ call_item.item_name+"");
							}
						}
					}
				);
			}
			dotf.write_line("}");
		}
		_ => println!("can't write callgraph {}", filename),
	}
//	fail!();
}


pub type SetOfCalls<'a> =HashSet<(DefId,&'a CrossCrateMapItem)>;

/// calls closure for each caller-callee pair encountered in the whole crates' static callgraph, including both function and method calls.
/// does so by traversing all the nodes, finding any that are function declarations, then calls a gather function that traverses the bodies looking for calls & method calls.
/// requires populated CrossCrateMap and NodeMaps
pub fn visit_call_graph<'a>(xcm:&'a CrossCrateMap, nmaps:&NodeMaps, f:|caller:(DefId, &'a CrossCrateMapItem), callee:(DefId,&'a CrossCrateMapItem)|)
{

	for (node_id, info) in nmaps.node_info_map.iter() {
		info.as_fn_decl().map(
			|(ref fn_item, fn_decl)|{
				let caller_defid=ast::DefId{krate:0,node:*node_id};
				let caller_ccmitem=xcm.find(&caller_defid).unwrap();

				let mut calls:SetOfCalls=HashSet::new();

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
fn gather_call_graph_rec<'s>(calls:&mut SetOfCalls<'s>,xcm:&'s CrossCrateMap, nmaps:&NodeMaps, node:ast::NodeId) 
{
	let node=nmaps.node_info_map.find(&node).unwrap();
	for &child_id in node.children.iter() {
		let child_node = nmaps.node_info_map.find(&child_id).unwrap();
		gather_call_graph_rec(calls, xcm,nmaps, child_id);
		let target=match child_node.node {
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
}

// TODO: populate this chart!
pub struct FunctionUseGraph {
	pub fn_per_type: HashMap<DefId, DefId>,
	pub type_per_fn: HashMap<DefId, DefId>,
	pub common_pairs: HashSet<(DefId,DefId)>
}













