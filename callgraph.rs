use syntax::codemap;
use syntax::ast;
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

/// Generate callgraph file.
/// Todo - seperate into callgraph iterator and file writer passing a closure..
pub fn write_call_graph<'a>(xcm:&'a CrossCrateMap, nmaps:&NodeMaps, filename:&str) {
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
				let mut calls= HashSet::<&'a CrossCrateMapItem>::new();
				info.as_fn_decl().map(
					|(ref item, fn_decl)|{
						let xcmi=xcm.find(&ast::DefId{krate:0,node:*node_id}).unwrap();
						let caller_str = str_of_ident(item.ident);
						outf.write_line(format!("fn {}() {}:{}", caller_str, xcmi.file_name,xcmi.line+1));
						gather_call_graph_rec(&mut calls, xcm ,nmaps,  *node_id);
			
						for call_item in calls.iter() {
							outf.write_line(format!("\tcalls {}() {}:{}",call_item.item_name, call_item.file_name, call_item.line+1));
							dotf.write_line("\t"+caller_str  +" -> "+ call_item.item_name+"");
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

// todo - build Vec<(DefId/*caller*/,DefId/*callee*/)> 
fn gather_call_graph_rec<'a,'b,'c>(calls:&'a mut HashSet<&'b CrossCrateMapItem>,xcm:&'b CrossCrateMap, nmaps:&'c NodeMaps, node:ast::NodeId) 
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
					Some(xcm_item)=>{calls.insert(xcm_item);},
					None=>{} 
				}
			}
		}
	}
}
/*
				match xcm.find(x) {
					None=>{},
					Some(ref xmi)=> { //&CrossCrateMapItem
						println!("\tcalls: {}() {}:{}",xmi.item_name, xmi.file_name, xmi.line+1);
					}
				}
*/

// TODO: populate this chart!
pub struct FunctionUseGraph {
	pub fn_per_type: HashMap<ast::DefId, ast::DefId>,
	pub type_per_fn: HashMap<ast::DefId, ast::DefId>,
	pub common_pairs: HashSet<(ast::DefId,ast::DefId)>
}













