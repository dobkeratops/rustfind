use syntax::codemap;
use syntax::ast;
use syntax::codemap::Pos;
use rustc::middle::ty;
use std::hash::Hash;
use collections::{HashMap,HashSet};
use std::slice;
use std::cmp;
use std::io;
use std::io::fs;
use codemaput::{ZIndexFilePos,ToZIndexFilePos};
use find_ast_node::{FNodeInfoMap,FNodeInfo,AstNode_};
use rfindctx::{RustFindCtx};
use crosscratemap::{CrossCrateMap};
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
			println!("{} fn {}(..)", node_id, str_of_ident(item.ident));
		});
	}
//	fail!();
}

pub fn dump_callgraph(xcm:&CrossCrateMap, nmaps:&NodeMaps) {
	for (node_id, info) in nmaps.node_info_map.iter() {
		info.as_fn_decl().map(|(ref item, fn_decl)|{
			println!("node={} fn {}(..)", node_id, str_of_ident(item.ident));
			dump_calls(xcm,nmaps,  *node_id);
		});
	}
//	fail!();
}

// todo - build Vec<(DefId/*caller*/,DefId/*callee*/)> 
pub fn dump_calls(xcm:&CrossCrateMap, nmaps:&NodeMaps, node:ast::NodeId) 
{
	let node=nmaps.node_info_map.find(&node).unwrap();
	for &child_id in node.children.iter() {
		let child_node = nmaps.node_info_map.find(&child_id).unwrap();
		dump_calls(xcm,nmaps, child_id);
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
				println!("\tcalls: {} {}", x, xcm.find(x).map(|x|x.item_name.clone().unwrap_or(~"<same_crate>")).unwrap_or(~"<unnamed>"));
			}
		}
	}
}

// TODO: populate this chart!
pub struct FunctionUseGraph {
	pub fn_per_type: HashMap<ast::DefId, ast::DefId>,
	pub type_per_fn: HashMap<ast::DefId, ast::DefId>,
	pub common_pairs: HashSet<(ast::DefId,ast::DefId)>
}













