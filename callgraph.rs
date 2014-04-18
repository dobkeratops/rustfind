use syntax::ast;
use syntax::ast::DefId;
use std::path::posix;
use collections::{HashMap,HashSet};
use std::io::IoResult;
use std::io::fs;
use crosscratemap::CrossCrateMapItem;
use find_ast_node::{FNodeInfo, astnode_item, astnode_expr};
pub use super::NodeMaps;
use rfindctx::{str_of_ident, str_of_opt_ident};

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

// TODO: Use mangled symbols for the nodes.
pub fn write_call_graph<'a>(nmaps:&'a NodeMaps, filename: &posix::Path, name:&str, opts:&CG_Options) -> IoResult<()> {

	let mut tg:TraitMap =HashMap::new();
//	gather_trait_graph(&tg, (xcm,nmaps), rf_get_root_node(nmaps.node_info_map));

	visit_call_graph( nmaps, |x,y|{});

	let mut dotf = try!(fs::File::create(filename));
	{
		{
			println!("writing callgraph file ..\n");
			try!(dotf.write_line("digraph "+ name +" {"));
			let mut fns_per_module:HashMap<&str,HashSet<RefCCMItem>> =HashMap::new();
			let mut all_calls:HashSet<(RefCCMItem<'a>,RefCCMItem<'a>)> =HashSet::new();
			try!(dotf.write_line("\tnode [style=filled, color=lightgrey ]"));

			visit_call_graph(nmaps, // We miss do notation :(
				|caller,callee| {	
					for x in [caller,callee].iter() {
						fns_per_module.insert_or_update_with(x.val1().file_name.as_slice(), HashSet::new(), |k,v|{v.insert(*x);});
					}

					if !(opts.local_only && callee.val0().krate!=0) {
						all_calls.insert( (caller, callee) );
					}
				}
			);

			// Write out a cluster for all the functions in a particular sourcefile.
			// TODO - should be able to recursively cluster directories for this..
			// TODO: Generalize to modules.
			for (&modname,items) in fns_per_module.iter() {
//				let modstr=::std::str::from_chars(modname.chars().map(|x|if x=='/'{'_'}else{x}));
				let modstr:~str=modname.chars().map(|x|match x{'/'|'.'=>'_',_=>x}).collect();
				if items.len()==0{ continue;}
				if modstr.chars().nth(0)==Some('<') {continue;} // things like <std macros>
				try!(dotf.write_line("\tsubgraph cluster_"+modstr+"{"));
				try!(dotf.write_line("\t\tlabel=\""+modname+"\""));
				try!(dotf.write_line("\t\tstyle=filled"));
				try!(dotf.write_line("\t\tcolor=darkgrey"));
				for &(defid,xcmi) in items.iter() {
					let url_name= xcmi.file_name+".html#"+(xcmi.line+1).to_str();
					try!(dotf.write_line("\t\t"+fn_to_dotfile_symbol((defid,xcmi)) + "["+
						if is_main((defid,xcmi)){"style=filled,fontcolor=white color=blue, "}else{" "/*"style=filled, color=lightgrey "*/}+
						"label=\""+xcmi.item_name+"()\" URL=\""+ url_name  + "\"];"));
				}
				try!(dotf.write_line("\t}"));
			}
			// Write out all the calls..
			try!(dotf.write_line("edge [len=4.0];"));
			for &(f1,f2) in all_calls.iter() {
				let fstr1=fn_to_dotfile_symbol(f1);
				let fstr2=fn_to_dotfile_symbol(f2);
				if fstr1.len()>0 && fstr2.len()>0 {
					
					try!(dotf.write_line("\t"+ fstr1 +" -> "+ fstr2
//						if is_main(f1)|| is_main(f2) {" [len=10.0];"} else {" [len=4.0];"}
					));
				}
			}

			fn fn_to_dotfile_symbol((def_id,xcmi):RefCCMItem)-> ~str {
				// TODO: this should be the symbols' qualified module pathname
				// its only coincidentally correlated with the filename+symbol most of the time.
				let pathname:~str=xcmi.file_name.chars().map(|x|match x{'/'|'.'=>'_',_=>x}).collect(); 
				pathname +"_"+xcmi.item_name
			}
			fn is_main((def_id,xcmi):RefCCMItem)->bool{
				xcmi.item_name.as_slice()=="main"
			}

//			fn rank_within(items:&Set<RefCCMItem>, &Vec<(RefCCMItem,RefCCMItem>
			dotf.write_line("}")
		}
	}
}


/// calls closure for each caller-callee pair encountered in the whole crates' static callgraph, including both function and method calls.
/// does so by traversing all the nodes, finding any that are function declarations, then calls a gather function that traverses the bodies looking for calls & method calls.
/// requires populated CrossCrateMap and NodeMaps

//static mut dbg1:HashSet<NodeId> =HashSet::new()
static debug_callgraph:bool=false;
pub fn visit_call_graph<'a>(nmaps:&'a NodeMaps, f:|caller:RefCCMItem<'a>, callee:RefCCMItem<'a>|)
{
//	let mut dbg_calls_found_recursively=0;
	for (node_id, info) in nmaps.node_info_map.iter() {
		info.rf_as_fn_decl().map(
			|(ref fn_item, fn_decl)|{
				let caller_defid=ast::DefId{krate:0,node:*node_id};
				let caller_ccmitem=nmaps.rf_find_source(&caller_defid).unwrap();

				let mut calls:SetOfItems=HashSet::new();
				gather_call_graph_rec(&mut calls, nmaps,  *node_id);

				if debug_callgraph{
					println!("{}() {}:{}",caller_ccmitem.item_name, caller_ccmitem.file_name, caller_ccmitem.line+1);
				}
				for &(ref defid,call_ccmitem) in calls.iter() {
					if debug_callgraph{
						println!("\tcalls  {}() {}:{}",call_ccmitem.item_name, call_ccmitem.file_name, call_ccmitem.line+1);
					}
					f((caller_defid, caller_ccmitem), (*defid,call_ccmitem));
				}
			}
		);
	}
}

fn gather_call_graph_rec<'s>(calls:&mut SetOfItems<'s>,nmaps:&'s NodeMaps, node:ast::NodeId)
{
	let node=nmaps.node_info_map.find(&node).unwrap();

	// todo 'for child in iter_children(nmaps,node)' {
//	for &child_id in node.children.iter() {
	node.rf_visit_children(
		nmaps.node_info_map,
		|child_id, child_node|{
			gather_call_graph_rec(calls, nmaps, child_id);
			let target=match child_node.rf_node() {
				// get the callee .. caution, its the fncalls's 'callee_expr'.id, not the caller node itself..
				astnode_expr(expr)=>{
					match expr.node {
						ast::ExprCall( ref call_expr,ref args)=>{
							nmaps.jump_def_map.find(&call_expr.id)
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
					match nmaps.rf_find_source(x) {
						Some(xcm_item)=>{calls.insert((*x,xcm_item));},
						None=>{/*println!("call with no source \n", )*/} 
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


fn gather_trait_graph_rec<'a>(tg:&mut HashMap<DefId,TraitInfo<'a>>, nmaps:&'a NodeMaps, node_id:ast::NodeId) 
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
							gather_trait_graph_sub((g,tr,tm),tg,nmaps,child_node);
						},
						_=>{}
					}
				}
				_=>{
				}
			}
			gather_trait_graph_rec(tg, nmaps, child_id);
		}
	);
}

// we hope they will in future allow inference between functions in the same module-
// easier to break up and refactor.
// the only reason we're writing this function is to reduce indentation above.
fn gather_trait_graph_sub<'a>(
		(g,tr,rm):(&ast::Generics,&Vec<ast::TraitRef>,&Vec<ast::TraitMethod>),
		tg:&mut HashMap<DefId, TraitInfo<'a>>,
		nmaps:&'a NodeMaps,
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













