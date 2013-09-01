use syntax::ast;
use rustc::middle::{ty,typeck};

use std::hashmap::HashMap;
use find_ast_node::*;
use rfindctx::*;
use rf_ast_ut::*;
use util::flatten_to_str; //todo - why is qualifying manually not working?!
//use super::rf_use_ast;


//todo - simple declartions of types shouldn't mean bringing in associated code modules ?
// a user that needs the type JumpToDefMap needn't necaserily need all its functions...

pub type JumpToDefMap = HashMap<ast::NodeId,ast::def_id> ;


pub fn lookup_def_node_of_node(dc:&RFindCtx,node:&AstNode, nodeinfomap:&FNodeInfoMap, node_def_node:&HashMap<ast::NodeId,ast::def_id>)->Option<ast::def_id> {
	
	match *node {
		astnode_expr(e)=>match e.node {
			// handle methods-calls
			ast::expr_method_call(ref id,ref receiver,ref ident,ref ty_params,ref arg_exprs,ref call_sugar)=>{
				let rec_ty_node= astnode_expr(*receiver).ty_node_id();
				let rec_ty_node1= dc.tycx.node_types.find(&(*id as uint));

				match dc.ca.maps.method_map.find(&e.id) {
					None=> {},//logi!("no method map entry for",e.id),
					Some(mme)=>{
						//logi!("Method Map entry for",e.id);
						match mme.origin {
							typeck::method_static(def_id)=> 
								return Some(def_id),
							typeck::method_trait(def_id,_)=>
								return Some(def_id),
							typeck::method_param(mp)=>{
								match dc.tycx.trait_method_def_ids.find(&mp.trait_id) {
									None=>{},
									Some(method_def_ids)=>{
										return Some(method_def_ids[mp.method_num])
									}
								}
							}
						}
					}
				}
			},
			// handle struct-fields? "object.field"
			ast::expr_field(ref object_expr,ref ident,ref ty_params)=>{
				// we want the type of the object..
				let obj_ty=dc.tycx.node_types.find(&(object_expr.id as uint));
				let tydef=/*rf_ast_ut::*/auto_deref_ty(ty::get(*obj_ty.unwrap()));
				match tydef.sty {
					ty::ty_struct(def,_)=> {
						let node_to_show=/*rf_ast_ut::*/find_named_struct_field(dc.tycx, def.node, ident).unwrap_or_default(def);
						return Some(node_to_show);//mk_result(dc,m,node_spans,node_to_show,"(struct_field)");
					},
					_=>return None
				}
			}
			_=>{}
		},
		_=>{}

	}

	// handle everything else
	match node.ty_node_id() {
		Some(id) =>{
			let (def_id,opt_info)= def_info_from_node_id(dc,nodeinfomap,id); 
			return if def_id != ast::def_id{crate:0,node:id} {Some(def_id)} else {None}
/*			match opt_info {
				Some(info)=> {
					return Some(def_id);
				},
				_=>{ println("can't find def for"+node.get_id().to_str()+".ty_node_id="+id.to_str()); //return None;
					//let (def_id,opt_info)= def_info_from_node_id(dc,nodeinfomap,node.get_id().unwrap()); 
					//match opt_info {
					//	Some(info)=> {return Some(def_id);}
					//	None=>{}
					//}
				}
			}
*/
		},
		None=> {}
	};
	return None;
}

pub fn build_jump_to_def_map(dc:&RFindCtx, nim:@mut FNodeInfoMap,nd:&HashMap<ast::NodeId,ast::def_id>)->~JumpToDefMap{
// todo: NodeId->AStNode  .. lookup_def_ inner functionality extracted
	let mut jdm=~HashMap::new();
	for (k,node_info) in nim.iter() {
		match lookup_def_node_of_node(dc,&node_info.node,nim,nd) {
			None=>{},
			Some(def_node_id)=>{
//				if *k != def_node_id.node && def_node_id.crate==0 || (def_node_id.crate!=0) 
				{
					jdm.insert(*k,def_node_id);
				}
			}
		}
	}
	jdm
}

pub fn def_info_from_node_id<'a,'b>(dc:&'a RFindCtx, node_info:&'b FNodeInfoMap, id:ast::NodeId)->(ast::def_id,Option<&'b FNodeInfo>) {
	let crate_num=0;
	match dc.tycx.def_map.find(&id) { // finds a def..
		Some(a)=>{
			match get_def_id(crate_num,*a){
				Some(b)=>
					(b,node_info.find(&b.node)),
//				match b.crate {
//					0=>(b.node,node_info.find(&b.node)),
//					_ => (id as int, None)
//				},
				None=>(ast::def_id{crate:0,node:id as int},None)
			}
		},
		None=>(ast::def_id{crate:0,node:id as int},None)
	}
}



pub fn dump_json(dc:&RFindCtx) {
	// TODO: full/partial options - we currently wwrite out all the nodes we find.
	// need option to only write out nodes that map to definitons. 
	println("{");
	println("\tcode_map:[");
//	for dc.sess.codemap.files.iter().advance |f| {
	for f in dc.sess.codemap.files.iter() {
		print("\t\t{ name:\""+f.name+"\",\tglobal_start_pos:"+f.start_pos.to_str()+
			",\tlength:"+(f.src.len()).to_str()+
			",\tnum_lines:"+f.lines.len().to_str()+
			",\tlines:[\n"+ flatten_to_str(*f.lines, |&x|{*x-*f.start_pos} ,",") +
			"\n\t\t]\n\t},\n");
	}
	println("\t]");
	println("\tnode_spans:");
	let nim=build_node_info_map(dc.crate);
	let node_def_node = build_node_def_node_table(dc);
	let jdm=build_jump_to_def_map(dc,nim,node_def_node);
	println(nim.to_json_str(dc));	
	println(",");
	println("\tnode_defs [\n");
	println(jdm.to_json_str());
	println("\t],\n");
	println("\tdef_ids:");
	println(node_def_node.to_json_str());
	println("}");
}
