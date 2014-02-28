use rf_common::*;
use syntax::ast;
use rustc::middle::{ty,typeck};
use syntax::codemap::BytePos;
use rsfind::ShowDefMode;

use find_ast_node::{FNodeInfoMap, FNodeInfo, AstNode, NodeTreeLoc, find_node_tree_loc_at_byte_pos,
	build_node_def_node_table, build_node_info_map, get_node_source, astnode_expr,
	get_def_id, byte_pos_from_text_file_pos_str};
use rfindctx::{RFindCtx,get_source_loc};
use codemaput::ZTextFilePos;
use rf_ast_ut::{auto_deref_ty, find_named_struct_field};
use util::flatten_to_str; //todo - why is qualifying manually not working?!
//use super::rf_use_ast;


//todo - simple declartions of types shouldn't mean bringing in associated code modules ?
// a user that needs the type JumpToDefMap needn't necaserily need all its functions...


pub macro_rules! if_some {
	($b:ident in $a:expr then $c:expr)=>(
		match $a {
			Some($b)=>$c,
			None=>{}
		}
	);
}

pub type JumpToDefMap = HashMap<ast::NodeId,ast::DefId> ;


pub fn lookup_def_node_of_node(dc:&RFindCtx,node:&AstNode, nodeinfomap:&FNodeInfoMap, _: &HashMap<ast::NodeId,ast::DefId>)->Option<ast::DefId> {

	match *node {
		astnode_expr(e)=>match e.node {
			// handle methods-calls
			ast::ExprMethodCall(..)=>{
				// Currently unused
// 				let rec_ty_node= astnode_expr(*receiver).ty_node_id();
// 				let rec_ty_node1= dc.tycx.node_types.find(&(*id as uint));

				match dc.ca.maps.method_map.find(&e.id) {
					None=> {},//logi!("no method map entry for",e.id),
					Some(mme)=>{
						//logi!("Method Map entry for",e.id);
						match mme.origin {
							typeck::method_static(def_id)=>
								return Some(def_id),
							typeck::method_object(_)=>
// 								return Some(mp.trait_id),
								return None,
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
			ast::ExprField(ref object_expr, ref ident, _)=>{
				// we want the type of the object..
				let obj_ty=dc.tycx.node_types.find(&(object_expr.id as uint));
				let tydef=/*rf_ast_ut::*/auto_deref_ty(ty::get(*obj_ty.unwrap()));
				match tydef.sty {
					ty::ty_struct(def,_)=> {
						let node_to_show=/*rf_ast_ut::*/find_named_struct_field(&dc.tycx, def.node, ident).unwrap_or(def);
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
			let (def_id, _)= def_info_from_node_id(dc,nodeinfomap,id);
			return if def_id != ast::DefId{crate:0,node:id} {Some(def_id)} else {None}
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

pub fn build_jump_to_def_map(dc:&RFindCtx, nim:@mut FNodeInfoMap,nd:&HashMap<ast::NodeId,ast::DefId>)->~JumpToDefMap{
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

pub fn def_info_from_node_id<'a,'b>(dc:&'a RFindCtx, node_info:&'b FNodeInfoMap, id:ast::NodeId)->(ast::DefId,Option<&'b FNodeInfo>) {
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
				None=>(ast::DefId{crate:0,node:id},None)
			}
		},
		None=>(ast::DefId{crate:0,node:id},None)
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


fn lookup_def_at_file_line_pos_old(dc:&RFindCtx,filepos:&str, show_all:ShowDefMode)->Option<~str> {

	let toks:~[&str]=filepos.split(':').collect();
	if toks.len()<3 { return None }

//	let line:Option<uint> = FromStr::from_str(toks[1]);
	if_some!(line in from_str::<u32>(toks[1]) then {
		if_some!(col in from_str::<u32>(toks[2]) then {
			//todo - if no column specified, just lookup everything on that line!

			match ZTextFilePos::new(toks[0],line-1,col-1).to_byte_pos(dc.tycx) {
				None=>{},
				Some(bp)=>{
					return lookup_def_at_byte_pos(dc,bp,show_all)
				}
			}
		})
	})
	return None;
}


pub fn lookup_def_at_text_file_pos(dc:&RFindCtx, tfp:&ZTextFilePos, show_mode:ShowDefMode)->Option<~str> {
	match tfp.to_byte_pos(dc.tycx) {
		None=>None,
		Some(bp)=>lookup_def_at_byte_pos(dc,bp,show_mode)
	}
}

pub fn lookup_def_at_text_file_pos_str(dc:&RFindCtx,file_pos_str:&str, show_mode:ShowDefMode)->Option<~str> {
	match byte_pos_from_text_file_pos_str(dc,file_pos_str) {
		None=>None,
		Some(bp)=>lookup_def_at_byte_pos(dc,bp,show_mode),
	}
}

pub fn node_id_from_text_file_pos_str(dc:&RFindCtx, file_pos_str:&str)->Option<ast::NodeId> {
	match node_from_text_file_pos_str(dc, file_pos_str) {
		None=>None,
		Some(an)=>an.get_id()
	}
}
pub fn node_from_text_file_pos_str(dc:&RFindCtx, file_pos_str:&str)->Option<AstNode> {
	match byte_pos_from_text_file_pos_str(dc,file_pos_str) {
		Some(bp)=>{let ndt=find_node_tree_loc_at_byte_pos(dc.crate,bp);Some(ndt.last().clone())},
		None=>None
	}
}




pub fn lookup_def_at_byte_pos(dc:&RFindCtx, bp:BytePos, m:ShowDefMode)->Option<~str> {
	let ndt=find_node_tree_loc_at_byte_pos(dc.crate,bp);
	lookup_def_of_node_tree_loc(dc,&ndt,m)
}

pub fn lookup_def_of_node_tree_loc(dc:&RFindCtx,node_tree_loc:&NodeTreeLoc,m:ShowDefMode)->Option<~str> {
	lookup_def_of_node(dc,node_tree_loc.last(),m)
}

pub fn lookup_def_of_node(dc:&RFindCtx,node:&AstNode,m:ShowDefMode)->Option<~str> {
	println("def of node:"+node.get_id().unwrap_or(0).to_str());
	let node_spans=build_node_info_map(dc.crate);
	let node_def_node = build_node_def_node_table(dc);
	lookup_def_of_node_sub(dc,node,m,node_spans,node_def_node)
}


pub fn lookup_def_of_node_sub(dc:&RFindCtx,node:&AstNode,m:ShowDefMode,nim:&FNodeInfoMap, node_def_node:&HashMap<ast::NodeId,ast::DefId>)->Option<~str> {
	// TODO - cache outside?


	fn mk_result(dc:&RFindCtx,  m:ShowDefMode, nim:&FNodeInfoMap, def_node_id:ast::DefId, _: &str)->Option<~str> {
		if def_node_id.crate!=0 {
			Some(~"{cross-crate-def not implemented, "+def_node_id.to_str()+"}")
		}
		else {
			match nim.find(&def_node_id.node) {
				None=>None,
				Some(def_info)=>{
					let loc=get_source_loc(dc,def_info.span.lo);
					let def_pos_str=
						loc.file.name + ":"+loc.line.to_str()+": "+
							match m { SDM_LineCol=>loc.col.to_str()+": ", _ =>~"" }+"\n";
					return	match m{
						SDM_Source=>Some(def_pos_str+get_node_source(dc.tycx,nim, def_node_id)+"\n"),
						SDM_GeditCmd=>Some("+"+loc.line.to_str()+" "+loc.file.name+" "),
						_ => Some(def_pos_str)
					};

				}
			}
		}
	}
	match lookup_def_node_of_node(dc, node, nim, node_def_node) {
		None=>None,
		Some(def_node_id)=>mk_result(dc,m, nim,def_node_id, "")
	}
}

pub fn make_jdm(dc:&RFindCtx)->(@mut FNodeInfoMap, ~HashMap<ast::NodeId,ast::DefId>,~JumpToDefMap)
{
    let nim=build_node_info_map(dc.crate);
    let ndm=build_node_def_node_table(dc);
    let jdm=build_jump_to_def_map(dc,nim,ndm);
    (nim,ndm,jdm)
}
