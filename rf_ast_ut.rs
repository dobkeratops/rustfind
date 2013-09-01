use std::str;
use syntax::{ast,ast_map};
use rustc::middle::ty;

pub fn dump_methods_of_t(tycx:&ty::ctxt_, t:*ty::t_opaque) {
	for (&k,&method) in tycx.methods.iter() {
		dump!(method.transformed_self_ty, t);
		if method.transformed_self_ty==Some(t) {
			dump!(method);
		}
	}

}


pub fn get_struct_def<'a,'b>(tc:&'a ty::ctxt_, struct_node_id:ast::NodeId)->Option<(@ast::item,@ast::struct_def,ast::Generics)> {
	match tc.items.find(&struct_node_id) {
		None=>{None},
		Some(node)=>match *node {
			ast_map::node_item(item,ref path)=>{
				match item.node {
					ast::item_struct(sd, ref generics)=>Some((item, sd, generics.clone())),
					_=>None
				}
			}
			_=> None
		},
	}
}

pub fn find_named_struct_field(tc:&ty::ctxt_, struct_node_id:ast::NodeId, field_ident:&ast::ident)->Option<ast::def_id> {
	match get_struct_def(tc,struct_node_id) {
		None=>None,
		Some((it,sd,ge))=>{
			for f in sd.fields.iter() {
				match f.node.kind {
					ast::named_field(ref ident,vis)=>if *ident==*field_ident {return Some(ast::def_id{crate:0,node:f.node.id});},
					_=>return None
				}
			}
			None
		}
	}
}
