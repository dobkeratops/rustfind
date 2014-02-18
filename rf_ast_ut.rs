use syntax::{ast,ast_map};
use rustc::middle::ty;


pub fn auto_deref_ty<'a> (t: &'a ty::t_box_) -> &'a ty::t_box_ {
	match t.sty {
        ty::ty_box(p) 
        | ty::ty_uniq(p) => {
            ty::get(p)
        },
        ty::ty_ptr(p)
        | ty::ty_rptr(_, p) => {
			ty::get(p.ty)
		},
		_ => t
	}
}


pub fn dump_ctxt_def_map(tycx:ty::ctxt) {
//	let a:()=ctxt.tycx.node_types
	logi!("===Test ctxt def-map table..===");
    let def_map = tycx.def_map.borrow().get();
	for (key,value) in def_map.iter(){
		dump!(key,value);
	}
}

/*
pub fn dump_methods_of_t(tycx:ty::ctxt, t:*ty::t_opaque) {
	for (&k,&method) in tycx.methods.iter() {
		dump!(method.transformed_self_ty, t);
		if method.transformed_self_ty==Some(t) {
			dump!(method);
		}
	}

}

pub fn dump_methods_of_type(tycx:ty::ctxt, type_node_id:ast::NodeId) {
	let ot = tycx.node_types.find(&(type_node_id as uint));
	match ot {
		None=> {},
		Some(t)=> {
			for (&k,&method) in tycx.methods.iter() {
				dump!(method.transformed_self_ty, ot);
				if method.transformed_self_ty==Some(*t) {
					dump!(method);
				}
			}
		}
	}
}
*/


pub fn get_struct_def<'a,'b>(tc:&'a ty::ctxt, struct_node_id:ast::NodeId)->Option<(@ast::Item,@ast::StructDef,ast::Generics)> {
	match tc.map.find(struct_node_id) {
		None=>{None},
		Some(node)=>match node {
			ast_map::NodeItem(item)=>{
				match item.node {
					ast::ItemStruct(sd, ref generics)=>Some((item, sd, generics.clone())),
					_=>None
				}
			}
			_=> None
		},
	}
}

pub fn find_named_struct_field(tc:&ty::ctxt, struct_node_id:ast::NodeId, field_ident:&ast::Ident)->Option<ast::DefId> {
	match get_struct_def(tc,struct_node_id) {
		None=>None,
		Some((_, sd, _))=>{
			for f in sd.fields.iter() {
				match f.node.kind {
					ast::NamedField(ref ident, _)=>if ident.name ==field_ident.name {return Some(ast::DefId{krate:0,node:f.node.id});},
					_=>return None
				}
			}
			None
		}
	}
}
