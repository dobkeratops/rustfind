use rf_common::*;
pub use syntax::{ast,abi};
pub use syntax::visit;
pub use syntax::parse::token;
pub use syntax::visit::{Visitor};
pub use syntax::codemap;
pub use syntax::codemap::BytePos;
pub use rfindctx::*;
//use rustc::middle::mem_categorization::ast_node;
use rustc::middle::ty;
use rfindctx::{RustFindCtx,};
use codemaput::{ZTextFilePos,ToZIndexFilePos,dump_span,get_span_str};
use find_ast_node::{AstNodeAccessors,FNodeInfo,FNodeInfoMap,AstNode_,mkAstSPtrClone,AstSPtr,astnode_trait_ref,astnode_variant,astnode_item,KindToStr,NodeTreeLoc,NodeKind};
use fa=find_ast_node;


pub struct FNodeInfoMapBuilder {
	pub all_nodes: FNodeInfoMap
}

pub fn FNodeInfoMapBuilder_new() -> FNodeInfoMapBuilder {
	FNodeInfoMapBuilder {
		all_nodes: FNodeInfoMap::new()
	}
}


impl self::FNodeInfoMapBuilder {
//	use visit_rust_ast::FNodeInfoMapBuilder;

    pub fn trait_ref(&mut self, tr:&ast::TraitRef, p: ast::NodeId) {
        push_span(&mut self.all_nodes, tr.ref_id, p,None, fa::NK_TraitRef, tr.path.span, astnode_trait_ref(mkAstSPtrClone(tr)) );
    }

    pub fn variant(&mut self, va:&ast::Variant, p: ast::NodeId) {
        push_span(&mut self.all_nodes, va.node.id,p, Some(va.node.name),fa::NK_Variant, va.span, astnode_variant(mkAstSPtrClone(&va.node)))
//       visit_item(va,(s,va.node.id,v)) - TODO , are we actually suppoed to iterate here? why was't it done
    }
}

pub fn rf_push_parent_child(spt:&mut FNodeInfoMap, parent_id:ast::NodeId, child_id: ast::NodeId) {
	let parent_node = spt.find_mut(&parent_id);
	match (parent_node) {
		Some(p)=> p.children.push(child_id),
		_=>{},
	}
}

pub fn push_span(spt:&mut FNodeInfoMap,node_id:ast::NodeId, parent:ast::NodeId, _:Option<ast::Ident>,k:NodeKind, s:codemap::Span,nd:AstNode_) {
	// grr. we would really like to traverse children and gather array of ptrs to chilren as a child pointer, 
	// instead we're 'push_back' reallocing all over the place, yuk.

    spt.insert(node_id,
		FNodeInfo{
//			id: node_id,
//			ident:nd.rf_get_ident(),
			kind:k,
			span:s,node:nd,
			parent_id:parent,
			children:Vec::new()
		}
	);
	rf_push_parent_child(spt, parent,node_id);
}

pub fn push_spanned<T:AstNodeAccessors>(spt:&mut FNodeInfoMap,k:NodeKind,s:&codemap::Spanned<T>,ast_node:AstNode_,parent:ast::NodeId) {
    match s.node.rf_get_id() {
        Some(node_id)=>{
			spt.insert(node_id,
				FNodeInfo{
//					id:node_id, 
//					ident:ast_node.rf_get_ident(),
					kind:k,
					span:s.span,
					node:ast_node,
					parent_id:parent,
					children:Vec::new()
				}
			);
			rf_push_parent_child(spt, parent,node_id);
		},
        None=>{}
    }
}

impl Visitor<ast::NodeId> for FNodeInfoMapBuilder {
    // use default impl
//   fn visit_view_item(&mut self, a:&ast::ViewItem, p: ast::NodeId) {
//       walk_view_item(self, a, s);
//   }

    fn visit_generics(&mut self, g:&ast::Generics, p: ast::NodeId) {
        for typ in g.ty_params.iter() {
            // unfortunately no span for type param
//           push_span(&mut self.node_spans, g.def_id.id, Some(g.ident), "ty_param_def", g.def_id.span, astnode_ty_param_def(tp))
            for type_bound in typ.bounds.iter() {
                match type_bound {
                    &ast::TraitTyParamBound(ref tr) => {
                        self.trait_ref(tr, p);
                    }
                    _ => {}
                }
            }
        }

        visit::walk_generics(self, g, p);
    }

    fn visit_item(&mut self, a:&ast::Item, p: ast::NodeId) {
        push_span(&mut self.all_nodes,a.id,p,item_get_ident(a),a.get_kind(),a.span,astnode_item(mkAstSPtrClone(a)));

        // TODO: Push nodes for type-params... since we want to click on their defs...
        match a.node {
            ast::ItemImpl(_, ref o_traitref, _, ref methods) => {
//               self.visit_generics(g, p);
                match *o_traitref {
                    None => {}
                    Some(ref tr) => self.trait_ref(tr, p),
                }

                for m in methods.iter() {
                    push_span(&mut self.all_nodes, m.id, p, Some(a.ident), fa::NK_Method, m.span, fa::astnode_method(*m));
                }
            }
            ast::ItemEnum(ref ed, _) => {
                for v in ed.variants.iter() {
                    self.variant(*v, p);
                }
            }
            ast::ItemTrait(_, ref tr, _) => {
                for t in tr.iter() {
                    self.trait_ref(t, p);
                }
            }
            _ => {}
        }

        visit::walk_item(self, a, a.id);
    }

    fn visit_local(&mut self, a:&ast::Local, p: ast::NodeId) {
        push_span(&mut self.all_nodes, a.id, p, None, fa::NK_Local, a.span, fa::astnode_local(@*a.clone())); // How to remove this?!

        visit::walk_local(self, a, a.id);
    }

    fn visit_block(&mut self, a: &ast::Block, p: ast::NodeId) {
        push_span(&mut self.all_nodes, a.id, p, None, fa::NK_Block, a.span, fa::astnode_none);

        visit::walk_block(self, a, a.id);
    }

    fn visit_stmt(&mut self, a:&ast::Stmt, p: ast::NodeId) {
        push_spanned(&mut self.all_nodes, fa::NK_Stmt, a, fa::astnode_stmt(mkAstSPtrClone(a)), p);

        visit::walk_stmt(self, a, p);
    }

    // we do nothing yet, use default impl
//   fn visit_arm(&mut self, a:&ast::Arm, p: ast::NodeId) {}

    fn visit_pat(&mut self, a: &ast::Pat, p: ast::NodeId) {
        push_span(&mut self.all_nodes, a.id, p, None, fa::NK_Pat, a.span, fa::astnode_pat(mkAstSPtrClone(a)));

        visit::walk_pat(self, a, a.id);
    }

    fn visit_decl(&mut self, a:&ast::Decl, p: ast::NodeId) {
		// TODO - check if 'decl' should be emitted at all - shouldn't we drill down and get a specific decl .. (struct,fn..)
        push_spanned(&mut self.all_nodes, fa::NK_Decl, a, fa::astnode_decl(@*a), p);

        visit::walk_decl(self, a, p);
    }
    // we do nothing, use default for now
//   fn visit_struct_def(&mut self, s)

    fn visit_expr(&mut self, a:&ast::Expr, p: ast::NodeId) {
        push_span(&mut self.all_nodes, a.id, p, expr_get_ident(a), a.get_kind(), a.span, fa::astnode_expr(mkAstSPtrClone(a)));

        visit::walk_expr(self, a, a.id);
    }

    // default, we do nothing
//   fn visit_expr_post()

    fn visit_ty(&mut self, a:&ast::Ty, p: ast::NodeId) {
        push_span(&mut self.all_nodes, a.id, p, None, fa::NK_Type, a.span, fa::astnode_ty(mkAstSPtrClone(a)));

        visit::walk_ty(self, a, a.id);
    }

    // default, we do nothing
//   fn visit_fn()

    fn visit_struct_field(&mut self, a: &ast::StructField, p: ast::NodeId) {
        push_spanned(&mut self.all_nodes, fa::NK_StructField, a, fa::astnode_struct_field(mkAstSPtrClone(a)), p);

        visit::walk_struct_field(self, a, p);
    }

    fn visit_ty_method(&mut self, a:&ast::TypeMethod, p: ast::NodeId) {
        push_span(&mut self.all_nodes, a.id, p, Some(a.ident), fa::NK_TyMethod, a.span, fa::astnode_ty_method(mkAstSPtrClone(a)));

        visit::walk_ty_method(self, a, a.id);
    }
}

fn item_get_ident(a:&ast::Item)->Option<ast::Ident> { Some(a.ident) }
fn expr_get_ident(_ :&ast::Expr)->Option<ast::Ident> {
    None
}

#[deriving(Clone)]
pub struct FindAstNodeSt {
    pub result: NodeTreeLoc,        // todo - full tree path, all the parent nodes.
    pub location: u32,
    pub stop: bool,
//  node_spans: HashMap<ast::node_id,codemap::span>
}

pub struct Finder {
    pub env: FindAstNodeSt
}

impl Finder {
    pub fn new (location: u32) -> Finder {
        let env = FindAstNodeSt{
            result:Vec::from_elem(1,fa::astnode_root), location:location, stop:false

        };
        Finder {
            env: env
        }
    }
}


pub fn span_contains(x: u32, s: codemap::Span)->bool {
    BytePos(x)>=s.lo && BytePos(x)<s.hi
}

impl Visitor<()> for Finder {
    fn visit_view_item(&mut self, a:&ast::ViewItem, _: ()) {
        if span_contains(self.env.location, a.span) {
            self.env.result.push(fa::astnode_view_item(mkAstSPtrClone(a)));;
        }
        visit::walk_view_item(self, a, ());
    }

    fn visit_item(&mut self, a:&ast::Item, _: ()) {
        if span_contains(self.env.location, a.span) {
            self.env.result.push(fa::astnode_item(mkAstSPtrClone(a)));
        }
        visit::walk_item(self, a, ());
    }

    fn visit_local(&mut self, a:&ast::Local, _: ()) {
        if span_contains(self.env.location, a.span) {
            self.env.result.push(fa::astnode_local(@*a.clone()));
        }

        visit::walk_local(self, a, ());
    }

    fn visit_block(&mut self, a:&ast::Block, _: ()) {
        if span_contains(self.env.location, a.span) {
            self.env.result.push(fa::astnode_block(mkAstSPtrClone(a)));
        }

        visit::walk_block(self, a, ());
    }

    fn visit_stmt(&mut self, a:&ast::Stmt, _: ()) {
        if span_contains(self.env.location, a.span) {
            self.env.result.push(fa::astnode_stmt(mkAstSPtrClone(a)));
        }

        visit::walk_stmt(self, a, ());
    }

    fn visit_arm(&mut self, a:&ast::Arm, _: ()) {
           // The whole arm doesn't have a span.
//       if span.contains(self.env.location, a.span) {
//           self.env.result.push(astnode_pat(a));
//       }
        visit::walk_arm(self, a, ());
    }

    fn visit_pat(&mut self, a: &ast::Pat, _: ()) {
        if span_contains(self.env.location, a.span) {
            self.env.result.push(fa::astnode_pat(mkAstSPtrClone(a)));
        }

        visit::walk_pat(self, a, ());
    }

    fn visit_decl(&mut self, a:&ast::Decl, _: ()) {
        if span_contains(self.env.location, a.span) {
            self.env.result.push(fa::astnode_decl(@*a));
        }

        visit::walk_decl(self, a, ());
    }

    fn visit_expr(&mut self, a:&ast::Expr, _: ()) {
        if span_contains(self.env.location, a.span) {
            self.env.result.push(fa::astnode_expr(mkAstSPtrClone(a)));
        }

        visit::walk_expr(self, a, ());
    }

    fn visit_ty(&mut self, a:&ast::Ty, _: ()) {
        if span_contains(self.env.location, a.span) {
            self.env.result.push(fa::astnode_ty(mkAstSPtrClone(a)));
        }

        visit::walk_ty(self, a, ());
    }

    // use default impl for now as we don't do anything here
//   fn visit_fn(fk:&vist::fn_kind, fd:&as::fn_decl, body:&ast::Block,
//       sp:codemap::Span, nid:ast::NodeId, s:  FindAstNodeSt) {}

    fn visit_struct_field(&mut self, a: &ast::StructField, _: ()) {
        if span_contains(self.env.location, a.span) {
            self.env.result.push(fa::astnode_struct_field(mkAstSPtrClone(a)));
        }

        visit::walk_struct_field(self, a, ());
    }
}
