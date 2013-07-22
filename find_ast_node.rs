#[macro_escape]

use syntax::ast::*;
use syntax::visit::*;
use syntax::codemap::*;
use macros::*;


macro_rules! dump{ ($($a:expr),*)=>
	(	{	let mut txt=file!()+~":"+line!().to_str()+~": " ; 
			$( txt=txt.append(
				fmt!("%s=%?",stringify!($a),$a)+~",") 
			);*; 
			println(txt); 
		}
	)
}

/// main
pub fn find(c:@crate,_location:uint)->~[AstNode] {
	let mut s= @mut State{
		result:~[], location:_location, stop:false
	};
	let vt=mk_vt(@Visitor{
		visit_item:f_item,
		visit_struct_field:f_struct_field,
		visit_expr:f_expr,
		visit_pat:f_pat,
		visit_decl:f_decl,
		//visit_ty:f_ty,

		.. *default_visitor::<@mut State>()
		}
	);
	visit_crate(c, (s,vt));
	s.result.clone()
}


#[deriving(Clone)]
pub enum AstNode
{
	// struct Visitor<E>.visit_mod: @fn(&_mod, span, node_id, (E, vt<E>)),
	// struct Visitor<E>.visit_view_item: @fn(&view_item, (E, vt<E>)),
	astnode_view_item(@view_item),
	// struct Visitor<E>.visit_foreign_item: @fn(@foreign_item, (E, vt<E>)),
	// struct Visitor<E>.visit_item: @fn(@item, (E, vt<E>)),
	astnode_item(@item),
	// struct Visitor<E>.visit_local: @fn(@local, (E, vt<E>)),
	// struct Visitor<E>.visit_block: @fn(&blk, (E, vt<E>)),
	// struct Visitor<E>.visit_stmt: @fn(@stmt, (E, vt<E>)),
	// struct Visitor<E>.visit_arm: @fn(&arm, (E, vt<E>)),
	// struct Visitor<E>.visit_pat: @fn(@pat, (E, vt<E>)),
	astnode_pat(@pat),
	// struct Visitor<E>.visit_decl: @fn(@decl, (E, vt<E>)),
	astnode_decl(@decl),
	// struct Visitor<E>.visit_expr: @fn(@expr, (E, vt<E>)),
	astnode_expr(@expr),
	// struct Visitor<E>.visit_expr_post: @fn(@expr, (E, vt<E>)),
	astnode_expr_post(@expr),
	// struct Visitor<E>.visit_ty: @fn(&Ty, (E, vt<E>)),
	astnode_ty(@Ty),
	// struct Visitor<E>.visit_generics: @fn(&Generics, (E, vt<E>)),
	// struct Visitor<E>.visit_fn: @fn(&fn_kind, &fn_decl, &blk, span, node_id, (E, vt<E>)),
	// struct Visitor<E>.visit_ty_method: @fn(&ty_method, (E, vt<E>)),
	astnode_ty_method(@ty_method),
	// struct Visitor<E>.visit_trait_method: @fn(&trait_method, (E, vt<E>)),
	astnode_trait_method(@trait_method),
	// struct Visitor<E>.visit_struct_def: @fn(@struct_def, ident, &Generics, node_id, (E, vt<E>)),
	astnode_struct_def(@struct_def),
	// struct Visitor<E>.visit_struct_field: @fn(@struct_field, (E, vt<E>)),
	astnode_struct_field(@struct_field),

	astnode_empty	
}

struct State {
	result: ~[AstNode],		// todo - full tree path, all the parent nodes.
	location: uint,
	stop: bool
}
fn span_contains(l:uint,s:span)->bool {
	let BytePos(lo)=s.lo;
	let BytePos(hi)=s.hi;
	l>=lo && l<hi
}

// struct Visitor<E>.visit_mod: @fn(&_mod, span, node_id, (E, vt<E>)),
// struct Visitor<E>.visit_view_item: @fn(&view_item, (E, vt<E>)),
// struct Visitor<E>.visit_foreign_item: @fn(@foreign_item, (E, vt<E>)),
// struct Visitor<E>.visit_item: @fn(@item, (E, vt<E>)),
fn f_item(a:@item, (s,v):(@mut State, vt<@mut State>)) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_item(a));
		let found=a.span;
		dump!(found)
	}
	visit_item(a,(s,v))
}
// struct Visitor<E>.visit_local: @fn(@local, (E, vt<E>)),
// struct Visitor<E>.visit_block: @fn(&blk, (E, vt<E>)),
// struct Visitor<E>.visit_stmt: @fn(@stmt, (E, vt<E>)),
// struct Visitor<E>.visit_arm: @fn(&arm, (E, vt<E>)),
// struct Visitor<E>.visit_pat: @fn(@pat, (E, vt<E>)),
fn f_pat(a:@pat, (s,v):(@mut State, vt<@mut State>)) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_pat(a));
		let found=a.span;
		dump!(found)
	}
	visit_pat(a,(s,v))
}


// struct Visitor<E>.visit_decl: @fn(@decl, (E, vt<E>)),
fn f_decl(a:@decl, (s,v):(@mut State, vt<@mut State>)) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_decl(a));
		let found=a.span;
		dump!(found)
	}
	visit_decl(a,(s,v))
}


// struct Visitor<E>.visit_expr: @fn(@expr, (E, vt<E>)),
fn f_expr(a:@expr, (s,v):(@mut State, vt<@mut State>)) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_expr(a));
		let found=a.span;
		dump!(found)
	}
	visit_expr(a,(s,v))
}

// struct Visitor<E>.visit_expr_post: @fn(@expr, (E, vt<E>)),
fn f_expr_post(a:@expr, (s,v):(@mut State, vt<@mut State>)) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_expr_post(a));
		let found=a.span;
		dump!(found)
	}
	visit_expr(a,(s,v))
}

// struct Visitor<E>.visit_ty: @fn(&Ty, (E, vt<E>)),
fn f_ty(a:@Ty, (s,v):(@mut State, vt<@mut State>)) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_ty(a));
		let found=a.span;
		dump!(found)
	}
	visit_ty(a,(s,v))
}

// struct Visitor<E>.visit_generics: @fn(&Generics, (E, vt<E>)),
// struct Visitor<E>.visit_fn: @fn(&fn_kind, &fn_decl, &blk, span, node_id, (E, vt<E>)),

// struct Visitor<E>.visit_ty_method: @fn(&ty_method, (E, vt<E>)),
// struct Visitor<E>.visit_trait_method: @fn(&trait_method, (E, vt<E>)),
// struct Visitor<E>.visit_struct_def: @fn(@struct_def, ident, &Generics, node_id, (E, vt<E>)),
// struct Visitor<E>.visit_struct_field: @fn(@struct_field, (E, vt<E>)),
fn f_struct_field(a:@struct_field, (s,v):(@mut State, vt<@mut State>)) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_struct_field(a));
		let found=a.span;
		dump!(found)
	}
	visit_struct_field(a,(s,v))
}
