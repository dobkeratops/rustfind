
use syntax::ast::*;
use syntax::visit::*;
use syntax::codemap::*;

macro_rules! dump{ ($($a:expr),*)=>
	(	{	let mut txt=file!()+":"+line!().to_str()+": " ; 
			$( txt=txt.append(
				fmt!("%s=%?",stringify!($a),$a)+",") 
			);*; 
			println(txt); 
		}
	)
}

/// main
pub fn find(c:@crate,_location:uint)->~[AstNode] {
	let mut s= @mut State{
		result:~[astnode_root], location:_location, stop:false
	};
	let vt=mk_vt(@Visitor{
		visit_view_item:f_view_item,
		visit_item:f_item,
		visit_local:f_local,
		visit_block:f_block,
		visit_stmt:f_stmt,
		visit_arm:f_arm,
		visit_struct_field:f_struct_field,
		visit_pat:f_pat,
		visit_decl:f_decl,
		visit_ty:f_ty,
		visit_expr:f_expr,
//		visit_struct_def:f_struct_def,
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
	astnode_mod(@_mod),
	// struct Visitor<E>.visit_view_item: @fn(&view_item, (E, vt<E>)),
	astnode_view_item(@view_item),
	// struct Visitor<E>.visit_foreign_item: @fn(@foreign_item, (E, vt<E>)),
	// struct Visitor<E>.visit_item: @fn(@item, (E, vt<E>)),
	astnode_item(@item),
	// struct Visitor<E>.visit_local: @fn(@local, (E, vt<E>)),
	astnode_local(@local),
	//struct Visitor<E>.visit_block: @fn(&blk, (E, vt<E>)),
	astnode_block(@blk),
	// struct Visitor<E>.visit_stmt: @fn(@stmt, (E, vt<E>)),
	astnode_stmt(@stmt),
	// struct Visitor<E>.visit_arm: @fn(&arm, (E, vt<E>)),
	astnode_arm(@arm),
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
	//asstnode_fn(@fn_kind,@fn_decl,@
	// struct Visitor<E>.visit_ty_method: @fn(&ty_method, (E, vt<E>)),
	astnode_ty_method(@ty_method),
	// struct Visitor<E>.visit_trait_method: @fn(&trait_method, (E, vt<E>)),
	astnode_trait_method(@trait_method),
	// struct Visitor<E>.visit_struct_def: @fn(@struct_def, ident, &Generics, node_id, (E, vt<E>)),
	astnode_struct_def(@struct_def),
	// struct Visitor<E>.visit_struct_field: @fn(@struct_field, (E, vt<E>)),
	astnode_struct_field(@struct_field),

	astnode_root	
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
 //fn f_mod(a:&_mod, (s,v):(@mut State, vt<@mut State>)) {
	//if span_contains(s.location, a.span) {
	//	s.result.push(astnode_mod(@copy *a));
	//}
	//visit_struct_field(a,(s,v))
   //}
// struct Visitor<E>.visit_view_item: @fn(&view_item, (E, vt<E>)),
fn f_view_item(a:&view_item, (s,v):(@mut State, vt<@mut State>)) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_view_item(@ copy *a));
	}
	visit_view_item(a,(s,v))
}// struct Visitor<E>.visit_foreign_item: @fn(@foreign_item, (E, vt<E>)),
// struct Visitor<E>.visit_item: @fn(@item, (E, vt<E>)),
fn f_item(a:@item, (s,v):(@mut State, vt<@mut State>)) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_item(a));
	}
	visit_item(a,(s,v))
}
// struct Visitor<E>.visit_local: @fn(@local, (E, vt<E>)),
fn f_local(a:@local, (s,v):(@mut State, vt<@mut State>)) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_local(a));
	}
	visit_local(a,(s,v))
}

// struct Visitor<E>.visit_block: @fn(&blk, (E, vt<E>)),
fn f_block(a:&blk, (s,v):(@mut State, vt<@mut State>)) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_block(@ copy *a));
	}
	visit_block(a,(s,v))
}

// struct Visitor<E>.visit_stmt: @fn(@stmt, (E, vt<E>)),
fn f_stmt(a:@stmt, (s,v):(@mut State, vt<@mut State>)) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_stmt(a));
	}
	visit_stmt(a,(s,v))
}

// struct Visitor<E>.visit_arm: @fn(&arm, (E, vt<E>)),
fn f_arm(a:&arm, (s,v):(@mut State, vt<@mut State>)) {
// the whole arm doesn't have a span 
//	if span_contains(s.location, a.span) {
//		s.result.push(astnode_arm(@copy *a));
//	}
	visit_arm(a,(s,v))
}
// struct Visitor<E>.visit_pat: @fn(@pat, (E, vt<E>)),
fn f_pat(a:@pat, (s,v):(@mut State, vt<@mut State>)) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_pat(a));
	}
	visit_pat(a,(s,v))
}


// struct Visitor<E>.visit_decl: @fn(@decl, (E, vt<E>)),
fn f_decl(a:@decl, (s,v):(@mut State, vt<@mut State>)) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_decl(a));
	}
	visit_decl(a,(s,v))
}


// struct Visitor<E>.visit_expr: @fn(@expr, (E, vt<E>)),
fn f_expr(a:@expr, (s,v):(@mut State, vt<@mut State>)) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_expr(a));
	}
	visit_expr(a,(s,v))
}

// struct Visitor<E>.visit_expr_post: @fn(@expr, (E, vt<E>)),
fn f_expr_post(a:@expr, (s,v):(@mut State, vt<@mut State>)) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_expr_post(a));
	}
	visit_expr(a,(s,v))
}

// struct Visitor<E>.visit_ty: @fn(&Ty, (E, vt<E>)),
fn f_ty(a:&Ty, (s,v):(@mut State, vt<@mut State>)) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_ty(@copy *a));
	}
	visit_ty(a,(s,v))
}

// struct Visitor<E>.visit_generics: @fn(&Generics, (E, vt<E>)),
// struct Visitor<E>.visit_fn: @fn(&fn_kind, &fn_decl, &blk, span, node_id, (E, vt<E>)),
fn f_fn(fk:&fn_kind, fd:&fn_decl, body:&blk, sp:span, nid:node_id, (s,v):(@mut State, vt<@mut State>)) {
	//if span_contains(s.location, span) {
		//s.result.push(astnode_fn2(a));
	//}
	visit_fn(fk,fd,body,sp,nid,(s,v))
}
// struct Visitor<E>.visit_ty_method: @fn(&ty_method, (E, vt<E>)),
// struct Visitor<E>.visit_trait_method: @fn(&trait_method, (E, vt<E>)),
// struct Visitor<E>.visit_struct_def: @fn(@struct_def, ident, &Generics, node_id, (E, vt<E>)),
 //fn f_struct_def(a:@struct_def, (s,v):(@mut State, vt<@mut State>)) {
 //	if span_contains(s.location, a.span) {
 //		s.result.push(astnode_struct_def(a));
 //	}
 //	visit_struct_field(a,(s,v))
 //}
// struct Visitor<E>.visit_struct_field: @fn(@struct_field, (E, vt<E>)),
fn f_struct_field(a:@struct_field, (s,v):(@mut State, vt<@mut State>)) {
	if span_contains(s.location, a.span) {
		s.result.push(astnode_struct_field(a));
	}
	visit_struct_field(a,(s,v))
}
