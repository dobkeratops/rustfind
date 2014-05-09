use std::local_data;

use syntax::ast;
use syntax::codemap;
use syntax::parse::token;
use rustc::{driver, middle};
use rustc::metadata::cstore;

/// context object for the application; holds structs populated by rustc
pub struct RustFindCtx {
     pub crate_: @ast::Crate,
//     tycx: middle::ty::ctxt, // todo: lazy and make an Rc<T>, or propogate the lifetimes needed for &..
//     sess: driver::session::Session,
     pub ca: driver::driver::CrateAnalysis	//todo: tycx is in here!
}
/// accessors for RustFindCtx
impl RustFindCtx {
	pub fn codemap<'a>(&'a self)->&'a codemap::CodeMap { self.ca.ty_cx.sess.codemap() }
	pub fn session<'a>(&'a self)->&'a driver::session::Session { &self.ca.ty_cx.sess }
	pub fn cstore<'a>(&'a self)->&'a cstore::CStore { &self.session().cstore }
	pub fn tycx_ref<'a>(&'a self)->&'a middle::ty::ctxt {
		return	&'a self.ca.ty_cx
	}
	pub fn tycx_val(self)->middle::ty::ctxt {
		return	self.ca.ty_cx
	}
}
// what was this?
//pub static ctxtkey: local_data::Key<@RustFindCtx> = &local_data::Key;

pub fn first_file_name(dc:&RustFindCtx)->StrBuf {
    let files = dc.codemap().files.borrow();
    files.get(0).name.to_strbuf() // clone?
}

pub fn find_file_name_in(dc:&RustFindCtx,fname:&str)->Option<StrBuf> {
    // todo subsequence match..
    // TODO - is there an existing way of doing this, "index_of.." ..contains()..?
    let files = dc.codemap().files.borrow();
    for f in files.iter() {
        if fname==f.name.as_slice() {return Some(fname.to_strbuf());}
    }
    None
}


pub fn get_source_loc(dc:&RustFindCtx, pos:codemap::BytePos)->codemap::Loc {
    dc.codemap().lookup_char_pos(pos)
}


pub fn str_of_opt_ident(id:Option<ast::Ident>)->StrBuf{
    match id {
        Some(i)=>token::get_ident(i).get().to_strbuf(), None=>StrBuf::new()
    }
}

pub fn str_of_ident(id:ast::Ident)->StrBuf{
	token::get_ident(id).get().to_strbuf()
}




