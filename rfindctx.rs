use std::local_data;

use syntax;
use syntax::ast;
use syntax::codemap;
use syntax::parse::token;
use rustc::{driver, middle};
use rustc::metadata::cstore;

pub struct RFindCtx {
     crate_: @ast::Crate,
//     tycx: middle::ty::ctxt, // todo: lazy and make an Rc<T>, or propogate the lifetimes needed for &..
//     sess: driver::session::Session,
     ca: driver::driver::CrateAnalysis	//todo: tycx is in here!
}
impl RFindCtx {
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

pub static ctxtkey: local_data::Key<@RFindCtx> = &local_data::Key;

pub fn first_file_name(dc:&RFindCtx)->~str {
    let files = dc.codemap().files.borrow();
    let files = files.get();
    files.get(0).name.to_str() // clone?
}

pub fn find_file_name_in(dc:&RFindCtx,fname:&str)->Option<~str> {
    // todo subsequence match..
    // TODO - is there an existing way of doing this, "index_of.." ..contains()..?
    let files = dc.codemap().files.borrow();
    let files = files.get();
    for f in files.iter() {
        if fname==f.name {return Some(fname.to_owned());}
    }
    None
}


pub fn get_source_loc(dc:&RFindCtx, pos:codemap::BytePos)->codemap::Loc {
    dc.codemap().lookup_char_pos(pos)
}


pub fn str_of_opt_ident(ident:Option<ast::Ident>)->~str{
    match ident {
        Some(i)=>token::get_ident(i).get().to_owned(), None=>~""
    }
}

