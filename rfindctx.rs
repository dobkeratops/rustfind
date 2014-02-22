use std::local_data;

use syntax::ast;
use syntax::codemap;
use syntax::parse::token;
use rustc::{driver, middle};

pub struct RFindCtx {
     crate_: @ast::Crate,
     tycx: middle::ty::ctxt,
     sess: driver::session::Session,
     ca: driver::driver::CrateAnalysis
}

pub static ctxtkey: local_data::Key<@RFindCtx> = &local_data::Key;

pub fn first_file_name(dc:&RFindCtx)->~str {
    let files = dc.tycx.sess.codemap.files.borrow();
    let files = files.get();
	files[0].name.to_str() // clone?
}

pub fn find_file_name_in(dc:&RFindCtx,fname:&str)->Option<~str> {
	// todo subsequence match..
	// TODO - is there an existing way of doing this, "index_of.." ..contains()..?
    let files = dc.tycx.sess.codemap.files.borrow();
    let files = files.get();
	for f in files.iter() {
		if fname==f.name {return Some(fname.to_owned());}
	}
	None
}


pub fn get_source_loc(dc:&RFindCtx, pos:codemap::BytePos)->codemap::Loc {
	dc.tycx.sess.codemap.lookup_char_pos(pos)
}


pub fn str_of_opt_ident(dc:&RFindCtx, ident:Option<ast::Ident>)->~str{
	match ident {
		Some(i)=>token::get_ident(i).get().to_owned(), None=>~""
	}
}

