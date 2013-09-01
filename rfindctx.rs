use std::num;
use std::num::*;
use std::local_data;

use syntax::parse;
use syntax::ast;
use syntax::ast_map;
use syntax::visit;
use syntax::oldvisit;
use syntax::oldvisit::*;
use syntax::oldvisit::{Visitor, fn_kind};
use syntax::codemap;
use rustc::{front, metadata, driver, middle};
use rustc::middle::mem_categorization::ast_node;
use rustc::middle::ty;

use std::hashmap;
use codemaput::*;

pub struct RFindCtx {
     crate: @ast::Crate,
     tycx: middle::ty::ctxt,
     sess: driver::session::Session,
     ca: driver::driver::CrateAnalysis
}

pub static ctxtkey: local_data::Key<@RFindCtx> = &local_data::Key;

pub fn first_file_name(dc:&RFindCtx)->~str {
	dc.tycx.sess.codemap.files[0].name.to_str() // clone?
}

pub fn find_file_name_in(dc:&RFindCtx,fname:&str)->Option<~str> {
	// todo subsequence match..
	// TODO - is there an existing way of doing this, "index_of.." ..contains()..?
	for f in dc.tycx.sess.codemap.files.iter() {
		if fname==f.name {return Some(fname.to_owned());}
	}
	None
}


