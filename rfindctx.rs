use std::num;
use std::num::*;

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



