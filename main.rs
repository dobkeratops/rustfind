extern mod syntax;
extern mod rustc;

extern mod extra;

use rustc::{front, metadata, driver, middle};

use syntax::parse;
use syntax::ast;
use syntax::ast_map;

use std::os;
use std::local_data;
use extra::json::ToJson;

pub static ctxtkey: local_data::Key<@DocContext> = &local_data::Key;

struct DocContext {
    crate: @ast::crate,
    tycx: middle::ty::ctxt,
    sess: driver::session::Session
}

/// Parses, resolves, and typechecks the given crate
fn get_ast_and_resolve(cpath: &Path, libs: ~[Path]) -> DocContext {
    let parsesess = parse::new_parse_sess(None);
    let sessopts = @driver::session::options {
        binary: @"rustdoc",
        maybe_sysroot: Some(@std::os::self_exe_path().get().pop()),
        addl_lib_search_paths: @mut libs,
        .. copy (*rustc::driver::session::basic_options())
    };


    let diagnostic_handler = syntax::diagnostic::mk_handler(None);
    let span_diagnostic_handler =
        syntax::diagnostic::mk_span_handler(diagnostic_handler, parsesess.cm);

    let mut sess = driver::driver::build_session_(sessopts, parsesess.cm,
                                                  syntax::diagnostic::emit,
                                                  span_diagnostic_handler);

    let (crate, tycx) = driver::driver::compile_upto(sess, sessopts.cfg.clone(),
                                                     &driver::driver::file_input(cpath.clone()),
                                                     driver::driver::cu_typeck, None);
                                                     
    DocContext { crate: crate.unwrap(), tycx: tycx.unwrap(), sess: sess }
}

fn main() {
    use extra::getopts::*;
    use std::hashmap::HashMap;

	println("Rustfind:-");
    let args = os::args();
    let opts = ~[
        optmulti("L")
    ];
    let matches = getopts(args.tail(), opts).get();
    let libs = opt_strs(&matches, "L").map(|s| Path(*s));

    let ctxt = @get_ast_and_resolve(&Path(matches.free[0]), libs);
    debug!("defmap:");
    for ctxt.tycx.def_map.iter().advance |(k, v)| {
        debug!("%?: %?", k, v);
    }
    local_data::set(ctxtkey, ctxt);
/*
    let mut v = @mut RustdocVisitor::new();
    v.visit(ctxt.crate);

    let mut crate = v.clean();
    println(crate.to_json().to_str());
*/
}
