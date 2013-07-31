
extern mod syntax;
extern mod rustc;
extern mod extra;

use rustc::{front, metadata, driver, middle};
use rustc::middle::*;

use syntax::parse;
use syntax::ast;
use syntax::ast_map;
use syntax::visit;
use syntax::visit::*;
use syntax::visit::{Visitor, fn_kind};
use find_ast_node::*;
use text_formatting::*;
use syntax::diagnostic;
use syntax::codemap::BytePos;
use std::io;

use syntax::abi::AbiSet;
use syntax::ast;
use syntax::codemap;

use std::hashmap::*;
use std::os;
use std::local_data;
use extra::json::ToJson;

mod text_formatting;
mod find_ast_node;
mod ioutil;


pub static ctxtkey: local_data::Key<@DocContext> = &local_data::Key;

pub macro_rules! if_some {
	($b:ident in $a:expr then $c:expr)=>(
		match $a {
			Some($b)=>$c,
			None=>{}
		}
	);
}
pub macro_rules! tlogi{ 
	($($a:expr),*)=>(println((file!()+":"+line!().to_str()+": " $(+$a.to_str())*) ))
}
pub macro_rules! logi{ 
	($($a:expr),*)=>(println(""$(+$a.to_str())*) )
}
//macro_rules! dump{ ($a:expr)=>(logi!(fmt!("%s=%?",stringify!($a),$a).indent(2,160));)}
macro_rules! dump{ ($($a:expr),*)=>
	(	{	let mut txt=~""; 
			$( { txt=txt.append(
				 fmt!("%s=%?",stringify!($a),$a)+",") 
				}
			);*; 
			logi!(txt); 
		}
	)
}

pub macro_rules! if_some {
	($b:ident in $a:expr then $c:expr)=>(
		match $a {
			Some($b)=>$c,
			None=>{}
		}
	);
}

/// tags: crate,ast,parse resolve
/// Parses, resolves the given crate
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
                                                     driver::driver::cu_no_trans, None);
                                                     
	let c=crate.unwrap();
	let t=tycx.unwrap();
    DocContext { crate: c, tycx: t, sess: sess }
}

fn main() {
    use extra::getopts::*;
    use std::hashmap::HashMap;

    let args = os::args();

    let opts = ~[
        optmulti("L"),optflag("d"),optflag("j"),optflag("h"),optflag("i")
    ];

    let matches = getopts(args.tail(), opts).get();
    let libs1 = opt_strs(&matches, "L").map(|s| Path(*s));
	let libs=if libs1.len()>0 {libs1} else {
		match (os::getenv(&"RUST_LIBS")) {
			Some(x)=>~[Path(x)],
			None=>~[]
		}		
	};

	if opt_present(&matches,"h") {
		println("rustfind: useage:-");
		println(" filename.rs [-L<library path>]  : dump JSON map of the ast nodes & defintions");
		println(" filename.rs:line:col : TODO return definition reference of symbol at given position");
		println(" -i filename.rs [-L<lib path>] : interactive mode");
		println(" -d filename.rs [-L<lib path>] : debug for this tool");
		println(" set RUST_LIBS for a default library search path");

	};
	if matches.free.len()>0 {
		let filename=&matches.free[0];
		let dc = @get_ast_and_resolve(&Path(*filename), libs);
		local_data::set(ctxtkey, dc);
	
		if (opt_present(&matches,"d")) {
			debug_test(dc);
		} else if (opt_present(&matches,"j")){
			dump_json(dc);
		} else {	// default, dump json map of nodes,defs,spans
			dump_json(dc);
		}
		if opt_present(&matches,"i") {
			rustfind_interactive(dc)
		}
	}

}

fn option_to_str<T:ToStr>(opt:&Option<T>)->~str {
	match *opt { Some(ref s)=>~"("+s.to_str()+~")",None=>~"(None)" }
}

trait MyToStr {  fn my_to_str(&self)->~str; }
impl MyToStr for codemap::span {
	fn my_to_str(&self)->~str { ~"("+self.lo.to_str()+~".."+self.hi.to_str() }
}

/// Todo , couldn't quite see how to declare this as a generic method of Option<T>
pub fn some<T>(o:&Option<T>,f:&fn(t:&T)) {
	match *o {
		Some(ref x)=>f(x),
		None=>{}
	}
}
pub fn some_else<T,X,Y>(o:&Option<T>,f:&fn(t:&T)->Y,default_value:Y)->Y {
	match *o {
		Some(ref x)=>f(x),
		None=>default_value
	}
}

fn dump_json(dc:&DocContext) {
	// TODO: full/partial options - we currently wwrite out all the nodes we find.
	// need option to only write out nodes that map to definitons. 
	println("{");
	println("\tnode_spans:");
	let node_spans=build_node_spans_table(dc.crate);
	println(node_spans_table_to_json(node_spans));	
	println(",");
	println("\tnode_defs:");
	let node_def_node = build_node_def_node_table(dc);
	println(node_def_node_table_to_json(node_def_node));
	println("}");

}

fn debug_test(dc:&DocContext) {

	// TODO: parse commandline source locations,convert to codemap locations
	//dump!(ctxt.tycx);
	logi!("==== Get table of node-spans...===")
	let node_spans=build_node_spans_table(dc.crate);
	println(node_spans_table_to_json(node_spans));

	logi!("==== Node Definition mappings...===")
	let node_def_node = build_node_def_node_table(dc);
	println(node_def_node_table_to_json(node_def_node));


	logi!("==== dump def table.===");
	dump_ctxt_def_map(dc);

	logi!("==== Test node search by location...===");

	// Step a test 'cursor' src_pos through the given source file..
	let mut test_cursor=15 as uint;
	while test_cursor<350 {
		let loc = get_source_loc(dc,BytePos(test_cursor));

		logi!(~"\n=====Find AST node at: ",loc.file.name,":",loc.line,":",loc.col,":"," =========");

		let node = find_ast_node::find(dc.crate,BytePos(test_cursor));
		let node_info =  find_ast_node::get_node_info_str(dc,node);
		dump!(node_info);
		println("node ast loc:"+(do node.map |x| { option_to_str(&x.get_id()) }).to_str());
		if_some!(id in node.last().ty_node_id() then {
			logi!("source=",get_node_source(dc.tycx, node_spans,id));
			if_some!(t in find_ast_node::safe_node_id_to_type(dc.tycx, id) then {
				println(fmt!("typeinfo: %?",
					{let ntt= rustc::middle::ty::get(t); ntt}));
				dump!(id,dc.tycx.def_map.find(&id));
				});
			let (def_id,opt_info)= def_info_from_node_id(dc,node_spans,id); 
			if_some!(info in opt_info then{
				logi!("src node=",id," def node=",def_id,
					" span=",info.span.my_to_str());
				logi!("def source=", get_node_source(dc.tycx, node_spans, def_id));
			})
		})

		test_cursor+=11;
	}
}

fn get_source_loc(dc:&DocContext, pos:codemap::BytePos)->codemap::Loc {
	dc.tycx.sess.codemap.lookup_char_pos(pos)
}

pub fn dump_node_source_for_single_file_only(text:&[u8], ns:&NodeSpans, id:ast::node_id) {
	match(ns.find(&id)) {None=>logi!("()"),
		Some(info)=>{
			dump_span(text, &info.span);
		}
	}
}

// TODO- this should return a slice
pub fn get_node_source(c:ty::ctxt, ns:&NodeSpans, id:ast::node_id)->~str {
	match (ns.find(&id)){None=>~"",
		Some(info)=>{
			let loc_lo=c.sess.codemap.lookup_char_pos(info.span.lo);
			let loc_hi=c.sess.codemap.lookup_char_pos(info.span.hi);
			// TODO-assert both in same file!
			let file_org=*loc_lo.file.start_pos;
			let slice=loc_lo.file.src.slice(*info.span.lo-file_org, *info.span.hi-file_org );
//			~"source yada"
			slice.to_str()
		}
	}
}


pub fn dump_span(text:&[u8], sp:&codemap::span) {

	let line_col=text_offset_to_line_pos(text, *sp.lo);
	logi!(" line,ofs=",option_to_str(&line_col)," text=\"",
		std::str::from_bytes(text_span(text,sp)),"\"");
}


pub fn def_info_from_node_id<'a,'b>(dc:&'a DocContext, node_spans:&'b NodeSpans, id:ast::node_id)->(int,Option<&'b NodeInfo>) {
	let crate_num=0;
	match dc.tycx.def_map.find(&id) { // finds a def..
		Some(a)=>{
			match get_def_id(crate_num,*a){
				Some(b)=>(b.node,node_spans.find(&b.node)),
				None=>(id as int,None)
			}
		},
		None=>(id as int,None)
	}
	
}

// see: tycx.node_types:node_type_table:HashMap<id,t>
// 't'=opaque ptr, ty::get(:t)->t_box_ to resolve it

pub fn dump_ctxt_def_map(dc:&DocContext) {
//	let a:()=ctxt.tycx.node_types
	logi!("===Test ctxt def-map table..===");
	for dc.tycx.def_map.iter().advance |(key,value)|{
		dump!(key,value);
	}
}

pub fn text_line_pos_to_offset(text:&[u8], (line,ofs_in_line):(uint,uint))->Option<uint> {
	// line as reported by grep & text editors,counted from '1' not '0'
	let mut pos = 0;
	let tlen=text.len();	
	let	mut tline=0;
	let mut line_start_pos=0;
	while pos<tlen{
		match text[pos] as char{
			'\n' => {tline+=1; line_start_pos=pos;},
//			"\a" => {tpos=0;line_pos=pos;},
			_ => {}
		}
		// todo - clamp line end
		if tline==(line-1){ 
			return Some(line_start_pos + ofs_in_line);
		}
		pos+=1;
	}
	return None;
}

pub fn text_offset_to_line_pos(text:&[u8], src_ofs:uint)->Option<(uint,uint)> {
	// line as reported by grep & text editors,counted from '1' not '0'
	let mut pos = 0;
	let tlen=text.len();	
	let	mut tline=0;
	let mut line_start_pos=0;
	while pos<tlen{
		match text[pos] as char{
			'\n' => {
				if src_ofs<=pos && src_ofs>line_start_pos {
					return Some((tline+1,src_ofs-line_start_pos));
				}
				tline+=1; line_start_pos=pos;
			},
//			"\a" => {tpos=0;line_pos=pos;},
			_ => {}
		}
		// todo - clamp line end
		pos+=1;
	}
	return None;
}

pub fn text_span<'a,'b>(text:&'a [u8],s:&'b codemap::span)->&'a[u8] {
	text.slice(*s.lo,*s.hi)
}

pub fn build_node_def_node_table(dc:&DocContext)->~HashMap<ast::node_id, ast::def_id>
{
	let mut r=~HashMap::new();
	let curr_crate_id_hack=0;	// TODO WHAT IS CRATE ID REALLY?!
	// todo .. for range(0,c.next_id) || ??
	let mut id:ast::node_id=0;
	while id<*(dc.tycx.next_id) as ast::node_id {
		if_some!(t in safe_node_id_to_type(dc.tycx,id as int) then {
			if_some!(def in dc.tycx.def_map.find(&(id as int)) then { // finds a def..
				if_some!(did in get_def_id(curr_crate_id_hack,*def) then {
					r.insert(id as ast::node_id,did);
				})
			});
		});
		id+=1;
	}
	r
}

pub fn def_node_id_from_node_id(dc:&DocContext, id:ast::node_id)->ast::node_id {
	let crate_num=0;	// TODO - whats crate Id really???
	match dc.tycx.def_map.find(&id) { // finds a def..
		Some(a)=>{
			match get_def_id(crate_num,*a) {
				Some(b)=>b.node,
				None=>id as int
			}
		},
		None=>(id as int)	// no definition? say its its own definition
	}
}

pub fn rustfind_interactive(dc:&DocContext) {
	// TODO - check if RUSTI can already do this.. it would be better there IMO
	let node_spans=build_node_spans_table(dc.crate);
	println(node_spans_table_to_json(node_spans));

	logi!("==== Node Definition mappings...===");
	let node_def_node = build_node_def_node_table(dc);
	println(node_def_node_table_to_json(node_def_node));

	loop {
		print("rustfind>");
		let input_line=io::stdin().read_line();
		let toks:~[&str]=input_line.split_iter(' ').collect();
		if toks.len()>0 {
			match toks[0] {
				"h"=> println("interactive mode - enter symbol name o\n q-quit\n"),
				"q"=> break,
				_ =>{
					println(def_of_symbol_to_str(dc,node_spans,node_def_node,toks[0]));
				}
			}
		}
	}
}

pub fn def_of_symbol_to_str(dc:&DocContext, ns:&NodeSpans,ds:&HashMap<ast::node_id, ast::def_id>,s:&str)->~str {
	~"TODO"	
}







