
extern mod syntax;
extern mod rustc;
extern mod extra;

use rustc::{front, metadata, driver, middle};
use rustc::middle::*;

use std::num;
use std::num::*;

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
	let quiet=true;
	fn no_emit(cmsp: Option<(@codemap::CodeMap, codemap::span)>, msg: &str, lvl: syntax::diagnostic::level) {
	}

    let diagnostic_handler = syntax::diagnostic::mk_handler(None);
    let span_diagnostic_handler =
        syntax::diagnostic::mk_span_handler(if quiet{no_emit}else{diagnostic_handler}, parsesess.cm);


    let mut sess = driver::driver::build_session_(sessopts, parsesess.cm,
                                                  if quiet{no_emit}else{syntax::diagnostic::emit},
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
        optmulti("L"),optflag("d"),optflag("j"),optflag("h"),optflag("i"),optflag("g")
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
		println(" -j filename.rs [-L<library path>]  : dump JSON map of the ast nodes & defintions");
		println(" cratename.rs anotherfile.rs:line:col:");
		println("    - load cratename.rs; look for definition at anotherfile.rs:line:col");
		println("    - where anotherfile.rs is assumed to be a module of the crate");
		println(" filename.rs:line:col : TODO return definition reference of symbol at given position");
		println(" -i filename.rs [-L<lib path>] : interactive mode");
		println(" -d filename.rs [-L<lib path>] : debug for this tool");
		println(" -g format output as gedit filepos +line filename");
		println(" set RUST_LIBS for a default library search path");
	};
	if matches.free.len()>0 {
		let filename=get_filename_only(matches.free[0]);
		let dc = @get_ast_and_resolve(&Path(filename), libs);
		local_data::set(ctxtkey, dc);
	
		if (opt_present(&matches,"d")) {
			debug_test(dc);
		} else if (opt_present(&matches,"j")){
			dump_json(dc);
		}
		let mut i=0;
		while i<matches.free.len() {
			let mode=if opt_present(&matches,"g"){SDM_GeditCmd} else {SDM_Source};
			print(lookup_def_of_file_line_pos(dc,matches.free[i],mode));
			i+=1;
		}
		if opt_present(&matches,"i") {
			rustfind_interactive(dc)
		}
	}
}

fn get_filename_only(fnm:&str)->~str {
	let toks:~[&str]=fnm.split_iter(':').collect();
	return toks[0].to_str();
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

	

	while test_cursor<500 {
		let loc = get_source_loc(dc,BytePos(test_cursor));

		logi!(~"\n=====Find AST node at: ",loc.file.name,":",loc.line,":",loc.col,":"," =========");

		let node = find_ast_node::find_in_tree(dc.crate,BytePos(test_cursor));
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

		test_cursor+=20;
	}

	// test byte pos from file...
	logi!("====test file:pos source lookup====");
	dump!(get_file_line_col_len_str(dc.tycx,&"test_input.rs",3,0,10));
	dump!(get_file_line_col_len_str(dc.tycx,&"test_input.rs",9,0,10));
	dump!(get_file_line_col_len_str(dc.tycx,&"test_input2.rs",4,0,10));
	dump!(get_file_line_col_len_str(dc.tycx,&"test_input2.rs",11,0,10));
	let ospan=file_line_col_len_to_byte_pos(dc.tycx, &"test_input2.rs", 10,0,32);
	if_some!(x in ospan then {
		let (lo,hi)=x;
		logi!(get_span_str(dc.tycx, &codemap::span{lo:lo,hi:hi,expn_info:None} ));
	});
	dump!(get_file_line(dc.tycx,"test_input2.rs",5));
	dump!(get_file_line(dc.tycx,"test_input.rs",9));
	
	logi!("\n====test full file:pos lookup====");
	dump!(lookup_def_of_file_line_pos(dc, &"test_input.rs:9:20",SDM_Source));
	dump!(lookup_def_of_file_line_pos(dc, &"test_input2.rs:3:12",SDM_Source));
	
}

fn lookup_def_of_file_line_pos(dc:&DocContext,filepos:&str, show_all:ShowDefMode)->~str {

	let toks:~[&str]=filepos.split_iter(':').collect();
	if toks.len()<3 { return ~"" }

//	let line:Option<uint> = FromStr::from_str(toks[1]);
	if_some!(line in FromStr::from_str(toks[1]) then {
		if_some!(col in FromStr::from_str(toks[2]) then {
			//todo - if no column specified, just lookup everything on that line!

			match file_line_col_to_byte_pos(dc.tycx,toks[0],line,col) {
				None=>return ~"error: file not in crate or position out of range",
				Some(bp)=>{
					return lookup_def_of_byte_pos(dc,bp,show_all)
				}
			}
		})
	})
	return ~"";
}

enum ShowDefMode {
	SDM_Line=0,
	SDM_LineCol=1,
	SDM_Source=2,
	SDM_GeditCmd=3
}

fn lookup_def_of_byte_pos(dc:&DocContext, bp:BytePos, m:ShowDefMode)->~str {
	// TODO - cache these outside!!
	let node_spans=build_node_spans_table(dc.crate);
	let node_def_node = build_node_def_node_table(dc);

	let node = find_ast_node::find_node_at_byte_pos(dc.crate,bp);

	if_some!(id in node.ty_node_id() then {

		let (def_id,opt_info)= def_info_from_node_id(dc,node_spans,id); 
		if_some!(info in opt_info then{
			match node_spans.find(&def_id) {
				None=>~"",
				Some(def_info)=>{
					let loc=get_source_loc(dc,def_info.span.lo);
					let def_pos_str=
						loc.file.name + ":"+loc.line.to_str()+":"+
							match m { SDM_LineCol=>loc.col.to_str()+":", _ =>~"" }+"\n";
					return	match m{
						SDM_Source=>def_pos_str+get_node_source(dc.tycx,node_spans, def_id)+"\n",
						SDM_GeditCmd=>"+"+loc.line.to_str()+" "+loc.file.name+" ",
						_ => def_pos_str};
						}
			};		
		})

	})
	return ~"";
}

fn get_file_line_col_len_str(cx:ty::ctxt, filename:&str, line:uint, col:uint,len:uint)->~str {
	let x=file_line_col_len_to_byte_pos(cx, filename, line,col,len);
	match  x  {
		Some((bp_lo,bp_hi))=>get_span_str(cx,
			&codemap::span{lo:bp_lo,hi:bp_hi,expn_info:None}
		),
		None=>~""
	}
}
fn get_file_line(cx:ty::ctxt, filename:&str, src_line:uint)->~str {
//	for c.sess.codemap.files.rev_iter().advance |fm:&codemap::FileMap| {
	let mut i=cx.sess.codemap.files.len();
	while i>0 {	// caution, need loop because we return, wait for new foreach ..in..
		i-=1;
		let fm=&cx.sess.codemap.files[i];
		let filemap_filename:&str=fm.name;	
		if filename==filemap_filename {
			let s=*fm.lines[src_line-1];
			let e=if src_line>=fm.lines.len() {
				*fm.start_pos+fm.src.len()
			} else {
				*fm.lines[src_line]
			};
			return get_span_str(cx, &codemap::span{lo:BytePos(s),hi: BytePos(e), expn_info:None} )
		}
	}
	return ~"";
	
}

fn get_source_loc(dc:&DocContext, pos:codemap::BytePos)->codemap::Loc {
	dc.tycx.sess.codemap.lookup_char_pos(pos)
}
fn loc_to_str(loc:codemap::Loc)->~str {
	loc.file.name+":"+loc.line.to_str()+":"+loc.col.to_str()+":"
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
	match (ns.find(&id)){
		None=>~"",
		Some(info)=>{
			get_span_str(c,&info.span)
		}
	}
}


pub fn get_span_str(c:ty::ctxt, sp:&codemap::span)->~str {
	let loc_lo=c.sess.codemap.lookup_char_pos(sp.lo);
	let loc_hi=c.sess.codemap.lookup_char_pos(sp.hi);
	// TODO-assert both in same file!
	let file_org=*loc_lo.file.start_pos;
	let slice=loc_lo.file.src.slice(*sp.lo-file_org, *sp.hi-file_org );
	slice.to_str()
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

/// Get from text editor's description of location to inlined-crate byte-offset
pub fn file_line_col_len_to_byte_pos(c:ty::ctxt,src_filename:&str,src_line:uint ,src_col:uint,len:uint )->Option<(codemap::BytePos,codemap::BytePos)>

{
//	for c.sess.codemap.files.rev_iter().advance |fm:&codemap::FileMap| {
	let mut i=c.sess.codemap.files.len();
	while i>0 {	// caution, need loop because we return, wait for new foreach ..in..
		i-=1;
		let fm=&c.sess.codemap.files[i];
		let filemap_filename:&str=fm.name;	
		if src_filename==filemap_filename {
			let line_pos=*fm.lines[src_line-1];
			let bp_start=*fm.lines[src_line-1]+src_col;
			let bp_end=(bp_start+len).min(&(*fm.start_pos+fm.src.len()));
			return Some((BytePos(bp_start), BytePos(bp_end)))
		}
	}
	return None;
}
pub fn file_line_col_to_byte_pos(c:ty::ctxt,src_filename:&str,src_line:uint ,src_col:uint )->Option<codemap::BytePos>

{
//	for c.sess.codemap.files.rev_iter().advance |fm:&codemap::FileMap| {
	let mut i=c.sess.codemap.files.len();
	while i>0 {	// caution, need loop because we return, wait for new foreach ..in..
		i-=1;
		let fm=&c.sess.codemap.files[i];
		let filemap_filename:&str=fm.name;	
		if src_filename==filemap_filename {
			let line_pos=*fm.lines[src_line-1];
			let bp_start=*fm.lines[src_line-1]+src_col;
			return Some(BytePos(bp_start))
		}
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







