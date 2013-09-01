extern mod syntax;
extern mod rustc;
extern mod extra;

use rustc::{front, metadata, driver, middle};
use rustc::middle::{ty,typeck};
use rustc::metadata::cstore;

use std::num;
use std::num::*;
use std::str;

use syntax::parse;
use syntax::ast;
use syntax::ast_map;
use syntax::visit;
use syntax::parse::token;
//use syntax::visit::*;
use syntax::visit::{Visitor, fn_kind};
use find_ast_node::{FNodeInfoMap,build_node_info_map,get_def_id,get_node_info_str,safe_node_id_to_type,byte_pos_from_text_file_pos_str,AstNode,byte_pos_from_text_file_pos_str,find_node_tree_loc_at_byte_pos,NodeTreeLoc,astnode_expr,FNodeInfo,ToJsonStr,ToJsonStrFc,AstNodeAccessors,KindToStr,build_node_def_node_table};
use syntax::diagnostic;
use syntax::codemap::BytePos;
use std::io;
use jumptodefmap::*;

use syntax::abi::AbiSet;
use syntax::ast;
use syntax::codemap;

use std::hashmap::HashMap;
use std::os;
use std::local_data;
use extra::json::ToJson;
use rfindctx::{RFindCtx,ctxtkey};
pub use codemaput::{ZTextFilePos,ZTextFilePosLen,get_span_str,ToZTextFilePos,ZIndexFilePos,ToZIndexFilePos};
//pub use codemaput::*;
use rsfind::{ShowDefMode,SDM_LineCol,SDM_Line,SDM_Source,SDM_GeditCmd,MyOption};
use crosscratemap::{CrossCrateMap,CrossCrateMapItem};
use rfserver::rustfind_interactive;
use jumptodefmap::*;

pub mod find_ast_node;
pub mod text_formatting;
pub mod ioutil;
pub mod htmlwriter;
pub mod rust2html;
pub mod codemaput;
pub mod rfindctx;
pub mod rsfind;
pub mod crosscratemap;
pub mod rfserver;
pub mod util;
pub mod rf_ast_ut;
pub mod jumptodefmap;


/*
  test multiline
  */

/*
example of cross crate referencing
/home/walter/gplsrc/rust/src/librustc/middle/ty.rs:4080:	
pub fn lookup_struct_fields(cx: ctxt, did: ast::def_id) -> ~[field_ty] {
	if did.crate == ast::LOCAL_CRATE {
		....
	} else {
		return csearch::get_struct_fields(cx.sess.cstore, did);
	}
*/

pub fn dump_json(dc:&RFindCtx) {
	// TODO: full/partial options - we currently wwrite out all the nodes we find.
	// need option to only write out nodes that map to definitons. 
	println("{");
	println("\tcode_map:[");
//	for dc.sess.codemap.files.iter().advance |f| {
	for f in dc.sess.codemap.files.iter() {
		print("\t\t{ name:\""+f.name+"\",\tglobal_start_pos:"+f.start_pos.to_str()+
			",\tlength:"+(f.src.len()).to_str()+
			",\tnum_lines:"+f.lines.len().to_str()+
			",\tlines:[\n"+ util::flatten_to_str(*f.lines, |&x|{*x-*f.start_pos} ,",") +
			"\n\t\t]\n\t},\n");
	}
	println("\t]");
	println("\tnode_spans:");
	let nim=build_node_info_map(dc.crate);
	let node_def_node = build_node_def_node_table(dc);
	let jdm=build_jump_to_def_map(dc,nim,node_def_node);
	println(nim.to_json_str(dc));	
	println(",");
	println("\tnode_defs [\n");
	println(jdm.to_json_str());
	println("\t],\n");
	println("\tdef_ids:");
	println(node_def_node.to_json_str());
	println("}");
}


/*
pub fn build_jump_to_def_map(dc:&RFindCtx, nim:@mut FNodeInfoMap,nd:&HashMap<ast::NodeId,ast::def_id>)->~JumpToDefMap{
// todo: NodeId->AStNode  .. lookup_def_ inner functionality extracted
	let mut jdm=~HashMap::new();
	for (k,nim) in nim.iter() {
		match get_ast_node_of_node_id(nim,*k) {
			None=>{},
			Some(ast_node)=>{
				match lookup_def_node_of_node(dc,&ast_node,nim,nd) {
					None=>{},
					Some(def_node_id)=>{
						if *k != def_node_id.node && def_node_id.crate==0 || (def_node_id.crate!=0) {
							jdm.insert(*k,def_node_id);
						}
					}
				}
			}
		}
	}
	jdm
}
*/



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
	($b:ident in $a:expr then $c:expr _else $d:expr)=>(
		match $a {
			Some($b)=>$c,
			None=>{$d}
		}
	);
}



fn main() {

    use extra::getopts::*;
    use std::hashmap::HashMap;


	//test_default_arg(1,(2,3));
	//test_default_arg(1);

    let args = os::args();

    let opts = ~[
        optmulti("L"),optflag("d"),optflag("j"),optflag("h"),optflag("i"),optflag("g"),optflag("w"),optflag("f"),optopt("x")
    ];

	let matches = getopts(args.tail(), opts).unwrap();
    let libs1 = opt_strs(&matches, "L").map(|s| Path(*s));
	let libs=if libs1.len()>0 {libs1} else {
		match (os::getenv(&"RUST_LIBS")) {
			Some(x)=>~[Path(x)],
			None=>~[]
		}
	};

	if opt_present(&matches,"h") {
		println("rustfind: useage:-");
		println(" filename.rs [-L<lib path>] : create linked html pages for sources in crate");
		println(" -j filename.rs [-L<library path>]  : dump JSON map of the ast nodes & defintions");
		println(" cratename.rs anotherfile.rs:line:col:");
		println("    - load cratename.rs; look for definition at anotherfile.rs:line:col");
		println("    - where anotherfile.rs is assumed to be a module of the crate");
		println(" -f filename.rs:line:col : return definition reference of symbol at given position");
		println(" -i filename.rs [-L<lib path>] : interactive mode");
		println(" -d filename.rs [-L<lib path>] : debug for this tool");
		println(" -g format output as gedit filepos +line filename");
		println(" -x where to look for html of external crates -  :eg rustfind filename mysource.rs -x ~/rust/src\n");
		println(" set RUST_LIBS for a default library search path");
	};
	if matches.free.len()>0 {
		let mut done=false;
		let filename=util::get_filename_only(matches.free[0]);
		let dc = @get_ast_and_resolve(&Path(filename), libs);
		local_data::set(ctxtkey, dc);
	
		if (opt_present(&matches,"d")) {
			debug_test(dc);
			done=true;
		} else if (opt_present(&matches,"j")){
			dump_json(dc);
		}
		let mut i=0;
		let lib_html_path=if opt_present(&matches,"x"){ opt_str(&matches,"x")} else {~""};
		if (opt_present(&matches,"f")) {
			while i<matches.free.len() {
				let mode=if opt_present(&matches,"g"){SDM_GeditCmd} else {SDM_Source};
				print(lookup_def_at_text_file_pos_str(dc,matches.free[i],mode).unwrap_or_default(~"no def found\n"));
				i+=1;
				done=true;
			}
		}
		if opt_present(&matches,"i") {
			rustfind_interactive(dc);
			done=true;
		}

		// Dump as html..
		if opt_present(&matches,"w") || !(done) {
			println("Creating HTML pages from source:-");
			write_source_as_html(dc,lib_html_path, rust2html::DefaultOptions);
			println("Creating HTML pages from source.. done");
		}
	}
}

/// tags: crate,ast,parse resolve
/// Parses, resolves the given crate
fn get_ast_and_resolve(
	cpath: &std::path::PosixPath, 
	libs: ~[std::path::PosixPath]) 
	-> RFindCtx {

    let parsesess = parse::new_parse_sess(None);
    let sessopts = @driver::session::options {
        binary: @"rustdoc",
        maybe_sysroot: Some(@std::os::self_exe_path().unwrap().pop()),
        addl_lib_search_paths: @mut libs,
        ..  (*rustc::driver::session::basic_options()).clone()
    };
	let quiet=true;
	fn no_emit(cmsp: Option<(@codemap::CodeMap, codemap::span)>, msg: &str, lvl: syntax::diagnostic::level) {
	}


    let diagnostic_handler = syntax::diagnostic::mk_handler(None);
    let span_diagnostic_handler =
        syntax::diagnostic::mk_span_handler(diagnostic_handler, parsesess.cm);


    let mut sess = driver::driver::build_session_(sessopts, parsesess.cm,
                                                  if quiet{no_emit}else{syntax::diagnostic::emit},
                                                  span_diagnostic_handler);
	let input=driver::driver::file_input(cpath.clone());
	let cfg= driver::driver::build_configuration(sess); //was, @"", &input);

	let crate1=driver::driver::phase_1_parse_input(sess,cfg.clone(),&input);
	let crate2=driver::driver::phase_2_configure_and_expand(sess,cfg,crate1);
	
	let ca=driver::driver::phase_3_run_analysis_passes(sess,crate2);  
    RFindCtx { crate: crate2, tycx: ca.ty_cx, sess: sess, ca:ca }
}
/*
pub fn some<T>(o:&Option<T>,f:&fn(t:&T)) {
	match *o {
		Some(ref x)=>f(x),
		None=>{}
	}
}
*/





fn lookup_def_at_file_line_pos_old(dc:&RFindCtx,filepos:&str, show_all:ShowDefMode)->Option<~str> {

	let toks:~[&str]=filepos.split_iter(':').collect();
	if toks.len()<3 { return None }

//	let line:Option<uint> = FromStr::from_str(toks[1]);
	if_some!(line in FromStr::from_str::<uint>(toks[1]) then {
		if_some!(col in FromStr::from_str::<uint>(toks[2]) then {
			//todo - if no column specified, just lookup everything on that line!

			match ZTextFilePos::new(toks[0],line-1,col-1).to_byte_pos(dc.tycx) {
				None=>{},
				Some(bp)=>{
					return lookup_def_at_byte_pos(dc,bp,show_all)
				}
			}
		})
	})
	return None;
}


pub fn lookup_def_at_text_file_pos(dc:&RFindCtx, tfp:&ZTextFilePos, show_mode:ShowDefMode)->Option<~str> {
	match tfp.to_byte_pos(dc.tycx) {
		None=>None,
		Some(bp)=>lookup_def_at_byte_pos(dc,bp,show_mode)
	}
}

pub fn lookup_def_at_text_file_pos_str(dc:&RFindCtx,file_pos_str:&str, show_mode:ShowDefMode)->Option<~str> {
	match byte_pos_from_text_file_pos_str(dc,file_pos_str) {
		None=>None,
		Some(bp)=>lookup_def_at_byte_pos(dc,bp,show_mode),
	}
}

pub fn node_id_from_text_file_pos_str(dc:&RFindCtx, file_pos_str:&str)->Option<ast::NodeId> {
	match node_from_text_file_pos_str(dc, file_pos_str) {
		None=>None,
		Some(an)=>an.get_id()
	}
}
pub fn node_from_text_file_pos_str(dc:&RFindCtx, file_pos_str:&str)->Option<AstNode> {
	match byte_pos_from_text_file_pos_str(dc,file_pos_str) {
		Some(bp)=>{let ndt=find_node_tree_loc_at_byte_pos(dc.crate,bp);Some(ndt.last().clone())},
		None=>None
	}
}


pub fn lookup_def_at_byte_pos(dc:&RFindCtx, bp:BytePos, m:ShowDefMode)->Option<~str> {
	let ndt=find_node_tree_loc_at_byte_pos(dc.crate,bp);
	lookup_def_of_node_tree_loc(dc,&ndt,m)
}

pub fn dump_node_tree_loc(ndt:&NodeTreeLoc) {
//	for ndt.iter().advance |x|
	for x in ndt.iter()
	 {print(x.kind_to_str()+".");} print("\n");
}

fn lookup_def_of_node_tree_loc(dc:&RFindCtx,node_tree_loc:&NodeTreeLoc,m:ShowDefMode)->Option<~str> {
	lookup_def_of_node(dc,node_tree_loc.last(),m)
}

fn lookup_def_of_node(dc:&RFindCtx,node:&AstNode,m:ShowDefMode)->Option<~str> {
	println("def of node:"+node.get_id().unwrap_or_default(0).to_str());
	let node_spans=build_node_info_map(dc.crate);
	let node_def_node = build_node_def_node_table(dc);
	lookup_def_of_node_sub(dc,node,m,node_spans,node_def_node)
}


fn lookup_def_of_node_sub(dc:&RFindCtx,node:&AstNode,m:ShowDefMode,nim:&FNodeInfoMap, node_def_node:&HashMap<ast::NodeId,ast::def_id>)->Option<~str> {
	// TODO - cache outside?


	fn mk_result(dc:&RFindCtx,  m:ShowDefMode, nim:&FNodeInfoMap, def_node_id:ast::def_id, extra_str:&str)->Option<~str> {
		if def_node_id.crate!=0 {
			Some(~"{cross-crate-def not implemented, "+def_node_id.to_str()+"}")
		}
		else {
			match nim.find(&def_node_id.node) {
				None=>None,
				Some(def_info)=>{
					let loc=rfindctx::get_source_loc(dc,def_info.span.lo);
					let def_pos_str=
						loc.file.name + ":"+loc.line.to_str()+": "+
							match m { SDM_LineCol=>loc.col.to_str()+": ", _ =>~"" }+"\n";
					return	match m{
						SDM_Source=>Some(def_pos_str+get_node_source(dc.tycx,nim, def_node_id)+"\n"),
						SDM_GeditCmd=>Some("+"+loc.line.to_str()+" "+loc.file.name+" "),
						_ => Some(def_pos_str)
					};

				}
			}
		}
	}
	match lookup_def_node_of_node(dc, node, nim, node_def_node) {
		None=>None,
		Some(def_node_id)=>mk_result(dc,m, nim,def_node_id, "")
	}
}


pub fn dump_node_source_for_single_file_only(text:&[u8], ns:&FNodeInfoMap, id:ast::NodeId) {
	match(ns.find(&id)) {None=>logi!("()"),
		Some(info)=>{
			codemaput::dump_span(text, &info.span);
		}
	}
}

// TODO- this should return a slice?
pub fn get_node_source(c:ty::ctxt, nim:&FNodeInfoMap, did:ast::def_id)->~str {
	if did.crate==0{
		match (nim.find(&did.node)){
			None=>~"",
			Some(info)=>{
				get_span_str(c,&info.span)
			}
		}
	} else {
		"{out of crate def:"+did.to_str()+"}"
	}
}




// see: tycx.node_types:node_type_table:HashMap<id,t>
// 't'=opaque ptr, ty::get(:t)->t_box_ to resolve it







fn debug_test(dc:&RFindCtx) {

	// TODO: parse commandline source locations,convert to codemap locations
	//dump!(ctxt.tycx);
	logi!("==== Get table of node-spans...===")
	let node_info_map=build_node_info_map(dc.crate);
	println(node_info_map.to_json_str(dc));

	logi!("==== Node Definition mappings...===")
	let node_def_node = build_node_def_node_table(dc);
	println(node_def_node.to_json_str());

	logi!("==== JumpToDef Table ===");
	let jdm = build_jump_to_def_map(dc,node_info_map,node_def_node);
	println(jdm.to_json_str());


	logi!("==== dump def table.===");
	rf_ast_ut::dump_ctxt_def_map(dc.tycx);

	logi!("==== Test node search by location...===");

	// Step a test 'cursor' src_pos through the given source file..
	let mut test_cursor=15 as uint;

	while test_cursor<500 {
		let loc = rfindctx::get_source_loc(dc,BytePos(test_cursor));

		logi!(~"\n=====Find AST node at: ",loc.file.name,":",loc.line,":",loc.col,":"," =========");

		let nodetloc = find_node_tree_loc_at_byte_pos(dc.crate,BytePos(test_cursor));
		let node_info =  get_node_info_str(dc,&nodetloc);
		dump!(node_info);
		println("node ast loc:"+(do nodetloc.map |x| { x.get_id().to_str() }).to_str());


		if_some!(id in nodetloc.last().ty_node_id() then {
			logi!("source=",get_node_source(dc.tycx, node_info_map,ast::def_id{crate:0,node:id}));
			if_some!(t in safe_node_id_to_type(dc.tycx, id) then {
				println(fmt!("typeinfo: %?",
					{let ntt= rustc::middle::ty::get(t); ntt}));
				dump!(id,dc.tycx.def_map.find(&id));
				});
			let (def_id,opt_info)= def_info_from_node_id(dc,node_info_map,id); 
			if_some!(info in opt_info then{
				logi!("src node=",id," def node=",def_id,
					" span=",fmt!("%?",info.span));
				logi!("def source=", get_node_source(dc.tycx, node_info_map, def_id));
			})
		})

		test_cursor+=20;
	}

	// test byte pos from file...
	logi!("====test file:pos source lookup====");
	dump!(ZTextFilePosLen::new("test_input.rs",3-1,0,10).get_str(dc.tycx));
	dump!(ZTextFilePosLen::new("test_input.rs",9-1,0,10).get_str(dc.tycx));
	dump!(ZTextFilePosLen::new("test_input2.rs",4-1,0,10).get_str(dc.tycx));
	dump!(ZTextFilePosLen::new("test_input2.rs",11-1,0,10).get_str(dc.tycx));
	let ospan=ZTextFilePosLen::new("test_input2.rs", 10-1,0,32).to_byte_pos(dc.tycx);
	if_some!(x in ospan then {
		let (lo,hi)=x;
		logi!(get_span_str(dc.tycx, &codemap::span{lo:lo,hi:hi,expn_info:None} ));
	});
	dump!(codemaput::zget_file_line_str(dc.tycx,"test_input2.rs",5-1));
	dump!(codemaput::zget_file_line_str(dc.tycx,"test_input.rs",9-1));
	
	logi!("\n====test full file:pos lookup====");
	dump!(lookup_def_at_text_file_pos(dc, &ZTextFilePos::new("test_input.rs",8-1,21),SDM_Source));println("");
	dump!(lookup_def_at_text_file_pos(dc, &ZTextFilePos::new("test_input2.rs",3-1,12),SDM_Source));println("");
	dump!(lookup_def_at_text_file_pos(dc, &ZTextFilePos::new("test_input.rs",10-1,8),SDM_Source));println("");
	dump!(lookup_def_at_text_file_pos(dc, &ZTextFilePos::new("test_input.rs",13-1,16),SDM_Source));println("");
	dump!(lookup_def_at_text_file_pos(dc, &ZTextFilePos::new("test_input.rs",11-1,10),SDM_Source));println("");
	
}

pub fn read_cross_crate_map(dc:&RFindCtx, crate_num:int, crate_name:&str,lib_path:&str)->~CrossCrateMap {
	let mut raw_bytes=ioutil::fileLoad(crate_name);
	if (raw_bytes.len()==0) {
		println("loading lib crosscratemap "+lib_path+"/"+crate_name);
		raw_bytes=ioutil::fileLoad(lib_path+"/"+crate_name);
	}
	let rfx=str::from_bytes(raw_bytes);
	println("loaded cratemap "+rfx.len().to_str()+"bytes"+" as crate "+crate_num.to_str());
//	for &x in raw_bytes.iter() { rfx.push_char(x as char); }

	let mut xcm=~HashMap::new();
	for s in rfx.line_iter() {
//		println(s.to_str());
		let toks=s.split_iter('\t').to_owned_vec();
		if toks.len()>=6 {
			match toks[0] {
				"jdef"=> {
					// special entries
				}
				_=>{	// everything else is a node instance

					let node_id:int=std::int::from_str(toks[1]).unwrap_or_default(0);
					xcm.insert(ast::def_id{crate:crate_num, node:node_id,},
						CrossCrateMapItem{
							fname:	toks[2].to_owned(),
							line:	std::uint::from_str(toks[3]).unwrap_or_default(0)-1,
							col:	std::uint::from_str(toks[4]).unwrap_or_default(0),
							len:	std::uint::from_str(toks[5]).unwrap_or_default(0)
						}
					);
				}
			}
		}
	}
	//dump!(xcm);
	println("from cratemap "+rfx.len().to_str()+"bytes");
	xcm
}


pub fn write_source_as_html(dc:&RFindCtx,lib_html_path:~str,opts:uint) {

	let mut xcm:~CrossCrateMap=~HashMap::new();
	cstore::iter_crate_data(dc.tycx.cstore, |i,md| {
//		dump!(i, md.name,md.data.len(),md.cnum);
		println("loading cross crate data "+i.to_str()+" "+md.name);
		let xcm_sub=read_cross_crate_map(dc, i, md.name+&".rfx",lib_html_path);
		for (k,v) in xcm_sub.iter() {xcm.insert(*k,(*v).clone());}
	});

	let nim=build_node_info_map(dc.crate);
	let ndm = build_node_def_node_table(dc);
	let jdm=build_jump_to_def_map(dc,nim,ndm);
	rust2html::write_source_as_html_sub(dc,nim,jdm,xcm,lib_html_path,opts);
	write_cross_crate_map(dc,lib_html_path,nim,ndm,jdm);
}
fn str_of_opt_ident(dc:&RFindCtx, ident:Option<ast::ident>)->~str{
	match ident {
		Some(i)=>dc.sess.str_of(i).to_owned(), None=>~""
	}
}
pub fn write_cross_crate_map(dc:&RFindCtx,lib_html_path:~str,nim:&FNodeInfoMap, ndm:&HashMap<ast::NodeId, ast::def_id>, jdm:&JumpToDefMap) {
	// write inter-crate node map
	let crate_rel_path_name= dc.sess.codemap.files[0].name;
	

	let curr_crate_name_only=crate_rel_path_name.split_iter('/').last().unwrap_or_default("").split_iter('.').nth(0).unwrap_or_default("");
	println("writing rustfind cross-crate link info for "+curr_crate_name_only);
	let mut outp=~"";
	// todo - idents to a seperate block, they're rare.
	for (k,ni) in nim.iter() {
		match ni.span.lo.to_text_file_pos(dc.tycx) {
			Some(tfp)=>{	
				outp.push_str(curr_crate_name_only+"\t"+k.to_str()+"\t"+tfp.name+"\t"+(tfp.line+1).to_str()+"\t"+tfp.col.to_str()+"\t"+(*ni.span.hi-*ni.span.lo).to_str() + "\t"+ni.kind+ "\t"+str_of_opt_ident(dc,ni.ident)+"\n");
			},
			None=>{}
		}
	}
	
	for (k,v) in jdm.iter()  {
		let cname:~str= if v.crate>0 {
			cstore::get_crate_data(dc.tycx.cstore,v.crate).name.to_str()
		} else {
			curr_crate_name_only.to_str()
		};
		//println(cdata.name);
		outp.push_str("jdef\t"+k.to_str()+"\t"+cname+"\t" +v.node.to_str()+"\n");
	}	

//	for (k,v) in ndm.iter()  {
//		outp.push_str("def\t"+k.to_str()+"\t"+dc.tycx.cstore.crate() +v.node.to_str()+"\n");
//	}	

	
	{	let x=curr_crate_name_only+~".rfx";
		println("writing "+x);
		ioutil::fileSaveStr(outp, x);
	}	
}

