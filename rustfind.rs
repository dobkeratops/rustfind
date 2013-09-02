extern mod syntax;
extern mod rustc;
extern mod extra;

use rustc::{front, metadata, driver, middle};
use rustc::middle::{ty,typeck};
use rustc::metadata::cstore;

use std::num::*;
use std::{num,str,io,os,local_data};
use std::hashmap::HashMap;

use syntax::{parse,ast,ast_map,codemap,diagnostic};
use syntax::parse::token;
use syntax::visit::{Visitor, fn_kind};
use find_ast_node::{FNodeInfoMap,safe_node_id_to_type,get_node_info_str,build_node_info_map,AstNode,find_node_tree_loc_at_byte_pos,NodeTreeLoc,astnode_expr,FNodeInfo,ToJsonStr,ToJsonStrFc,AstNodeAccessors,KindToStr,build_node_def_node_table,get_node_source};
use jumptodefmap::*;

use syntax::abi::AbiSet;

use rfindctx::{RFindCtx,ctxtkey};
pub use codemaput::{ZTextFilePos,ZTextFilePosLen,get_span_str,ToZTextFilePos,ZIndexFilePos,ToZIndexFilePos};
use rsfind::{ShowDefMode,SDM_LineCol,SDM_Line,SDM_Source,SDM_GeditCmd,MyOption};
use crosscratemap::{CrossCrateMap,CrossCrateMapItem};

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
        optmulti("L"),optflag("d"),optflag("r"),optflag("j"),optflag("h"),optflag("i"),optflag("g"),optflag("w"),optflag("f"),optopt("x")
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
		println("rustfind: args/useage:-");
		println(" filename.rs [-L<lib path>] : create linked html pages for sources in crate");
		println(" -r filename.rs [-L<library path>]  : dump .rfx 'crosscratemap'  containing ast nodes and jump definitions");
		println(" -x where to look for html of external crates -  :eg rustfind filename mysource.rs -x ~/rust/src\n");
		println(" cratename.rs anotherfile.rs:line:col:");
		println("    - load cratename.rs; look for definition at anotherfile.rs:line:col");
		println("    - where anotherfile.rs is assumed to be a module of the crate");
		println(" -f filename.rs:line:col : return definition reference of symbol at given position");
		println(" -i filename.rs [-L<lib path>] : interactive mode");
		println(" -g format output as gedit filepos +line filename");
		println(" debug opts:-\n");
		println(" -j filename.rs [-L<library path>]  : dump JSON map of the ast nodes & defintions");
		println(" -d filename.rs [-L<lib path>] : debug for this tool");
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
			rfserver::run_server(dc);
			done=true;
		}
		if opt_present(&matches,"r") {
			println("Writing .rfx ast nodes/cross-crate-map:-");
			write_source_as_html_and_rfx(dc,lib_html_path, rust2html::DefaultOptions,false);
			println("Writing .rfx .. done");
			done=true;
		}

		// Dump as html..
		if opt_present(&matches,"w") || !(done) {
			println("Creating HTML pages from source & .rfx:-");
			write_source_as_html_and_rfx(dc,lib_html_path, rust2html::DefaultOptions,true);
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

fn debug_test(dc:&RFindCtx) {

	// TODO: parse commandline source locations,convert to codemap locations
	//dump!(ctxt.tycx);
	logi!("==== Get tables of node-spans,def_maps,JumpToDefTable..===")
    let (node_info_map,node_def_node,jdm)=jumptodefmap::make_jdm(dc);
	println(node_info_map.to_json_str(dc));

	logi!("==== Node Definition mappings...===")
	println(node_def_node.to_json_str());

	logi!("==== JumpToDef Table ===");
	println(jdm.to_json_str());


	logi!("==== dump def table.===");
	rf_ast_ut::dump_ctxt_def_map(dc.tycx);

	logi!("==== Test node search by location...===");

	// Step a test 'cursor' src_pos through the given source file..
	let mut test_cursor=15 as uint;

	while test_cursor<500 {
		let loc = rfindctx::get_source_loc(dc,codemap::BytePos(test_cursor));

		logi!(~"\n=====Find AST node at: ",loc.file.name,":",loc.line,":",loc.col,":"," =========");

		let nodetloc = find_node_tree_loc_at_byte_pos(dc.crate,codemap::BytePos(test_cursor));
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



pub fn write_source_as_html_and_rfx(dc:&RFindCtx,lib_html_path:&str,opts:uint, write_html:bool) {

	let mut xcm:~CrossCrateMap=~HashMap::new();
	cstore::iter_crate_data(dc.tycx.cstore, |i,md| {
//		dump!(i, md.name,md.data.len(),md.cnum);
		println("loading cross crate data "+i.to_str()+" "+md.name);
		let xcm_sub=crosscratemap::read_cross_crate_map(dc, i, md.name+&".rfx",lib_html_path);
		for (k,v) in xcm_sub.iter() {xcm.insert(*k,(*v).clone());}
	});

    let (info_map,def_map,jump_map)=make_jdm(dc);
	crosscratemap::write_cross_crate_map(dc,lib_html_path,
        info_map,def_map,jump_map);
	if write_html {
		rust2html::write_source_as_html_sub(dc,info_map,jump_map,xcm,lib_html_path,opts);
	}
}

