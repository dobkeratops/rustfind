#![feature(managed_boxes)]
#![feature(globs)]
#![feature(macro_rules)]
#![feature(log_syntax)]
#![allow(non_camel_case_types)]
#![allow(unused_variable)]
#![allow(unused_imports)]
#![allow(unused_must_use)]

extern crate syntax;
extern crate rustc;
extern crate getopts;
extern crate serialize;
extern crate collections;

extern crate time;
extern crate libc;
extern crate log;

use std::io::println;

use rustc::{driver};
use rustc::metadata::creader::Loader;

use std::{os,local_data};
use std::cell::RefCell;
use collections::{HashMap,HashSet};
use std::path::Path;
use jumptodefmap::{MultiMap};
use std::path::posix;
//pub use super::NodeMaps;

use getopts::{optmulti, optopt, optflag, getopts};

use syntax::{parse,ast,codemap};
use syntax::codemap::Pos;
use find_ast_node::{safe_node_id_to_type,get_node_info_str,find_node_tree_loc_at_byte_pos,ToJsonStr,ToJsonStrFc,AstNodeAccessors,get_node_source};
use jumptodefmap::{lookup_def_at_text_file_pos_str, make_jump_to_def_map, def_info_from_node_id,
    lookup_def_at_text_file_pos, dump_json};

pub use rustfindctx::RustFindCtx;
//pub use rustfindctx::ctxtkey;
pub use codemaput::{ZTextFilePos,ZTextFilePosLen,get_span_str,ToZTextFilePos,ZIndexFilePos,ToZIndexFilePos};
use rsfind::{SDM_LineCol,SDM_Source,SDM_GeditCmd};
use crosscratemap::{CrossCrateMap,CrossCrateMapItem};
use timer::Profiler;
use jumptodefmap::*;
use find_ast_node::*;

pub mod rf_common;
pub mod find_ast_node;
pub mod text_formatting;
pub mod ioutil;
//pub mod htmlwriter;
pub mod rust2html;
pub mod codemaput;
pub mod rustfindctx;
pub mod rsfind;
pub mod crosscratemap;
pub mod rfserver;
pub mod util;
pub mod rf_ast_ut;
pub mod jumptodefmap;
pub mod timer;
pub mod indexpage;
pub mod callgraph;
pub mod visit_rust_ast;

pub struct RF_Options {
	pub write_file_path:bool,
	pub write_references:bool,
	pub write_callgraph:bool,
	pub write_html:bool,
	pub output_dir:posix::Path,
	pub rustdoc_url:Option<posix::Path>,
	pub callgraph_opt:callgraph::CG_Options,
}
impl RF_Options {
    pub fn new() -> RF_Options {
        RF_Options {
            write_file_path: true,
            write_references: true,
			write_callgraph: true,
			write_html:true,
            output_dir: Path::new(""),
            rustdoc_url: None,
			callgraph_opt:callgraph::CG_Options::new(),
        }
    }
}
pub struct NodeMaps<'nm, 'ast>  {
    pub node_info_map:&'nm FNodeInfoMap<'ast>,
    pub jump_def_map:&'nm JumpToDefMap,
    pub jump_ref_map:&'nm JumpToRefMap,
	pub xcmap:&'nm CrossCrateMap,
	pub krate: &'ast ast::Crate,
}
impl<'a,'astl> NodeMaps<'a,'astl> {
	pub fn rf_find_source(&'a self,defid:&ast::DefId)->Option<&'a CrossCrateMapItem>{
		self.xcmap.find(defid)
	}
	pub fn rf_find_local_node(&'a self,node:ast::NodeId)->Option<&'a CrossCrateMapItem> {
		self.xcmap.find(&ast::DefId{krate:0,node:node})
	}
}

pub macro_rules! tlogi{
    ($($a:expr),*)=>(println!((file!()+":"+line!().to_str()+": " $(+$a.to_str())*) ))
}
pub macro_rules! logi{
    ($($a:expr),*)=>(println(""$(+$a.to_str())*) )
}
macro_rules! dump{ ($($a:expr),*)=>
    (   {   let mut txt=StrBuf::new(); txt.push_str(file!()+":"+line!().to_str()+":");
            $( { txt=txt.append(
                 format!("\t{:s}={:?}",stringify!($a),$a)+";")
                }
            );*;
            ::std::io::println(txt.as_slice());
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

fn optgroups () -> ~[getopts::OptGroup] {
    ~[
        optmulti("L", "", "Path to search for libraries", "<lib path>"),
        optflag("D", "", "Debug for this tool (DEBUG)"),
        optflag("r", "", "Dump .rfx `crosscratemap` containing ast nodes and jump definitions"),
        optflag("j", "", "Dump json map of the ast nodes & definitions (DEBUG)"),
        optflag("h", "help", "Print this help menu and exit"),
        optflag("i", "", "Run interactive server"),
        optflag("g", "", "Format output as gedit `filepos +line filename` (use with -f)"),
        optflag("w", "", "Dump as html"),
        optflag("n", "no-refs", "Dont write references block at the end (default is yes)"),
        optflag("f", "", "Return definition reference of symbol at given position"),
        optopt("x", "external_crates", "Path to html of external crates, or '-x .' to emit relative ", "$RUST_PATH/src"),
        optflag("d", "", "TODO url for rustdoc pages to link to, default=unused"),
        optopt("o", "", "Directory to output the html / rfx files to", "OUTDIR"),
        optflag("C", "cg-local-only", "callgraph rendering limited to local crate only"),
        optflag("F", "cg-find", "find node in callgraph"),
        optflag("m", "cg-max-nodes", "max callgraph nodes to display")
    ]
}

fn usage(binary: &str) {
    let message = format!("Usage: {} [OPTIONS] INPUT", binary);
    println!("{}", getopts::usage(message, optgroups()));

    println!(" cratename.rs anotherfile.rs:line:col:");
    println!("    - load cratename.rs; look for definition at anotherfile.rs:line:col");
    println!("    - where anotherfile.rs is assumed to be a module of the crate");
    println!(" Set RUST_LIBS for a default library search path");
    println!(" ");
    println!("example use:- ");
    println!(" ");
    println!("rustfind cratefile.rs  - generate html view of source");
}

fn main() {

//    let mut args = os::args();
//    let binary = args.shift().unwrap();

    let mut args = os::args().move_iter().collect::<Vec<StrBuf>>();
    let binary = args.shift().unwrap();
    let args = args.move_iter().collect::<Vec<StrBuf>>();


    let opts = optgroups();
	let matches = getopts(args, opts).unwrap();
    let libs1 = matches.opt_strs("L").iter().map(|s| Path::new(s.as_slice())).collect::<Vec<Path>>();

    let libs:Vec<Path> = if libs1.len() > 0 {
        libs1
    } else {
        match os::getenv(&"RUST_LIBS") {
            Some(x) => vec!(Path::new(x)),
            None => {
				println("ERROR.. No library path specified with -L , and RUST_LIBS not set, \n so we dont have a library path to use");
				println("possible fix, set RUST_LIBS = $RUST_PATH/x86_64-unknown-linux-gnu/stage2/lib/rustlib/x86_64-unknown-linux-gnu/lib  ?");
				fail!();
				Vec::<Path>::new()
			}
        }
    };

    if matches.opt_present("h") || matches.opt_present("help") {
        usage(binary);
        return;
    };
    if matches.free.len()>0 {
        let mut done=false;
        let filename=util::get_filename_only(matches.free.get(0).as_slice());
        let dc = @get_ast_and_resolve(&Path::new(filename), libs.move_iter().collect());
//        local_data::set(ctxtkey, dc);

        let mut all_options = RF_Options::new();
        if matches.opt_present("D") {
            debug_test(dc);
            done=true;
        } else if matches.opt_present("j"){
            dump_json(dc);
        }
        let mut i=0;
        let lib_html_path = if matches.opt_present("x") {
            let given_path=matches.opt_str("x").unwrap();
			if given_path!=StrBuf::from_str(".") {given_path+"/"} 
			else 
				{StrBuf::from_str("")}	// "-x ."we need this behaviour to emit relative pages we can publish
			
        } else {
			println!("No library path specified with -x, so using $RUST_PATH/src\n")
			match os::getenv("RUST_PATH") {
				None=>{
					println!("$RUST_PATH not set, so can't find library crate .rfx files to get standard library links. set this or pass the path with option -x");
					StrBuf::from_str("")
				}
	            Some(value) => value+"/src/"
			}
        };
        if matches.opt_present("f") {
            while i<matches.free.len() {
                let mode=if matches.opt_present("g"){SDM_GeditCmd} else {SDM_Source};
                println!("{}", lookup_def_at_text_file_pos_str(dc,matches.free.get(i).as_slice(),mode).unwrap_or(StrBuf::from_str("no def found")));
                i+=1;
                done=true;
            }
        }
        if matches.opt_present("d") {
			all_options.rustdoc_url = Some(Path::new(matches.opt_str("d").unwrap_or(StrBuf::from_str(""))));
			println!("TODO:linking to rustdoc pages at {}",all_options.rustdoc_url.clone().unwrap().as_str());
        }
        if matches.opt_present("n") {
			all_options.write_references=false;
		}
        if matches.opt_present("i") {
            rfserver::run_server(dc);
            done=true;
        }
        if matches.opt_present("o") {
            let out_dir = matches.opt_str("o").unwrap();
            let mut out_path = Path::new("./");
            out_path.push(out_dir + "/");
            all_options.output_dir = out_path;
        }
        if matches.opt_present("F") {
            let find_node = matches.opt_str("F").unwrap();
			all_options.callgraph_opt.search.push(find_node);
        }
        if matches.opt_present("m") {
            let max_nodes:uint = from_str(matches.opt_str("F").unwrap_or(StrBuf::from_str("100"))).unwrap_or(100);
			all_options.callgraph_opt.max_nodes=max_nodes;
		}
        if matches.opt_present("r") {
            println!("Writing .rfx ast nodes/cross-crate-map:-");
			all_options.write_html=false;
            write_crate_as_html_and_rfx(dc,lib_html_path, &all_options);
            println!("Writing .rfx .. done");
            done=true;
        }
		// callgraph options, todo se
        if matches.opt_present("C") {
			all_options.callgraph_opt.local_only=true;
		}

		// callgraph options - If didn't search any, look for 'main'..
		if all_options.callgraph_opt.search.len()==0 {
			all_options.callgraph_opt.search.push("main".to_owned());
		}

        // Dump as html..
        if matches.opt_present("w") || !(done) {
            println!("Creating HTML pages from source & .rfx:-");
			all_options.write_html=true;
            write_crate_as_html_and_rfx(dc,lib_html_path, &all_options);
            println!("Creating HTML pages from source.. done");
        }
	} else {
        usage(binary);
        return;
    }
}

/// tags: crate,ast,parse resolve
/// Parses, resolves the given crate

fn get_ast_and_resolve(
    cpath: &std::path::Path,
    libs: HashSet<std::path::Path>)
    -> RustFindCtx {

    let parsesess = parse::new_parse_sess();
    let sessopts = driver::session::Options {
        maybe_sysroot: Some(std::os::self_exe_path().unwrap()),
        addl_lib_search_paths:  RefCell::new(libs),
        ..  (rustc::driver::session::basic_options()).clone()
    };
    // Currently unused
//  let quiet = true;


	let codemap = syntax::codemap::CodeMap::new();
    let diagnostic_handler = syntax::diagnostic::default_handler(); // TODO add back the blank emitter here ...
    let span_diagnostic_handler =
        syntax::diagnostic::mk_span_handler(diagnostic_handler, codemap);


    let sess = driver::driver::build_session_(sessopts, 
                                              Some(cpath.clone()), 
//                                              parsesess.cm,
                                              span_diagnostic_handler);
    let cfg = driver::driver::build_configuration(&sess); //was, @"", &input);
    let input=driver::driver::FileInput(cpath.clone());

    let crate1 = driver::driver::phase_1_parse_input(&sess,cfg.clone(),&input);
	
    let (crate2, ast_map) = {
		let loader = &mut Loader::new(&sess);
		driver::driver::phase_2_configure_and_expand(
			&sess,
			loader,
			crate1,
			&from_str("TODO_WHAT_IS CrateId").unwrap()
		)
	};

//    let ca = driver::driver::phase_3_run_analysis_passes(sess, &crate2, ast_map);
//    RFindCtx { crate_: @crate2, tycx: ca.ty_cx,/* sess: sess,*/ ca:ca }
//    let driver::driver::CrateAnalysis{exported_items,public_items, ty_cx,..}
//		 = driver::driver::phase_3_run_analysis_passes(sess, &crate2, ast_map);
//    RFindCtx { crate_: @crate2, tycx: ty_cx,/* sess: sess,*/ /*ca:ca*/ }
	let ca= driver::driver::phase_3_run_analysis_passes(sess, &crate2, ast_map);
    RustFindCtx { crate_: @crate2, /*tycx: ca.ty_cx, sess: sess,*/ ca:ca }
}

fn debug_test(dc:&RustFindCtx) {

    // TODO: parse commandline source locations,convert to codemap locations
    //dump!(ctxt.tycx);
    logi!("==== Get tables of node-spans,def_maps,JumpToDefTable..===")
    let (node_info_map,node_def_node,jdm)=make_jump_to_def_map(dc);
    println(node_info_map.to_json_str(dc).as_slice());

    logi!("==== Node Definition mappings...===")
    println(node_def_node.to_json_str().as_slice());

    logi!("==== JumpToDef Table ===");
    println(jdm.to_json_str().as_slice());


    logi!("==== dump def table.===");
    rf_ast_ut::dump_ctxt_def_map(dc.tycx_ref());

    logi!("==== Test node search by location...===");

    // Step a test 'cursor' src_pos through the given source file..
    let mut test_cursor=15 as uint;

    while test_cursor<500 {
        let loc = rustfindctx::get_source_loc(dc,codemap::BytePos(test_cursor as u32));

        logi!("\n=====Find AST node at: ",loc.file.name,":",loc.line,":",loc.col.to_uint().to_str(),":"," =========");

        let nodetloc = find_node_tree_loc_at_byte_pos(dc.crate_,codemap::BytePos(test_cursor as u32));
        let node_info =  get_node_info_str(dc,&nodetloc);
        dump!(node_info);
        println("node ast loc:"+(nodetloc.iter().map(|x| { x.rf_get_id().to_str() })).collect::<Vec<StrBuf>>().to_str());


        if_some!(id in nodetloc.last().get_ref().rf_ty_node_id() then {
            logi!("source=",get_node_source(dc.tycx_ref(), &node_info_map,ast::DefId{krate:0,node:id}));
            if_some!(t in safe_node_id_to_type(dc.tycx_ref(), id) then {
                println!("typeinfo: {:?}",
                    {let ntt= rustc::middle::ty::get(t); ntt});
                dump!(id,dc.tycx_ref().def_map.borrow().find(&id));
                });
            let (def_id,opt_info)= def_info_from_node_id(dc,&node_info_map,id);
            if_some!(info in opt_info then{
                logi!("src node=",id," def node=",def_id,
                    " span=",format!("{:?}",info.rf_span()));
                logi!("def source=", get_node_source(dc.tycx_ref(), &node_info_map, def_id));
            })
        })

        test_cursor+=20;
    }

    // test byte pos from file...
    logi!("====test file:pos source lookup====");
    dump!(ZTextFilePosLen::new("test_input.rs",3-1,0,10).get_str(dc.tycx_ref()));
    dump!(ZTextFilePosLen::new("test_input.rs",9-1,0,10).get_str(dc.tycx_ref()));
    dump!(ZTextFilePosLen::new("test_input2.rs",4-1,0,10).get_str(dc.tycx_ref()));
    dump!(ZTextFilePosLen::new("test_input2.rs",11-1,0,10).get_str(dc.tycx_ref()));
    let ospan=ZTextFilePosLen::new("test_input2.rs", 10-1,0,32).to_byte_pos(dc.tycx_ref());
    if_some!(x in ospan then {
        let (lo,hi)=x;
        logi!(get_span_str(dc.tycx_ref(), &codemap::Span{lo:lo,hi:hi,expn_info:None} ));
    });
    dump!(codemaput::zget_file_line_str(dc.tycx_ref(),"test_input2.rs",5-1));
    dump!(codemaput::zget_file_line_str(dc.tycx_ref(),"test_input.rs",9-1));

    logi!("\n====test full file:pos lookup====");
    dump!(lookup_def_at_text_file_pos(dc, &ZTextFilePos::new("test_input.rs",8-1,21),SDM_Source));println!("");
    dump!(lookup_def_at_text_file_pos(dc, &ZTextFilePos::new("test_input2.rs",3-1,12),SDM_Source));println!("");
    dump!(lookup_def_at_text_file_pos(dc, &ZTextFilePos::new("test_input.rs",10-1,8),SDM_Source));println!("");
    dump!(lookup_def_at_text_file_pos(dc, &ZTextFilePos::new("test_input.rs",13-1,16),SDM_Source));println!("");
    dump!(lookup_def_at_text_file_pos(dc, &ZTextFilePos::new("test_input.rs",11-1,10),SDM_Source));println!("");
}

pub fn write_crate_as_html_and_rfx(dc:&RustFindCtx,lib_html_path:&str,opts: &RF_Options) {
    let mut xcm:CrossCrateMap=HashMap::new();
	let tm=Profiler::new("write_crate_as_html_and_rfx");

    dc.cstore().iter_crate_data(|i,md| {
        println!("loading cross crate data {} {}", i, md.name);
        let xcm_sub=crosscratemap::cross_crate_map_read_into(&mut xcm, i as int, lib_html_path + "lib"+md.name+&"/lib.rfx",lib_html_path);
    });

	// Pull togetherr the node info maps/def maps /jump maps..
    let (info_map,def_map,jump_def_map) = make_jump_to_def_map(dc);
//	nim:&FNodeInfoMap, jdm:&JumpToDefMap,
    let mut def2refs = box MultiMap::new();
    for (&nref,&ndef) in jump_def_map.iter() {
        if ndef.krate==0 {
            def2refs.insert(ndef.node,nref);
        }
    };

	// combine_current_create: Allows us to use 'xcm' alone to resolve DefIds elsewhere.
	// existing codepath also uses local info in 'jump_maps' to do the same job, we can simplify that out.
	crosscratemap::cross_crate_map_combine_current_crate(&mut xcm, dc,&info_map,def_map,jump_def_map); 
    crosscratemap::cross_crate_map_write(dc,lib_html_path, &info_map,def_map,jump_def_map);

	// Finally collect these maps together for convinience. We always seem to need all of them.
    let nmaps=NodeMaps { node_info_map:&info_map, jump_def_map:jump_def_map, jump_ref_map:def2refs,xcmap: &xcm, krate:dc.crate_};


    if opts.write_html {
		let tm=::timer::Profiler::new("write_crate_as_html_and_rfx");
        rust2html::write_crate_as_html_sub(dc,&nmaps,lib_html_path,opts);
    }
	if !opts.write_callgraph {
		return;
	}
	let dirname=opts.output_dir.as_str().unwrap_or("");
	let dirname=if dirname.len()>0{dirname+"/"}else{StrBuf::from_str("")};
	callgraph::write_call_graph(&nmaps, dirname,"callgraph", &opts.callgraph_opt);
}



