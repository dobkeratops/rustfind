#![feature(managed_boxes)]
#![feature(globs)]
#![feature(macro_rules)]
#![feature(log_syntax)]

extern crate syntax;
extern crate rustc;
extern crate getopts;
extern crate serialize;
extern crate collections;
extern crate time;
extern crate libc;

use std::io::println;

use rustc::{driver};
use rustc::metadata::creader::Loader;

use std::{os,local_data};
use std::cell::RefCell;
use collections::{HashMap,HashSet};
use std::path::Path;
use rust2html::RF_Options;

use getopts::{optmulti, optopt, optflag, getopts};

use syntax::{parse,ast,codemap};
use syntax::codemap::Pos;
use find_ast_node::{safe_node_id_to_type,get_node_info_str,find_node_tree_loc_at_byte_pos,ToJsonStr,ToJsonStrFc,AstNodeAccessors,get_node_source};
use jumptodefmap::{lookup_def_at_text_file_pos_str, make_jump_to_def_map, def_info_from_node_id,
    lookup_def_at_text_file_pos, dump_json};

use rfindctx::{RustFindCtx,ctxtkey};
pub use codemaput::{ZTextFilePos,ZTextFilePosLen,get_span_str,ToZTextFilePos,ZIndexFilePos,ToZIndexFilePos};
use rsfind::{SDM_LineCol,SDM_Source,SDM_GeditCmd};
use crosscratemap::CrossCrateMap;
use timer::Profiler;

pub mod rf_common;
pub mod find_ast_node;
pub mod text_formatting;
pub mod ioutil;
//pub mod htmlwriter;
pub mod rust2html;
pub mod codemaput;
pub mod rfindctx;
pub mod rsfind;
pub mod crosscratemap;
pub mod rfserver;
pub mod util;
pub mod rf_ast_ut;
pub mod jumptodefmap;
pub mod timer;
pub mod indexpage;
pub mod callgraph;

pub macro_rules! tlogi{
    ($($a:expr),*)=>(println!((file!()+":"+line!().to_str()+": " $(+$a.to_str())*) ))
}
pub macro_rules! logi{
    ($($a:expr),*)=>(println(""$(+$a.to_str())*) )
}
macro_rules! dump{ ($($a:expr),*)=>
    (   {   let mut txt=~"";
            $( { txt=txt.append(
                 format!("{:s}={:?}",stringify!($a),$a)+",")
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
        optopt("o", "", "Directory to output the html / rfx files to", "OUTDIR")
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

    let mut args = os::args();
    let binary = args.shift().unwrap();

    let opts = optgroups();
	let matches = getopts(args, opts).unwrap();
    let libs1 = matches.opt_strs("L").iter().map(|s| Path::new(s.as_slice())).collect::<Vec<Path>>();
    let libs= if libs1.len() > 0 {
        libs1
    } else {
        match os::getenv(&"RUST_LIBS") {
            Some(x) => vec!(Path::new(x)),
            None => vec!()
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
        local_data::set(ctxtkey, dc);

        let mut html_options = rust2html::RF_Options::new();
        if matches.opt_present("D") {
            debug_test(dc);
            done=true;
        } else if matches.opt_present("j"){
            dump_json(dc);
        }
        let mut i=0;
        let lib_html_path = if matches.opt_present("x") {
            let given_path=matches.opt_str("x").unwrap();
			if given_path!=~"." {given_path+"/"} 
			else 
				{~""}	// "-x ."we need this behaviour to emit relative pages we can publish
			
        } else {
			println!("No library path specified with -x, so using $RUST_PATH/src\n")
			match os::getenv("RUST_PATH") {
				None=>{
					println!("$RUST_PATH not set, so can't find library crate .rfx files to get standard library links. set this or pass the path with option -x");
					~""
				}
	            Some(value) => value+"/src/"
			}
        };
        if matches.opt_present("f") {
            while i<matches.free.len() {
                let mode=if matches.opt_present("g"){SDM_GeditCmd} else {SDM_Source};
                println!("{}", lookup_def_at_text_file_pos_str(dc,matches.free.get(i).as_slice(),mode).unwrap_or(~"no def found"));
                i+=1;
                done=true;
            }
        }
        if matches.opt_present("d") {
			html_options.rustdoc_url = Some(Path::new(matches.opt_str("d").unwrap_or(~"")));
			println!("TODO:linking to rustdoc pages at {}",html_options.rustdoc_url.clone().unwrap().as_str());
        }
        if matches.opt_present("n") {
			html_options.write_references=false;
		}
        if matches.opt_present("i") {
            rfserver::run_server(dc);
            done=true;
        }
        if matches.opt_present("o") {
            let out_dir = matches.opt_str("o").unwrap();
            let mut out_path = Path::new("./");
            out_path.push(out_dir + "/");
            html_options.output_dir = out_path;
        }
        if matches.opt_present("r") {
            println!("Writing .rfx ast nodes/cross-crate-map:-");
            write_crate_as_html_and_rfx(dc,lib_html_path, &html_options, false);
            println!("Writing .rfx .. done");
            done=true;
        }

        // Dump as html..
        if matches.opt_present("w") || !(done) {
            println!("Creating HTML pages from source & .rfx:-");
            write_crate_as_html_and_rfx(dc,lib_html_path, &html_options, true);
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
    println(node_info_map.to_json_str(dc));

    logi!("==== Node Definition mappings...===")
    println(node_def_node.to_json_str());

    logi!("==== JumpToDef Table ===");
    println(jdm.to_json_str());


    logi!("==== dump def table.===");
    rf_ast_ut::dump_ctxt_def_map(dc.tycx_ref());

    logi!("==== Test node search by location...===");

    // Step a test 'cursor' src_pos through the given source file..
    let mut test_cursor=15 as uint;

    while test_cursor<500 {
        let loc = rfindctx::get_source_loc(dc,codemap::BytePos(test_cursor as u32));

        logi!(~"\n=====Find AST node at: ",loc.file.name,":",loc.line,":",loc.col.to_uint().to_str(),":"," =========");

        let nodetloc = find_node_tree_loc_at_byte_pos(dc.crate_,codemap::BytePos(test_cursor as u32));
        let node_info =  get_node_info_str(dc,&nodetloc);
        dump!(node_info);
        println("node ast loc:"+(nodetloc.iter().map(|x| { x.get_id().to_str() })).collect::<Vec<~str>>().to_str());


        if_some!(id in nodetloc.last().get_ref().ty_node_id() then {
            logi!("source=",get_node_source(dc.tycx_ref(), &node_info_map,ast::DefId{krate:0,node:id}));
            if_some!(t in safe_node_id_to_type(dc.tycx_ref(), id) then {
                println!("typeinfo: {:?}",
                    {let ntt= rustc::middle::ty::get(t); ntt});
                dump!(id,dc.tycx_ref().def_map.borrow().find(&id));
                });
            let (def_id,opt_info)= def_info_from_node_id(dc,&node_info_map,id);
            if_some!(info in opt_info then{
                logi!("src node=",id," def node=",def_id,
                    " span=",format!("{:?}",info.span));
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

pub fn write_crate_as_html_and_rfx(dc:&RustFindCtx,lib_html_path:&str,opts: &RF_Options, write_html:bool) {
    let mut xcm:~CrossCrateMap=~HashMap::new();
	let tm=Profiler::new("write_crate_as_html_and_rfx");

    dc.cstore().iter_crate_data(|i,md| {
        println!("loading cross crate data {} {}", i, md.name);
        let xcm_sub=crosscratemap::cross_crate_map_read_into(xcm, i as int, lib_html_path + "lib"+md.name+&"/lib.rfx",lib_html_path);
    });

    let (info_map,def_map,jump_map) = make_jump_to_def_map(dc);
	// combine_current_create: Allows us to use 'xcm' alone to resolve DefIds elsewhere.
	// existing codepath also uses local info in 'jump_maps' to do the same job, we can simplify that out.
	crosscratemap::cross_crate_map_combine_current_crate(xcm, dc,&info_map,def_map,jump_map); 
	
    crosscratemap::cross_crate_map_write(dc,lib_html_path, &info_map,def_map,jump_map);
    if write_html {
		let mut tm=::timer::Profiler::new("write_crate_as_html_and_rfx");
        rust2html::write_crate_as_html_sub(dc,&info_map,jump_map,xcm,lib_html_path,opts);

    }
}

