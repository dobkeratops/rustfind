extern mod syntax;
extern mod rustc;
extern mod extra;

use rustc::{front, metadata, driver, middle};
use rustc::middle::*;
use rustc::middle::typeck;

use std::num;
use std::num::*;

use syntax::parse;
use syntax::ast;
use syntax::ast_map;
use syntax::visit;
use syntax::parse::token;
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
use rfindctx::*;

use rust2html::*;
use codemaput::*;


pub mod find_ast_node;
pub mod text_formatting;
pub mod ioutil;
pub mod htmlwriter;
pub mod rust2html;
pub mod codemaput;
pub mod rfindctx;
/*
  test multiline
  */
#[deriving(Clone, Eq, Encodable, Decodable)]
enum ShowDefMode {
	SDM_Line=0,
	SDM_LineCol=1,
	SDM_Source=2,
	SDM_GeditCmd=3
}

fn dump_json(dc:&RFindCtx) {
	// TODO: full/partial options - we currently wwrite out all the nodes we find.
	// need option to only write out nodes that map to definitons. 
	println("{");
	println("\tcode_map:[");
//	for dc.sess.codemap.files.iter().advance |f| {
	for f in dc.sess.codemap.files.iter() {
		print("\t\t{ name:\""+f.name+"\",\tglobal_start_pos:"+f.start_pos.to_str()+
			",\tlength:"+(f.src.len()).to_str()+
			",\tnum_lines:"+f.lines.len().to_str()+
			",\tlines:[\n"+ flatten_to_str(*f.lines, |&x|{*x-*f.start_pos} ,",") +
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

pub fn build_jump_to_def_map(dc:&RFindCtx, nim:@mut FNodeInfoMap,nd:&HashMap<ast::NodeId,ast::def_id>)->~JumpToDefMap{
// todo: NodeId->AStNode  .. lookup_def_ inner functionality extracted
	let mut jdm=~HashMap::new();
	for (k,_) in nim.iter() {
		match get_ast_node_of_node_id(nim,*k) {
			None=>{},
			Some(ast_node)=>{
				match lookup_def_node_of_node(dc,&ast_node,nim,nd) {
					None=>{},
					Some(def_node_id)=>{
						if *k != def_node_id {
							jdm.insert(*k,def_node_id);
						}
					}
				}
			}
		}
	}

	jdm
}


pub static ctxtkey: local_data::Key<@RFindCtx> = &local_data::Key;

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


    let args = os::args();

    let opts = ~[
        optmulti("L"),optflag("d"),optflag("j"),optflag("h"),optflag("i"),optflag("g"),optflag("w"),optflag("f")
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
		println(" set RUST_LIBS for a default library search path");
	};
	if matches.free.len()>0 {
		let mut done=false;
		let filename=get_filename_only(matches.free[0]);
		let dc = @get_ast_and_resolve(&Path(filename), libs);
		local_data::set(ctxtkey, dc);
	
		if (opt_present(&matches,"d")) {
			debug_test(dc);
			done=true;
		} else if (opt_present(&matches,"j")){
			dump_json(dc);
		}
		let mut i=0;
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
			write_source_as_html(dc,rust2html::DefaultOptions);
			println("Creating HTML pages from source.. done");
		}

	}
}

struct HrcNode<NODE> {
	node:NODE,
	child:~[HrcNode<NODE>]
}

/// tags: crate,ast,parse resolve
/// Parses, resolves the given crate
fn get_ast_and_resolve(cpath: &std::path::PosixPath, libs: ~[std::path::PosixPath]) -> RFindCtx {
	


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

/*    let (crate, tycx) = driver::driver::compile_upto(sess, sessopts.cfg.clone(),
                                                     &driver::driver::file_input(cpath.clone()),
                                                     driver::driver::cu_no_trans, None);
*/
	let input=driver::driver::file_input(cpath.clone());
	let cfg= driver::driver::build_configuration(sess, @"", &input);

	let crate1=driver::driver::phase_1_parse_input(sess,cfg.clone(),&input);
	let crate2=driver::driver::phase_2_configure_and_expand(sess,cfg,crate1);
	
	let ca=driver::driver::phase_3_run_analysis_passes(sess,crate2);  
//	let c=crate.unwrap();
//	let t=tycx.unwrap();
    RFindCtx { crate: crate2, tycx: ca.ty_cx, sess: sess, ca:ca }
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

/// todo - collect stage.

fn flatten_to_str<T,U:ToStr>(xs:&[T],f:&fn(x:&T)->U, sep:&str)->~str {
	let mut acc=~"";
	let mut i=0; // TODO - functional way.
	while i<xs.len() {
		if i>0 {acc.push_str(sep);}
		acc.push_str( f(&xs[i]).to_str() );
	
		i+=1;
	}
	acc
}







fn lookup_def_at_file_line_pos_old(dc:&RFindCtx,filepos:&str, show_all:ShowDefMode)->Option<~str> {

	let toks:~[&str]=filepos.split_iter(':').collect();
	if toks.len()<3 { return None }

//	let line:Option<uint> = FromStr::from_str(toks[1]);
	if_some!(line in FromStr::from_str::<uint>(toks[1]) then {
		if_some!(col in FromStr::from_str::<uint>(toks[2]) then {
			//todo - if no column specified, just lookup everything on that line!

			match text_file_pos_to_byte_pos(dc.tycx,&ZTextFilePos::new(toks[0],line-1,col-1)) {
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
	match text_file_pos_to_byte_pos(dc.tycx, tfp) {
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


pub fn dump_methods_of_type(tycx:&ty::ctxt_, type_node_id:ast::NodeId) {
	let ot = tycx.node_types.find(&(type_node_id as uint));
	match ot {
		None=> {}, 
		Some(t)=> {
			for (&k,&method) in tycx.methods.iter() {
				dump!(method.transformed_self_ty, ot);
				if method.transformed_self_ty==Some(*t) {
					dump!(method);
				}
			}
		}
	}
}
fn dump_methods_of_t(tycx:&ty::ctxt_, t:*ty::t_opaque) {
	for (&k,&method) in tycx.methods.iter() {
		dump!(method.transformed_self_ty, t);
		if method.transformed_self_ty==Some(t) {
			dump!(method);
		}
	}

}

fn auto_deref_ty<'a>(t:&'a ty::t_box_)->&'a ty::t_box_ {
	match t.sty {
		ty::ty_box(mt)|ty::ty_ptr(mt)|ty::ty_uniq(mt)|ty::ty_rptr(_,mt)=>{
			ty::get(mt.ty)
		},	
		_=>t
	}
}
 
fn get_struct_def<'a,'b>(tc:&'a ty::ctxt_, struct_node_id:ast::NodeId)->Option<(@ast::item,@ast::struct_def,ast::Generics)> {
	match tc.items.find(&struct_node_id) {
		None=>{None},
		Some(node)=>match *node {
			syntax::ast_map::node_item(item,ref path)=>{
				match item.node {
					ast::item_struct(sd, ref generics)=>Some((item, sd, generics.clone())),
					_=>None
				}
			}
			_=> None
		},
	}
}

fn find_named_struct_field(tc:&ty::ctxt_, struct_node_id:ast::NodeId, field_ident:&ast::ident)->Option<ast::NodeId> {
	match get_struct_def(tc,struct_node_id) {
		None=>None,
		Some((it,sd,ge))=>{
			for f in sd.fields.iter() {
				match f.node.kind {
					ast::named_field(ref ident,vis)=>if *ident==*field_ident {return Some(f.node.id);},
					_=>return None
				}
			}
			None
		}
	}
}
fn some_or_else<T:Clone>(opt:&Option<T>,fallback_value:&T)->T {
	match *opt {
		Some(ref value)=>value.clone(),
		None=>fallback_value.clone()
	}
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

fn lookup_def_node_of_node(dc:&RFindCtx,node:&AstNode, node_spans:&FNodeInfoMap, node_def_node:&HashMap<ast::NodeId,ast::def_id>)->Option<ast::NodeId> {
	
	match *node {
		astnode_expr(e)=>match e.node {
			// handle methods-calls
			ast::expr_method_call(ref id,ref receiver,ref ident,ref ty_params,ref arg_exprs,ref call_sugar)=>{
				let rec_ty_node= astnode_expr(*receiver).ty_node_id();
				let rec_ty_node1= dc.tycx.node_types.find(&(*id as uint));

				match dc.ca.maps.method_map.find(&e.id) {
					None=> {},//logi!("no method map entry for",e.id),
					Some(mme)=>{
						//logi!("Method Map entry for",e.id);
						match mme.origin {
							typeck::method_static(def_id)=> 
								return Some(def_id.node),
							typeck::method_trait(def_id,_,_)=>
								return Some(def_id.node),
							typeck::method_param(mp)=>{
								match dc.tycx.trait_method_def_ids.find(&mp.trait_id) {
									None=>{},
									Some(method_def_ids)=>{
										return Some(method_def_ids[mp.method_num].node)
									}
								}
							}
						}
					}
				}
			},
			// handle struct-fields? "object.field"
			ast::expr_field(ref object_expr,ref ident,ref ty_params)=>{
				// we want the type of the object..
				let obj_ty=dc.tycx.node_types.find(&(object_expr.id as uint));
				let tydef=auto_deref_ty(ty::get(*obj_ty.unwrap()));
				match tydef.sty {
					ty::ty_struct(def,_)=> {
						let node_to_show=find_named_struct_field(dc.tycx, def.node, ident).unwrap_or_default(def.node);
						return Some(node_to_show);//mk_result(dc,m,node_spans,node_to_show,"(struct_field)");
					},
					_=>return None
				}
			}
			_=>{}
		},
		_=>{}

	}

	// handle everything else
	match node.ty_node_id() {
		Some(id) =>{
			let (def_id,opt_info)= def_info_from_node_id(dc,node_spans,id); 
			match opt_info {
				Some(info)=> {
					return Some(def_id);
				},
				_=>{}
			}
		}
		None=> {}
	};
	return None;
}

fn lookup_def_of_node_sub(dc:&RFindCtx,node:&AstNode,m:ShowDefMode,node_spans:&FNodeInfoMap, node_def_node:&HashMap<ast::NodeId,ast::def_id>)->Option<~str> {
	// TODO - cache outside?


	fn mk_result(dc:&RFindCtx,  m:ShowDefMode, node_spans:&FNodeInfoMap, def_node_id:ast::NodeId, extra_str:&str)->Option<~str> {
		match node_spans.find(&def_node_id) {
			None=>None,
			Some(def_info)=>{
				let loc=get_source_loc(dc,def_info.span.lo);
				let def_pos_str=
					loc.file.name + ":"+loc.line.to_str()+": "+
						match m { SDM_LineCol=>loc.col.to_str()+": ", _ =>~"" }+"\n";
				return	match m{
					SDM_Source=>Some(def_pos_str+get_node_source(dc.tycx,node_spans, def_node_id)+"\n"),
					SDM_GeditCmd=>Some("+"+loc.line.to_str()+" "+loc.file.name+" "),
					_ => Some(def_pos_str)
				};

			}
		}
	}
	match lookup_def_node_of_node(dc, node,node_spans, node_def_node) {
		None=>None,
		Some(def_node_id)=>mk_result(dc,m,node_spans,def_node_id, "")
	}
}

fn get_str_at_text_file_pos_len(cx:ty::ctxt, tfp:&ZTextFilePos,len:uint)->~str {
	let a=text_file_pos_len_to_byte_pos(cx, tfp,len);
	match  a  {
		Some((bp_lo,bp_hi))=>get_span_str(cx,
			&codemap::span{lo:bp_lo,hi:bp_hi,expn_info:None}
		),
		None=>~""
	}
}
fn zget_file_line_str(cx:ty::ctxt, filename:&str, src_line:uint)->~str {
//	for c.sess.codemap.files.rev_iter().advance |fm:&codemap::FileMap| {
	let mut i=cx.sess.codemap.files.len();
	while i>0 {	// caution, need loop because we return, wait for new foreach ..in..
		i-=1;
		let fm=&cx.sess.codemap.files[i];
		let filemap_filename:&str=fm.name;	
		if filename==filemap_filename {
			let s=*fm.lines[src_line];
			let e=if (src_line+1)>=fm.lines.len() {
				*fm.start_pos+fm.src.len()
			} else {
				*fm.lines[src_line+1]
			};
		}
	}
	return ~"";
}

fn get_source_loc(dc:&RFindCtx, pos:codemap::BytePos)->codemap::Loc {
	dc.tycx.sess.codemap.lookup_char_pos(pos)
}
fn loc_to_str(loc:codemap::Loc)->~str {
	loc.file.name+":"+loc.line.to_str()+":"+loc.col.to_str()+":"
}

pub fn dump_node_source_for_single_file_only(text:&[u8], ns:&FNodeInfoMap, id:ast::NodeId) {
	match(ns.find(&id)) {None=>logi!("()"),
		Some(info)=>{
			dump_span(text, &info.span);
		}
	}
}

// TODO- this should return a slice?
pub fn get_node_source(c:ty::ctxt, ns:&FNodeInfoMap, id:ast::NodeId)->~str {
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


pub fn def_info_from_node_id<'a,'b>(dc:&'a RFindCtx, node_info:&'b FNodeInfoMap, id:ast::NodeId)->(int,Option<&'b FNodeInfo>) {
	let crate_num=0;
	match dc.tycx.def_map.find(&id) { // finds a def..
		Some(a)=>{
			match get_def_id(crate_num,*a){
				Some(b)=>match b.crate {
					0=>(b.node,node_info.find(&b.node)),
					_ => (id as int, None)
				},
				None=>(id as int,None)
			}
		},
		None=>(id as int,None)
	}
}


// see: tycx.node_types:node_type_table:HashMap<id,t>
// 't'=opaque ptr, ty::get(:t)->t_box_ to resolve it

pub fn dump_ctxt_def_map(dc:&RFindCtx) {
//	let a:()=ctxt.tycx.node_types
	logi!("===Test ctxt def-map table..===");
	for (key,value) in dc.tycx.def_map.iter(){
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
pub fn text_file_pos_len_to_byte_pos(c:ty::ctxt,tfp:&ZTextFilePos,len:uint )->Option<(codemap::BytePos,codemap::BytePos)>

{
//	for c.sess.codemap.files.rev_iter().advance |fm:&codemap::FileMap| {
	let mut i=c.sess.codemap.files.len();
	while i>0 {	// caution, need loop because we return, wait for new foreach ..in..
		i-=1;
		let fm=&c.sess.codemap.files[i];
		let filemap_filename:&str=fm.name;	
		if filemap_filename==tfp.name {
			let line_pos=*fm.lines[tfp.line];
			let bp_start=*fm.lines[tfp.line]+tfp.col;
			let bp_end=(bp_start+len).min(&(*fm.start_pos+fm.src.len()));
			return Some((BytePos(bp_start), BytePos(bp_end)))
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

pub fn build_node_def_node_table(dc:&RFindCtx)->~HashMap<ast::NodeId, ast::def_id>
{
	let mut r=~HashMap::new();
	let curr_crate_id_hack=0;	// TODO WHAT IS CRATE ID REALLY?!
	// todo .. for range(0,c.next_id) || ??
	let mut id:ast::NodeId=0;
	while id<*(dc.tycx.next_id) as ast::NodeId {
		if_some!(t in safe_node_id_to_type(dc.tycx,id as int) then {
			if_some!(def in dc.tycx.def_map.find(&(id as int)) then { // finds a def..
				if_some!(did in get_def_id(curr_crate_id_hack,*def) then {
					r.insert(id as ast::NodeId,did);
				})
			});
		});
		id+=1;
	}
	r
}

pub fn def_node_id_from_node_id(dc:&RFindCtx, id:ast::NodeId)->ast::NodeId {
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
	dump_ctxt_def_map(dc);

	logi!("==== Test node search by location...===");

	// Step a test 'cursor' src_pos through the given source file..
	let mut test_cursor=15 as uint;

	while test_cursor<500 {
		let loc = get_source_loc(dc,BytePos(test_cursor));

		logi!(~"\n=====Find AST node at: ",loc.file.name,":",loc.line,":",loc.col,":"," =========");

		let nodetloc = find_node_tree_loc_at_byte_pos(dc.crate,BytePos(test_cursor));
		let node_info =  get_node_info_str(dc,&nodetloc);
		dump!(node_info);
		println("node ast loc:"+(do nodetloc.map |x| { option_to_str(&x.get_id()) }).to_str());


		if_some!(id in nodetloc.last().ty_node_id() then {
			logi!("source=",get_node_source(dc.tycx, node_info_map,id));
			if_some!(t in safe_node_id_to_type(dc.tycx, id) then {
				println(fmt!("typeinfo: %?",
					{let ntt= rustc::middle::ty::get(t); ntt}));
				dump!(id,dc.tycx.def_map.find(&id));
				});
			let (def_id,opt_info)= def_info_from_node_id(dc,node_info_map,id); 
			if_some!(info in opt_info then{
				logi!("src node=",id," def node=",def_id,
					" span=",info.span.my_to_str());
				logi!("def source=", get_node_source(dc.tycx, node_info_map, def_id));
			})
		})

		test_cursor+=20;
	}

	// test byte pos from file...
	logi!("====test file:pos source lookup====");
	dump!(get_str_at_text_file_pos_len(dc.tycx,&ZTextFilePos::new("test_input.rs",3-1,0),10));
	dump!(get_str_at_text_file_pos_len(dc.tycx,&ZTextFilePos::new("test_input.rs",9-1,0),10));
	dump!(get_str_at_text_file_pos_len(dc.tycx,&ZTextFilePos::new("test_input2.rs",4-1,0),10));
	dump!(get_str_at_text_file_pos_len(dc.tycx,&ZTextFilePos::new("test_input2.rs",11-1,0),10));
	let ospan=text_file_pos_len_to_byte_pos(dc.tycx, &ZTextFilePos::new("test_input2.rs", 10-1,0),32);
	if_some!(x in ospan then {
		let (lo,hi)=x;
		logi!(get_span_str(dc.tycx, &codemap::span{lo:lo,hi:hi,expn_info:None} ));
	});
	dump!(zget_file_line_str(dc.tycx,"test_input2.rs",5-1));
	dump!(zget_file_line_str(dc.tycx,"test_input.rs",9-1));
	
	logi!("\n====test full file:pos lookup====");
	dump!(lookup_def_at_text_file_pos(dc, &ZTextFilePos::new("test_input.rs",8-1,21),SDM_Source));println("");
	dump!(lookup_def_at_text_file_pos(dc, &ZTextFilePos::new("test_input2.rs",3-1,12),SDM_Source));println("");
	dump!(lookup_def_at_text_file_pos(dc, &ZTextFilePos::new("test_input.rs",10-1,8),SDM_Source));println("");
	dump!(lookup_def_at_text_file_pos(dc, &ZTextFilePos::new("test_input.rs",13-1,16),SDM_Source));println("");
	dump!(lookup_def_at_text_file_pos(dc, &ZTextFilePos::new("test_input.rs",11-1,10),SDM_Source));println("");
	
}

fn first_file_name(dc:&RFindCtx)->~str {
	dc.tycx.sess.codemap.files[0].name.to_str() // clone?
}

fn find_file_name_in(dc:&RFindCtx,fname:&str)->Option<~str> {
	// todo subsequence match..
	// TODO - is there an existing way of doing this, "index_of.." ..contains()..?
	for f in dc.tycx.sess.codemap.files.iter() {
		if fname==f.name {return Some(fname.to_owned());}
	}
	None
}

pub fn def_of_symbol_to_str(dc:&RFindCtx, ns:&FNodeInfoMap,ds:&HashMap<ast::NodeId, ast::def_id>,s:&str)->~str {
	~"TODO"	
}

pub fn write_source_as_html(dc:&RFindCtx,opts:uint) {
	let nim=build_node_info_map(dc.crate);
	let ndm = build_node_def_node_table(dc);
	let jdm=build_jump_to_def_map(dc,nim,ndm);
	rust2html::write_source_as_html_sub(dc,nim,ndm,jdm,opts);
}


pub fn rustfind_interactive(dc:&RFindCtx) {
	// TODO - check if RUSTI can already do this.. it would be better there IMO
	let node_spans=build_node_info_map(dc.crate);

	let node_def_node = build_node_def_node_table(dc);
	let mut curr_file=first_file_name(dc);

	loop {
		print("rustfind "+curr_file+">");
		let input_line=io::stdin().read_line();
		let toks:~[&str]=input_line.split_iter(' ').collect();
		if toks.len()>0 {
			match toks[0] {
				"h"|"help"=> print("interactive mode\n - enter file:line:pos or line:pos for current file\n - show location & def of symbol there\n j-dump json q-quit i-info\n"),
				"i"=> {
					println("files in current crate:-\n");
					for x in dc.tycx.sess.codemap.files.iter() {
						println("\t"+x.name);
					}
				}
				"j"=> dump_json(dc),
				"q"=> break,
				_ =>{
					// todo - if you just supply line, lookup defs on that line
					// todo - lookup defs from symbol, remembering context of previous lookups?
					let cmd=toks[0];
					let cmd1=match cmd[0] as char { '0'..'9'=>curr_file+":"+cmd,_=>cmd.to_str() };
					let subtoks:~[&str]=cmd1.split_iter(':').collect();
					curr_file=find_file_name_in(dc, subtoks[0].to_str()).unwrap_or_default(curr_file);
					//dump!(cmd1,subtoks,curr_file);
					let def=lookup_def_at_text_file_pos_str(dc, cmd1,SDM_Source);
					print(def.unwrap_or_default
					(~"no def found\n"));
					//println(def_of_symbol_to_str(dc,node_spans,node_def_node,toks[0]));
				}
			}
		}
	}
}

pub trait MyOption<T> {
	pub fn for_some(&self, f:&fn(t:&T));
	pub fn do_some<R>(&self, f:&fn(t:&T)->R)->Option<R>;
}
impl<T> MyOption<T> for Option<T>{
	pub fn for_some(&self, f:&fn(t:&T)) {
		match self {
			&None=>{},
			&Some(ref t)=>f(t)
		}
	}
	pub fn do_some<R>(&self, f:&fn(t:&T)->R)->Option<R> {
		match self {
			&None=>None,
			&Some(ref t)=>Some(f(t))
		}
	}

}




