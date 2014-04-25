use syntax::codemap;
use syntax::ast;
use syntax::ast::DefId;
use syntax::codemap::Pos;
use rustc::middle::ty;
use std::hash::Hash;
use collections::HashMap;
use std::slice;
use std::cmp;
use std::io;
use std::io::fs;
use codemaput::{ZIndexFilePos,ToZIndexFilePos};
use find_ast_node::{FNodeInfoMap,FNodeInfo};
use rfindctx::{RustFindCtx};
use crosscratemap::{CrossCrateMap};
use jumptodefmap::{JumpToDefMap,JumpToRefMap,MultiMap};
pub use super::NodeMaps;
use rsfind::MyOption;
use timer::Profiler;
use indexpage;

//
//use rust2html::HtmlWriter; TODO- why can't we just do this the recomended way?
use rust2html::htmlwriter::HtmlWriter;

pub mod htmlwriter;	// TODO - this is wrong, will cause confusion!
					// it just doesn't seem to work if we 'mod' in the cratefile
					// and refer to ::htmlwriter::HtmlWriter, or try to 'use' it 


// TODO: See how far we can decouple this from the link generation
// could we have an abstract linked-AST format , and an html renderer for that..
// eg incase this can be spun off as a completely seperate project
// eg: could this be done more efficiently client-side-throw json +javascript decoder at the browser.
// is there an existing cross-language linked-source generator? if not, could one be written? (would it get more traction?)

// TODO: view_item 'mod path::....' could be converted to file reference?

// TODO - how to handle out-of-crate references?
// perhaps we could detect items with def_id crate indices outside the current crate,
// and link to their *nodes* instead of source-lines? or would the spans still work?
// we could give the whole generator a root dir to look for crates? ... and assume html is generated in there..


fn file_get_time_stamp_str(fpath:&Path)->~str {
	let file_time=match fs::stat(fpath) {
		Ok(stat)=>stat.modified as i64, Err(_)=>0_i64
	};
	let ts=::time::at_utc(::time::Timespec::new(file_time/1000,0));
	ts.ctime()
}

/// Takes populated node maps (NodeMaps)&'CrossCrateMap, plus a 'filemap' from crate-analysis, and generates an HTML view of the source with links.
pub fn make_html_from_source(dc: &RustFindCtx, fm: &codemap::FileMap, nmaps: &NodeMaps,
                 fln: &FileLineNodes, lib_path: &str, 
                 out_file: &Path, options: &::RF_Options) -> ~str {
    // todo - Rust2HtmlCtx { fm,nim,jdm,jrm } .. cleanup common intermediates
	let  p=Profiler::new("make_html");
//	::callgraph::dump_callgraph(xcm, nmaps);

    let mut doc= HtmlWriter::new();

	let time_stamp=file_get_time_stamp_str(&Path::new(fm.name.clone()));	
	let git_str = get_git_branch_info();	// yikes, where?! cwd

    let hash=get_str_hash(fm.name);
	source_view_page_begin(&mut doc,out_file,options);


//  let bg=(~[~"383838",~"34383c",~"3c3834",~"383c34",~"343c38",~"38343c",~"3a343a",
//          ~"3a343a",~"36363a",~"363a36",~"3a3636",~"3a3a34",~"3a333a",~"343a3a",~"343a3c",~"343838"])[hash&15];
    // write the doc lines..

    let fstart = fm.start_pos;
    let max_digits=num_digits(fm.lines.borrow().len());

    if options.write_file_path {
        let t0=doc.begin_tag_check("div");//,&[(~"style",~"background-color:#"+bg+";")]);
        let t1=doc.begin_tag_check("fileblock");
        doc.write_path_links(fm.name);
		
		if git_str.len()>0 {doc.begin_tag("c40").writeln("\tgit branch:\t"+git_str).end_tag();}
		doc.begin_tag("c40").writeln("\tmodified:\t"+time_stamp).end_tag();
        doc.end_tag_check(t1);
        doc.end_tag_check(t0);
    }
    {
        let mut scw=SourceCodeWriter::new(&mut doc);
        for line in range(0, fm.lines.borrow().len()) {
            scw.line_index=line;
            // todo: line numbers want to go in a seperate column so they're unselectable..
            scw.doc.write_tagged("ln",pad_to_length((line+1).to_str(),max_digits," "));
            scw.doc.begin_tag_anchor((line+1).to_str()); 
            let lend = if line < (fm.lines.borrow().len()-1) {
                (fm.lines.borrow().get(line+1) - fstart).to_uint()
            } else {
                fm.src.len()
            };
            scw.doc.write(" ");
            let line_str = fm.src.slice((fm.lines.borrow().get(line) - fstart).to_uint(), lend);
            //doc.writeln(line_str);
			
            scw.doc.end_tag();
            
            // this block appears to be what corrupted it - is it anything to do with the iteration?
            
            let depth=scw.doc.depth();
//            let null_line=Vec::new();
            let def_nodes_on_line=fln.def_nodes_per_line.get(line)/*.unwrap_or(&null_line)*/; // caution, because what it returned was Option, it seems.
            for nl in def_nodes_on_line.iter() {
                scw.doc.begin_tag_anchor("n"+nl.to_str());
            }
            scw.doc.write(" "); // TODO, It should be ok to nest these surely.
            for _ in def_nodes_on_line.iter() {
                scw.doc.end_tag();
            }
            scw.doc.check_depth(depth);
            
            
            write_line_with_links(&mut scw,dc,fm,lib_path, nmaps, line_str, fln.nodes_per_line.get(line));
            scw.doc.writeln("");
    //      doc.writeln(markup_line);
        }
    }
    if options.write_references {
        write_symbol_references(&mut doc,dc,fm, lib_path,nmaps, &fln.nodes_per_line,options);
    }

	source_view_page_end(&mut doc,out_file,options);
    doc.doc.into_owned()
}

fn source_view_page_begin(doc:&mut HtmlWriter, out_file:&Path, options:&::RF_Options) {
	// TODO- could RAII this..
    write_head(doc, out_file, options);
    doc.begin_tag("body");
    doc.begin_tag("div");
    doc.begin_tag("maintext");
}
fn source_view_page_end(doc:&mut HtmlWriter, _:&Path, _:&::RF_Options)
{
    doc.end_tag();//maintext
    doc.end_tag();//div
    doc.end_tag();//body
}

pub fn write_crate_as_html_sub(dc:&RustFindCtx, nmaps:&NodeMaps,
	lib_path:&str, options: &::RF_Options) {

	//println!("output dir={}",options.output_dir.as_str().unwrap_or(""));
	indexpage::write_index_html(&Path::new("."), &[~"rs",~"cpp",~"h",~"c"],options);

    let npl=NodesPerLinePerFile::new(dc,nmaps.node_info_map);

//  let nspl=~[~[]];

    let files=&dc.codemap().files.borrow();
    for (i,cm_file) in files.iter().enumerate() {
        if is_valid_filename(cm_file.name) {
            let html_name = options.output_dir.join(Path::new(make_html_name(cm_file.name)));
            println!("generating {}: {}.. ", i.to_str(), html_name.display());
            let doc_str=make_html_from_source(dc, &**cm_file, nmaps,npl.file.get(i) , lib_path,
                                  &html_name, options);

			file_write_bytes_as(&html_name, doc_str.as_bytes() );
        }
    }

    // TODO -copy all resources to the output folder
	// for the minute we're just copying the compiled-in default.
	fs::mkdir(&Path::new("css"),io::UserDir);
	file_write_bytes_as(&Path::new("css/sourcestyle.css"), g_default_css);
}

pub fn file_write_bytes_as(file_path:&Path, data:&[u8]) {
	// todo - file_write_as<T>(,&[T]);
	match fs::File::create(file_path) {
		Err(_)		=> println!("cant write {}",file_path.as_str().unwrap()),
		Ok(mut f)	=> {f.write(data);},
	}
}

static g_default_css:&'static [u8]=include_bin!("resources/css/sourcestyle.css");

fn is_valid_filename(f:&str) ->bool{
    match f.chars().nth(0) {
        Some(c)=> match c{
            '<' => false,
            _=>true
        },
        None => false
    }
}

pub fn write_head(doc:&mut HtmlWriter, out_file: &Path, options: &::RF_Options) {
    let css_rel_path = &options.output_dir
                        .path_relative_from(&out_file.dir_path())
                        .unwrap_or(Path::new(""));
    fn write_css_link (doc: &mut HtmlWriter, css_rel_path: &Path, stylesheet_name: &str) {
        let path = match css_rel_path.with_filename(stylesheet_name).as_str() {
            Some(s) => s,
            None => stylesheet_name
        }.to_owned();
        doc.write_tag_ext("link", &[(~"href", path), (~"rel", ~"stylesheet"), (~"type", ~"text/css")]);
    };

    doc.begin_tag("head");
    write_css_link(doc, css_rel_path, "css/shCore.css");
    write_css_link(doc, css_rel_path, "css/shThemeDefault.css");
    write_css_link(doc, css_rel_path, "css/sourcestyle.css");
    doc.end_tag();
}

/// spawns git to find version info for the source tree here.
pub fn get_git_branch_info()->~str {
	use std::io;
	use std::io::process;
	use std::os;
	use std::str;
	use std::io::pipe;

	match process::Process::output("git",&[~"branch",~"-v"]) {
		Err(_)=>{},
		Ok(out)=> {
			let  curr_branch=~"";
			for line in str::from_utf8(out.output.as_slice()).unwrap_or("").lines() {
				if line.chars().nth(0).unwrap_or('\0')=='*' { return line.to_owned();}
			}
		},
	}
	return ~"";
}


fn get_str_hash(s:&str)->uint{
    let mut acc=0;
    for x in s.chars() { acc=((acc<<5)-acc)^(acc>>12); acc+=x as uint;}
    acc
}



fn num_digits(a:uint)->uint{
    let mut n=1;
    let mut aa=a;
    while aa>=10 { aa/=10; n+=1;}
    n
}
fn pad_to_length(a:&str,l:uint,pad:&str)->~str {
    let mut acc=StrBuf::from_str(" ");
    let mut i=(l-a.len()) as int;
    while i>0 {
        acc.push_str(pad);
        i-=1;//pad.len() as int;
    }
    acc.push_str(a);
    acc.into_owned()
}

/// Struct to accumulate nodes sorted for each line of the file.
pub struct FileLineNodes {
    nodes_per_line:Vec<Vec<ast::NodeId>>,
    def_nodes_per_line:Vec<Vec<ast::NodeId>>
}
/// Struct to accumulate nodes sorted for each line of the file.
struct NodesPerLinePerFile {
    file :Vec<FileLineNodes>
}
//type NodesPerLine=&[~[ast::NodeId]];

pub fn get_file_index(dc:&RustFindCtx,fname:&str)->Option<uint> {
    // todo - functional
    let mut index=0;
    let files = dc.codemap().files.borrow();
    while index<files.len() {
        if fname == files.get(index).name {
            return Some(index);
        }
        index+=1;
    }
    None
}
pub fn get_crate_name(dc:&RustFindCtx,ci:ast::CrateNum)->~str {
    super::codemaput::get_crate_name(dc.tycx_ref(),ci)
}


impl NodesPerLinePerFile {
    fn new(dc:&RustFindCtx, nim:&FNodeInfoMap)->~NodesPerLinePerFile {
        // todo, figure this out functionally?!
        //      dc.sess.codemap.files.map(
        //              |fm:&@codemap::FileMap|{ slice::from_elem(fm.lines.len(), ~[]) }
        //          ).collect();

        let mut npl=~NodesPerLinePerFile{file:Vec::new()};
//      let mut fi=0;
//      npl.file = slice::from_elem(dc.sess.codemap.files.len(),);
        let files = dc.codemap().files.borrow();
        for cmfile in files.iter() {
//      while fi<dc.sess.codemap.files.len() {
//          let num_lines=dc.sess.codemap.files[fi].lines.len();
            let num_lines=cmfile.lines.borrow();
            let num_lines = num_lines.len();
            npl.file.push(FileLineNodes{
                nodes_per_line: Vec::from_elem(num_lines,Vec::new()),
                def_nodes_per_line: Vec::from_elem(num_lines,Vec::new())
            });
//          fi+=1;
        };
        for (k,v) in nim.iter() {
            // TODO- only want the **DEF_NODES** for 'def_nodes_per_line', not all.
            // todo, this could be more direct, file index, line index, ...
            v.rf_span().lo.to_index_file_pos(dc.tycx_ref()).for_some(|ifp|{
                match v.rf_span().hi.to_index_file_pos(dc.tycx_ref()) {
                    None=>{ },
                    Some(ifpe)=>{

                        let  f = npl.file.get_mut(ifp.file_index as uint);
                        f.def_nodes_per_line.get_mut(ifp.line as uint).push(*k);
//                      dump!(ifp, f.nodes_per_line.len());
                        for li in range(ifp.line,ifpe.line+1) {
                            if li < f.nodes_per_line.len() as u32 {
//                              dump!(li, *k)
                                f.nodes_per_line.get_mut(li as uint).push(*k)
                            };
                        };
                    }
                };
            });
        };
//      npl.dump();
        npl
    }
    #[allow(dead_code)]
    fn dump(&self) {
        for f in self.file.iter() {
            print!("file \\{");
            for l in f.nodes_per_line.iter() {
                print!("line \\{{}", l.len().to_str());
                for n in l.iter() {
                    print!("{},", n.to_str());
                }
                print!("\\} line");
            }
            print!("\\}file");
        }
    }
}
pub fn node_color_index(ni:&FNodeInfo)->int {
    // TODO - map this a bit more intelligently..
	match ni.rf_kind() {
        NK_Fn=>1,
        NK_Add|NK_Sub|NK_Mul|NK_Div|NK_Assign|NK_Eq|NK_Le|NK_Gt|NK_Ge|NK_Ne|NK_BinOp|NK_AssignOp
        |NK_BitAnd|NK_BitXor|NK_BitOr|NK_Shl|NK_Shr|NK_Not|NK_Neg|NK_Box|NK_Uniq|NK_Deref|NK_AddrOf
            =>5,
        NK_De=>3,
        NK_TypeParam=>7,
        NK_Ty=>8,
        NK_StructField|NK_Field=>24,
        NK_Path=>26,
        NK_Call=>27,
        NK_Variant=>28,
        NK_MethodCall=>10,
        NK_Lit=>12,
        NK_Stmt=>13,
        NK_Mod=>38,
        NK_Local=>16,
        NK_Pat=>20,
        NK_Block|NK_FnBlock=>22,
        NK_Method|NK_TyMethod=>18,
        NK_Tup=>14,
        NK_Arm=>11,
        NK_Index=>13,
        NK_VStore=>16,
        NK_Mac=>10,
        NK_Struct=>31,
        NK_Trait=>32,
        NK_Impl=>33,
        NK_Enum=>34,
        NK_Keyword|NK_While|NK_Match|NK_Loop|NK_Do|NK_Cast|NK_If|NK_Return|NK_Unsafe|NK_Extern|NK_Crate|NK_As|NK_In|NK_For=>21,
        _ =>1
	}
}
pub fn color_index_to_tag(i:int)->~str {
    "c"+i.to_str()
}
fn is_alphanumeric(c:char)->bool {
    match c {
        'a'..'z'|'A'..'Z'|'0'..'9'|'_'=>true,
        _=>false
    }
}
fn is_alphanumeric_u8(c:u8)->bool {
    match c as char{
        'a' ..'z'|'A'..'Z'|'0'..'9'|'_'=>true,
        _=>false
    }
}
type ColorIndex=int;
/// todo .. option type!!!
fn is_text_here(line:&str, pos:uint,reftext:&str, color:ColorIndex)->Option<(ColorIndex,uint)> {
    if pos+reftext.len()+2>=line.len() { return None}
    if pos>0 {
        if is_alphanumeric(line[pos-1] as char) { return None }// must be word start
    }
    if is_alphanumeric(line[pos+reftext.len()] as char) { return None }// must be word start
    let mut i=0;
    while i<reftext.len() {
        if reftext[i]!=line[i+pos] {return None}
        i+=1;
    }

    return Some((color,reftext.len()));
}

fn sub_match(line:&str,x:uint,opts:&[&str])->Option<(uint,uint)>{
    let smp=x;
    if is_alphanumeric_u8(line[smp])==false{ return None;} // not alpha
    let mut opti=0;
    if line.is_char_boundary(x)==false { return None; }
    for o in opts.iter() {
        if (o.len()+x)>line.len() { continue; } // not enough chars in line
        if (o.len()+x+1)<line.len() {
            if is_alphanumeric_u8(line[o.len()+x]) { continue; }// not word boundary for this one
        }

        let ex=o.len()+x;
        if line.is_char_boundary(ex) {
            if line.slice(x,ex) == o.slice(0,o.len()) {
                return Some((opti,o.len()));
            }
        }
        opti+=1;

    }
    return None;
}


/// interface for emitting source code serially
/// carries state between emitting lines
struct SourceCodeWriter<'a, T> {
    doc: &'a mut T,
    multiline_comment_depth:int,
    brace_depth:int,
    bracket_depth:int,
    angle_bracket_depth:int,
    in_string:int,
    line_index:uint
}

impl<'a, T> SourceCodeWriter<'a,T> {
    fn new(d:&'a mut T)->SourceCodeWriter<'a, T> {
        SourceCodeWriter{
            doc:d, multiline_comment_depth:0, brace_depth:0,bracket_depth:0, angle_bracket_depth:0, in_string:0, line_index:0
        }
    }
}

static no_link:i64  =0 ;
static link_to_refs:bool    =true;
static link_debug:bool      =true;


fn write_line_with_links(dst:&mut SourceCodeWriter<HtmlWriter>,dc:&RustFindCtx,fm:&codemap::FileMap,lib_path:&str, nmaps:&NodeMaps, line:&str, nodes:&Vec<ast::NodeId>) {
    // todo ... BREAK THIS FUNCTION UP!!!!
    // and there is a load of messy cut paste too.


    let mut link:Vec<i64> = Vec::from_elem(line.len(),0 as ast::NodeId as i64);
    let mut color:Vec<int> = Vec::from_elem(line.len(),0 as int);
    let mut depth:Vec<uint> = Vec::from_elem(line.len(),0x7fffffff as uint);

    for node in nodes.iter() {

        match nmaps.node_info_map.find(node) {
            None=>{},
            Some(node_info)=>{
                // link_id >0 = node def link. link_id<0 = node ref link

                let link_id= match nmaps.jump_def_map.find(node) {
                    None=>
                        if link_to_refs{
                            if nmaps.jump_ref_map.find(*node).len()>0 {
                                - *node as i64
                            } else {
                                no_link
                            }
                        }else{
                            no_link
                        },
                    Some(x) => if link_debug{(x.node as i64)|(x.krate as i64<<48)} else {x.node as i64}
                };


                let os=node_info.rf_span().lo.to_index_file_pos(dc.tycx_ref());
                let oe=node_info.rf_span().hi.to_index_file_pos(dc.tycx_ref());
                if os.is_some() && oe.is_some() {
                    let e=oe.unwrap(); let s=os.unwrap();
                    let d = node_info.rf_span().hi.to_uint() - node_info.rf_span().lo.to_uint();    // todo - get the actual hrc node depth in here!
                    let xs = if dst.line_index <= s.line as uint {
                        s.col
                    } else {
                        0
                    };
                    // TODO: instead of this brute force 'painters-algorithm',
                    // clip spans, or at least have a seperate "buffer" for the line-coherent infill
                    // for multi-line spans

                    // 'paint' the nodes we have here.
                    //if (s.line==e.line)
                    let xe = if e.line as uint > dst.line_index {
                        line.len()
                    }else{
                        e.col as uint
                    };
                    let ci = node_color_index(node_info);
                    for x in range(xs as uint, cmp::min(xe, line.len())) {
                        if d <= *depth.get(x) {
                            *color.get_mut(x)=ci;
                            *depth.get_mut(x) = d;
                            if link_id!=0 {
                                *link.get_mut(x)=link_id;
                            }
                        }
                        if *link.get(x)==0 { *link.get_mut(x)=link_id;}
                    }
                }
            }
        }
    }


    // paint comments out, mark delimiter symbols--override what we get from buggy tree picture...
    // TODO ... need to figure out tree nodes encompasing the current line from above to
    // propogate information properly eg brackets inside a type ..

    {
        // handle decls, todo - combine with keywords..
        let mut x;
        let mut wb;
        //attributes/"preprocessor"
        if *line.as_bytes().get(0).unwrap() as char =='#' {
            for x in range(0,line.len()) { *color.get_mut(x)=50 }
        }
        // delimiters/whitespace/special chars characters - disable links & override some coloring
	// todo vec char or iterator utf8
        x=0;
        while x < line.len() {
            let c0=line[x] as char;

            match c0 {
                '{'|'}'|'['|']'|';'|',' => {*color.get_mut(x)=3; *link.get_mut(x)=0; },
                '('|')'=> {*color.get_mut(x)=4;*link.get_mut(x)=0;},
                _=>{}
            }
            if x<(line.len()-1) {
                let c1=*line.as_bytes().get(x+1).unwrap() as char;
                if (c0=='-' || c0=='=') && c1=='>' {
                    *color.get_mut(x)=4;*color.get_mut(x+1)=4; *link.get_mut(x)=0; *link.get_mut(x+1)=0;
                    x+=1;
                }
            }

            x+=1;
        }
        x=0; wb=true;
        while x<(line.len()) {
            // override color for top level decls
            if wb && is_alphanumeric(line[x] as char){
                let (decl_color,len)=
                                        is_text_here(line,x,"fn",30)
                    .unwrap_or( is_text_here(line,x,"struct",31)
                    .unwrap_or( is_text_here(line,x,"trait",32)
                    .unwrap_or( is_text_here(line,x,"impl",33)
                    .unwrap_or( is_text_here(line,x,"enum",34)
                    .unwrap_or( is_text_here(line,x,"type",35)
                    .unwrap_or( is_text_here(line,x,"static",36)
                    .unwrap_or( is_text_here(line,x,"macro_rules!",37)
                    .unwrap_or( is_text_here(line,x,"mod",38)
                    .unwrap_or( is_text_here(line,x,"class",39)
                    .unwrap_or((0,0)))))))))));
                if decl_color>0{
                    for x in range(x,x+len) { *link.get_mut(x)=0;/* clear link on the keyword part..*/}
                    let mut in_typaram=0;
                    while (x<line.len()) && (line[x] as char)!='{' && (line[x] as char)!='('{
                        in_typaram+=match line[x] as char {'<'=>1,_=>0};
                        *color.get_mut(x)=if in_typaram==0{decl_color}else{5};
                        in_typaram+=match line[x] as char {'>'=>-1,_=>0};
                        x+=1;
                    }
                }
            } else {
                wb= !is_alphanumeric_u8(line[x]);
            }
            x+=1;
        }
                // paint keywords out, these dont seem to come through as distinct ast nodes - as they can be differnt parts of a node
        x=0; wb=true;
        while x<line.len() {
            if wb {
                match sub_match(line,x,&[&"let",&"mut", &"const", &"use", &"mod", &"match",&"if",&"else",&"break",&"return",&"while",&"loop",&"for",&"do",&"ref",&"pub",&"priv",&"unsafe",&"extern",&"in",&"as",&"crate"]) {
                None=>{},
                Some((_, len))=>{
                    let me=x+len;
                    while x<me { *color.get_mut(x)=21; *link.get_mut(x)=0;x+=1; }
                    continue;
                    }
                }
            }
            if is_alphanumeric_u8(line[x]) {wb=false}
            x+=1;
        }

        // paint comments out, & disable links
        // TODO - multiline comments, feed in/out..
        x=0;
        while x<(line.len()) {
            if x<line.len()-1 {
                if line[x+0]=='/' as u8 && (line[x+1]=='/' as u8|| line[x+1]=='*' as u8) || dst.multiline_comment_depth>0{
                    let mut comment_color=40;
                    let mut slc=false;
                    if line[x]=='/' as u8 && line[x+1]=='*' as u8 {dst.multiline_comment_depth+=1;} else {slc=true;}
                    if dst.multiline_comment_depth>0 {comment_color=41;}
                    if x<line.len()-2 { if line[x+2]as char=='/' { comment_color=42 }}// doc-comments are a bit brightest
                    while x<line.len() && (slc || dst.multiline_comment_depth>0){
                        *color.get_mut(x)=comment_color;
                        *link.get_mut(x)=0;

                        x+=1;
                        if line[if x>=2{x-2}else{0}]=='*' as u8 && line[x-1]=='/' as u8 {dst.multiline_comment_depth-=1;}
                    }
                }
            }
            x+=1;
        }
    }
    let resolver=|x|resolve_link(x,dc,fm,lib_path, nmaps);
    write_line_attr_links(dst,line,&color,&link, resolver );
}

fn resolve_link(link:i64, dc:&RustFindCtx,fm:&codemap::FileMap,lib_path:&str, nmaps:&NodeMaps)->Option<~str> {
    if link !=no_link {
		// If this node is a definition, we write a link to references block(todo-page)-or TODO rustdoc page.
		// link to refs block with link = neg(node_id)
        if (link as i32)<0{
			symbol_refs_link_str(dc,fm, lib_path, nmaps, -((link as i32) as u32)) 
        } else
        {
            let def_crate = (link>>48) as u32;
            let def_node=(link&((1<<48)-1)) as u32;
			make_def_link_str(dc,fm,lib_path,nmaps, &DefId{krate:def_crate,node:def_node})
        }
    } else {
		None
    }
}

fn make_def_link_str(dc:&RustFindCtx, fm:&codemap::FileMap, lib_path:&str,nmaps:&NodeMaps,  defid:&DefId )->Option<~str>{
	let files = dc.codemap().files.borrow();

	match nmaps.rf_find_source(defid) {
		// link to another crate - we use the node index, because we haven't got its'
		// line table. ?! TODO: dont we hve the line table in 'crosscratemap'

		Some(a) if defid.krate>0 =>{
			Some(make_html_name_reloc(a.file_name,fm.name,lib_path)+
				"#n"+defid.node.to_str())
		},
		// Local crate link:
		Some(a)=>
		{
			match get_index_file_pos(nmaps.node_info_map, defid.node, dc.tycx_ref()) {
				Some(pos)=>{
					let files = dc.codemap().files.borrow();
					Some(make_html_name_rel(files.get(pos.file_index as uint).name, fm.name) +
						"#" + (pos.line + 1).to_str())
				},
				// Broken link. However, write out the create & node index for debug.
				// its probably in a macro expansion, 
				// TODO, can we link to macro invocation site?
				None=>None
			}
		}
		None=>None
	}
}

fn symbol_refs_link_str(dc:&RustFindCtx, fm:&codemap::FileMap, lib_path:&str, nmaps:&NodeMaps, id:u32)->Option<~str>
{
	let refs = nmaps.jump_ref_map.find(id);
	// if (num_refs >1) link to a refs page ... else just go to the ref..
	match refs.len() {
		0=> None,
		1=> make_def_link_str(dc, fm, lib_path,nmaps, 
				&DefId{krate:0,node:*(refs.iter().nth(0).unwrap())  }),
				// todo -can we do this without unwrap, and without double-testing it.
				// The pattern is, collection.map_either(for_one_item,  for_many_items)
		_=>{
			let ifp= get_index_file_pos(nmaps.node_info_map,id, dc.tycx_ref()).unwrap();
			Some("#line"+(ifp.line+1).to_str()+"_col"+ifp.col.to_str()+"_refs")
		}
	}
}


fn write_line_attr_links(dst:&mut SourceCodeWriter<HtmlWriter>,text_line:&str,color:&Vec<int>,links:&Vec<i64>, resolve_link: |i64| -> Option<~str>) {
    // emit a span..
    let no_color=-1;
    let mut curr_col=no_color;
    let mut curr_link=no_link;
    //let mut outp=HtmlWriter::new();
    let tag_depth=dst.doc.depth();
    assert!(text_line.len()==color.len() && text_line.len()==links.len());

    for x in range(0,text_line.len()) {
        // if state changed...
        if (curr_link,curr_col)!=(*links.get(x),*color.get(x)) {
            if curr_link !=no_link {dst.doc.end_tag();}
            if curr_col !=no_color {dst.doc.end_tag();}

            curr_col = *color.get(x);
            curr_link=*links.get(x);
            if curr_col !=no_color {
                dst.doc.begin_tag(color_index_to_tag(curr_col));
            }
            if curr_link !=no_link {
				let link=resolve_link(*links.get(x));
				match link {
					Some(link_target)=>{dst.doc.begin_tag_link(link_target);},
					None=> {dst.doc.begin_tag("nop");},
				}
            }
        }
        dst.doc.write_u8_(text_line[x]);
    }
    if curr_col !=no_color {dst.doc.end_tag();}
    if curr_link !=no_link {dst.doc.end_tag();}
    assert!(tag_depth==dst.doc.depth());
}

/// gather all the nodes within the file specified by 'FileMap'
fn find_defs_in_file(fm:&codemap::FileMap, nim:&FNodeInfoMap)->Vec<ast::NodeId> {
    // todo - functional way..
    let mut acc=Vec::new();
    for (n,info) in nim.iter() {
        if info.rf_span().lo >= fm.start_pos && (info.rf_span().lo < (fm.start_pos+codemap::BytePos(fm.src.len() as u32))) {
            acc.push(*n);
        }
    }
    acc
}



// TODO: find nodes of enclosing context.
// 'this function, called from these locations'
// 'this function, called fromm these functions ... <<< BETTER
// 'this type, used in these functions ... '
/// Get a line from sourcecode, but step past items and blank lines. return the line and next line index
fn is_whitespace(c:&char)->bool {
	match *c {
		' '|'\t'|'\n'=>true,
		_=>false
	}
}
fn get_source_line_filtered(fm:&codemap::FileMap, line_index:uint)-> (~str, uint) {
	
	let mut i=line_index;
	while i < num_source_lines(fm) {
		let line_str = get_source_line(fm, i);
		if line_str.chars().nth(0).unwrap_or('\0')!='#' {// not a 'lang item'
			if line_str.chars().filter(|x|!is_whitespace(x)).len()>0{ // not all whitespace
				return (line_str,i);
			}
		};
		i+=1;
	}
	return (~"",i);
}
#[test]
fn test_whitespace() {
	let str0="foo bar baz";
	let str1="\t   \t";
	let str2="\t foo\t";
	assert!(str0.chars().filter(|x|!is_whitespace(x)).len()>0)
	assert!(str1.chars().filter(|x|!is_whitespace(x)).len()==0)
	assert!(str2.chars().filter(|x|!is_whitespace(x)).len()>0)
}

fn get_source_line(fm:&codemap::FileMap, i: uint) -> ~str {

    let lines = fm.lines.borrow();
    let le = if (i as uint) < (lines.len() - 1) {
        lines.get(i as uint + 1).to_uint()
    } else {
        (fm.src.len() as uint) + fm.start_pos.to_uint()
    };
//  dump!(fm.lines[i-1],*fm.start_pos, fm.lines[i-1]-le);
    if i > 0 {
        fm.src.slice((lines.get(i as uint) - fm.start_pos).to_uint(), (le - fm.start_pos.to_uint()) as uint).to_owned()
    } else {
        ~""
    }
}
fn num_source_lines(fm:&codemap::FileMap)->uint {
    let lines = fm.lines.borrow();
	lines.len()
}

//fn split_by_key<T,K>(src:&[T],f:&fn(t:&T)->K)->(K,[&T])] {
//}
#[deriving(Clone)]
pub struct Extents<T> {
    lo:T, hi:T
}

macro_rules! min(
    ($left:expr, $right:expr) => (
		{	let a=$left.clone(); let b=$right.clone(); // evaluate once
        	if a < b {
	            a.clone()
	        } else  {
	            b.clone()
	        }
        }
	)
)
macro_rules! max(
    ($left:expr, $right:expr) => (
        {	let a=$left.clone(); let b=$right.clone(); // evaluate once
        	if a > b {
	            a.clone()
	        } else  {
	            b.clone()
	        }
        }
    )
)

impl<T:Ord+Clone> Extents<T> {
    pub fn new(lo:&T,hi:&T)->Extents<T> { Extents{lo:lo.clone(),hi:hi.clone()} }
    pub fn new_from_value(v:&T)->Extents<T>{ Extents {lo:v.clone(),hi:v.clone()} }
    pub fn contains(&self, other:&Extents<T>)->bool {
        other.lo >= self.lo && other.hi <=self.hi
    }
    pub fn intersection(&self,other:&Extents<T>)->Option<Extents<T>> {
        if self.overlaps(other) {
            Some(Extents{lo: min!(self.lo, other.lo), hi: max!(self.hi, other.hi)})
        } else {
            None
        }
    }
    pub fn include(&self,other:&Extents<T>)->Extents<T> {
        Extents{lo: min!(self.lo, other.lo), hi: max!(self.hi, other.hi)}
    }
    pub fn overlaps(&self, other:&Extents<T>)->bool {
        !(other.lo >= self.hi || other.hi <= self.lo)
    }
    pub fn contains_val(&self, value:&T)->bool {
        *value >= self.lo && *value <= self.hi
    }
    pub fn include_val(&self, value:&T)->Extents<T> {
        Extents { lo: min!(self.lo, *value), hi: max!(self.hi, *value) }
    }
}

fn ref_page_file_name(fm:&codemap::FileMap, nmaps:&NodeMaps,options:&::RF_Options, node_id:ast::NodeId)
{
	// todo-  we might write a page per symbol
	let path=options.output_dir.join(
			Path::new(
				make_html_name(
					fm.name.to_str()+"_n"+node_id.to_str()+"_refs")
				)
			);

//	println!("ref_page_name would be {}",path.as_str());

}

/// Write a block of links to symbol references.
/// Workaround for not having popup menus when you click on a symbol.
fn write_symbol_references(doc:&mut HtmlWriter,dc:&RustFindCtx, fm:&codemap::FileMap, _:&str,  nmaps:&NodeMaps, _: &Vec<Vec<ast::NodeId>>,options:&::RF_Options) {

	// Todo - from all references, collect file links. 'file ... references <files..>, referenced by<files..>'

	// TODO:
	// Make a page showing the references of other modules's symbols?
	// 

    let depth=doc.depth();
    doc.begin_tag_ext("div",[(~"class",~"refblock")]);

    let file_def_nodes = find_defs_in_file(fm,nmaps.node_info_map);

    for &def_node in file_def_nodes.iter() {
        let opt_def_info = nmaps.node_info_map.find(&def_node);
        if !opt_def_info.is_some() {continue;}
        let def_info = opt_def_info.unwrap();
//        if !(def_info.rf_kind()==~"fn" || def_info.rf_kind()==~"struct" || def_info.rf_kind()==~"trait" || def_info.rf_kind()==~"enum" || def_info.rf_kind()==~"ty") { continue; }
		match def_info.rf_kind() {
			NK_Fn|NK_Struct|NK_Trait|NK_Enum|NK_Ty=>{}// use these cases, else continue.
			_=> continue,
		}	// TODO: Include StructField ,Variant here, and include get refs of their sub elements..

        let refs = nmaps.jump_ref_map.find(def_node);
        let max_verbose_links=20;   // todo - sort..
        let max_short_links=60; // todo - sort..

		// If there's more than one reference we write a block of references.
		// TODO - it should be a seperate page. symbol -> symbol refs; pad out that page with additional 
        if refs.len()>1 {
			doc.writeln("");

			ref_page_file_name(fm,nmaps,options, def_node);
			doc.write_refs_header(dc,  nmaps,  fm,def_node);
			doc.writeln_tagged("c43","references:- "+refs.len().to_str());

            let opt_def_tfp = def_info.rf_span().lo.to_index_file_pos(dc.tycx_ref());
            if !opt_def_tfp.is_some() { continue;}
            let def_file_pos=opt_def_tfp.unwrap();
            let mut links_written=0 as uint;

            let mut curr_file=def_file_pos.file_index;
            let mut refs_iter=refs.iter()
                .filter(|&id|{nmaps.node_info_map.find(id).is_some()})
                .map(|&id|{
                    let oni=nmaps.node_info_map.find(&id); assert!(oni.is_some());
                    let ni=oni.unwrap();
                    let oifp=ni.rf_span().lo.to_index_file_pos(dc.tycx_ref());
                    assert!(oifp.is_some()); let ifp=oifp.unwrap();
                    (ni,ifp,id)})
                .collect::<~[(&FNodeInfo,ZIndexFilePos,ast::NodeId)]>();

            let l=refs_iter.len();
            fn pri_of(x:&FNodeInfo) -> uint { 
				match x.rf_kind() {
					NK_Impl=>0,
					NK_Ty=>1,
					NK_Trait=>2,
					NK_Struct=>3,
					_=>0x8000
                }
            }
            // todo: we want to sort based on node type to find impls, but we dont quite find what we want..
            refs_iter.mut_slice_to(l - 1).sort_by(
				|&(ni1, ref ifp1, _), &(ni2, ref ifp2, _)| {
					use std::cmp::{Less, Greater};
	                if ((ifp1.file_index - curr_file) & 0x7fff) as uint +
	                        pri_of(ni1) <= ((ifp2.file_index - curr_file) & 0x7fff) as uint +
	                        pri_of(ni2) {
	                    Less
	                } else {
	                    Greater
	                }
		        }
			);
            let mut newline=true;
            let mut last_link_line=0;

			let num_links=refs_iter.len();
			let lines_per_link = match refs_iter.len() {
				x if x<10=>5,
				x if x<20=>3,
				x if x<30=>1,
				_=>0,
			};

			for &(_, ref ref_ifp,ref id) in refs_iter.iter() {
                if *id!=def_node {

                    // Write a reference to the file, if we're looking a new file now
                    if curr_file!=ref_ifp.file_index {
                    	last_link_line=!0; // invalid value 
                        if newline==false {doc.writeln("");}
                        curr_file=ref_ifp.file_index;
                        doc.write_file_ref(dc,fm,curr_file as uint);
                        newline=true;
                    }
					// too many to show, just show filenames. 
					// TODO: if too many filenames, just show modules
					if lines_per_link==0 {continue;}	

					let files = dc.codemap().files.borrow();
					let rfm = &files.get(ref_ifp.file_index as uint);
					let tagname=make_html_name_rel(rfm.name, fm.name) + "#" + (ref_ifp.line + 1).to_str();
					let this_link_lines_shown=0;

                    if lines_per_link>0 {
                        if newline==false {
							
						}

                       	// Display a line from the referenced location, but
						// step past any #[lang items] - we want to show a meaningful item
						// also show some context, N lines behind, N lines ahead, like grep does..
					//	let  mut ref_line_index = ref_ifp.line as uint;
						// prefer signed numbers,we're dealing with offsets, they dont want to wrapround..
						let lines_of_context:int=1;//(lines_per_link-1)/2;
						let mut ref_line_index = ::std::cmp::max(last_link_line+1 as int, ref_ifp.line as int-lines_of_context);
						let end_line = ref_ifp.line as int + lines_of_context;
						// if we skipped anything, write a seperator
						if ref_line_index>last_link_line+1 as int && (last_link_line>0) {
							doc.writeln("--");
						}

						// todo ... make a set of the lines to show, make an expanded set around them..
						doc.begin_tag_link(tagname);
						while ref_line_index<=end_line {
							// todo - we want to highlight the line of the definition, but
							// we need to account for if we skipped it..
							let (src_line,i)=get_source_line_filtered(&***rfm, ref_line_index as uint); 
							if (i as int)<=end_line {
								doc.write_tagged(if i as u32==ref_ifp.line{&"c41"}else{&"c40"},(i+1).to_str()+&": ");
								doc.writeln_tagged(if i as u32==ref_ifp.line{&"c1"}else{&"c2"}, src_line);
								last_link_line=i as int;
							}
							ref_line_index = i as int+1;
							newline=true;
							
						}
						doc.end_tag();

					} else {

                       	if  num_links<200 {
	                        doc.begin_tag_link(tagname);
                            doc.write_tagged("c40","("+(ref_ifp.line+1).to_str()+")");
   	                        newline=false;
   	                        links_written+=1;
   	                        last_link_line=ref_ifp.line as int+1;
	                        doc.end_tag();
                        }
                    }
                }
            }
        }
    }
    doc.end_tag();
    doc.check_depth(depth);
}


impl ::rust2html::htmlwriter::HtmlWriter{ // todo, why doesn't that allow path reuse
	// todo - i dont think these are really methods of 'htmlwriter'. 
    fn write_refs_header(&mut self,dc:&RustFindCtx,nmaps:&NodeMaps, fm:&codemap::FileMap, node_id:ast::NodeId) {
    	let depth=self.depth();
        self.writeln("");
        nmaps.node_info_map.find(&node_id).for_some( |info| {
			// Get the extents of this node in the file.. TODO: a ranged filepos, surely?
            let oifp=info.rf_span().lo.to_index_file_pos(dc.tycx_ref());//.unwrap();
            let oifpe=info.rf_span().hi.to_index_file_pos(dc.tycx_ref());//.unwrap();
            if (oifp.is_some() && oifpe.is_some())==true {
                let node_file_pos=oifp.unwrap();
                let node_end_file_pos=oifp.unwrap();
    //      let def_info=nim.find(&nid).unwrap();
    //      let ifpe=get_node_index_file_pos(dc		,nim,nid).unwrap();

                self.begin_tag_anchor("line"+(node_file_pos.line+1).to_str()+"_col"+node_file_pos.col.to_str() + "_refs" );
                self.begin_tag("c43");
                self.writeln(dc.codemap().files.borrow().get(node_file_pos.file_index as uint).name + ":" + (node_file_pos.line + 1).to_str() + ":" + node_file_pos.col.to_str()
                            +"-"+(node_end_file_pos.line+1).to_str()+":"+node_file_pos.col.to_str() +" -" +info.rf_kind().as_str() + "- definition:");
                self.end_tag();

                self.begin_tag_link( "#"+(node_file_pos.line+1).to_str());
                self.begin_tag("pr");
            //          dump!(def_tfp);
				let (linestr1,l1)=get_source_line_filtered(fm,node_file_pos.line as uint);
				let (linestr2,l2)=get_source_line_filtered(fm,l1 as uint+1);
				let (linestr3,_)=get_source_line_filtered(fm,l2 as uint+1);
                self.writeln(linestr1 );
                self.writeln(linestr2 );
                self.writeln(linestr3 );
                self.end_tag();
                self.end_tag();
                self.end_tag();
            }
        });
        self.check_depth(depth);
    }

    fn write_file_ref(&mut self, dc:&RustFindCtx,origin_fm:&codemap::FileMap, fi:uint) {
        let fname = dc.codemap().files.borrow();
        let fname = fname.get(fi).name.as_slice();
        self
            .begin_tag_link( make_html_name_rel(fname,origin_fm.name));
        self
            .begin_tag("c40")
            .writeln(""+fname + ":")
            .end_tag();
        self
            .end_tag();
    }

    pub fn write_path_links(&mut self/*doc:&mut HtmlWriter*/, file_name:&str) {
    	let pldepth=self.depth();
        self.writeln("");
        let file_path_col=&"c0";
        let file_delim_col=&"c1";
        let name_parts = file_name.split('/').collect::<~[&str]>();
        let num_dirs=name_parts.len()-1;
        let mut link_target=StrBuf::from_str("./");


        for _ in range(0,num_dirs) {link_target.push_str("../");}
        self.write("    ");
        let t0=self.begin_tag_link(link_target.as_slice()+"index.html").depth(); self.write("(index<- )"); self.end_tag_check(t0); self.write("    ").depth();
        let t1=self.begin_tag_link(link_target.as_slice()).depth(); self.write("    ./"); self.end_tag_check(t1);

        for (i,x) in name_parts.iter().enumerate() {
            let is_dir = i < num_dirs;
            link_target.push_str(*x);
            if is_dir {link_target.push_str("/");}
            else { link_target.push_str(".html");}
            
            let fpc=self.begin_tag(file_path_col).depth();
            let fpl=self.begin_tag_link(link_target.as_slice()).depth(); self.write(*x);
            self.end_tag_check(fpl);/*link*/
			self.end_tag_check(fpc);/*file_path_col*/
            if is_dir {self.write_tagged(file_delim_col,"/");}
        }
        self.writeln("");
        self.writeln("");
        self.check_depth(pldepth);
    }
}

// TODO: a span index should uniquely identify the node.
// This adress form is a reasonable compromise between things we can see,
// things that are robust when some source changes, etc.
// file_index:line_index:col_index:length


fn get_index_file_pos(nim:&FNodeInfoMap, nid: ast::NodeId, tc:&ty::ctxt)->Option<ZIndexFilePos> {
	match nim.find(&nid) {
		Some(ni)=>ni.rf_span().lo.to_index_file_pos(tc),
		None=>None
	}
}


fn make_html_name(f: &str) -> ~str { 
    f + ".html"
}

fn count_chars_in(f:&str, x:char)->uint{
    let mut n=0;
    for c in f.chars() { if c==x {n+=1} }
    n
}


fn make_html_name_reloc(f:&str, origin:&str, reloc:&str)->~str {
    let mut acc=StrBuf::from_str("");
    if reloc.len()>0 {
//		printf("relocating path to %s"+str);
        acc=StrBuf::from_str(reloc);
        if acc.as_slice().chars().last().unwrap()!='/' { acc.push_char('/');}
    }else {
        for _ in range(0,count_chars_in(origin,'/')) {
            acc.push_str("../");
        }
    }
    acc.push_str(f);
    make_html_name(acc.as_slice()).to_owned()
}
fn make_html_name_rel(f:&str, origin:&str)->~str {
    make_html_name_reloc(f,origin,"")
}



