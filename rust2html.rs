use syntax::codemap;
use syntax::ast;
use ioutil::*;
use htmlwriter::*;
use std::hashmap::*;
use std::vec;
use std::str;
use extra::sort;
use codemaput::*;
use find_ast_node::*;
use rfindctx::*;
use syntax::*;

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

pub static WriteFilePath:uint	=0x0001;
pub static WriteReferences:uint	=0x0002;
pub static DefaultOptions:uint 	=WriteFilePath | WriteReferences;
// todo: options struct.

pub fn make_html(dc:&RFindCtx, fm:&codemap::FileMap,nim:&FNodeInfoMap,jdm:&JumpToDefMap, jrm:&JumpToRefMap,xcm:&::CrossCrateMap, fln:&FileLineNodes, lib_path:&str,options:uint)->~str {
	// todo - Rust2HtmlCtx { fm,nim,jdm,jrm } .. cleanup common intermediates
	
	let mut doc= HtmlWriter::new::();
	write_head(&mut doc);
	write_styles(&mut doc, fm.name);

	let hash=get_str_hash(fm.name);
	let bg=(~[~"383838",~"34383c",~"3c3834",~"383c34",~"343c38",~"38343c",~"3a343a",
			~"3a343a",~"36363a",~"363a36",~"3a3636",~"3a3a34",~"3a333a",~"343a3a",~"343a3c",~"343838"])[hash&15];
	// write the doc lines..
	doc.begin_tag_ext("body",&[(~"style",~"background-color:#"+bg+";")]);
	doc.begin_tag("maintext");
	let mut line=0;
	let fstart = *fm.start_pos;
	let max_digits=num_digits(fm.lines.len());
	
	if options & WriteFilePath!=0 {
		write_path_links(&mut doc,fm.name);
	}

	let mut multiline_comment_depth=0;	//todo: general purpose state object.
	while line<fm.lines.len() {
		// todo: line numbers want to go in a seperate column so they're unselectable..
		doc.write_tagged("ln",pad_to_length((line+1).to_str(),max_digits," "));
		doc.begin_tag_anchor((line+1).to_str());
		let lend=if line<(fm.lines.len()-1){*fm.lines[line+1]-fstart}else{fm.src.len()};
		doc.write(" ");
		let line_str=fm.src.slice(*fm.lines[line]-fstart,lend);
		//doc.writeln(line_str);
		doc.end_tag();
		for nl in fln.def_nodes_per_line[line].iter() {
			doc.begin_tag_anchor("n"+nl.to_str());
		}
		doc.write(" "); // TODO, It should be ok to nest these surely.
		for nl in fln.def_nodes_per_line[line].iter() {
			doc.end_tag();
		}
		multiline_comment_depth=
			write_line_with_links(&mut doc,dc,fm,lib_path, nim, jdm,jrm,xcm, line_str, fln.nodes_per_line[line],line, multiline_comment_depth);
		doc.writeln("");
//		doc.writeln(markup_line);
		line+=1;
	}
	if options & WriteReferences!=0{
		write_references(&mut doc,dc,fm,lib_path,nim,jdm,jrm, fln.nodes_per_line);
	}
	
	doc.end_tag();
	doc.end_tag();

	doc.doc
}

pub fn write_source_as_html_sub(dc:&RFindCtx, nim:&FNodeInfoMap,ndn:&HashMap<ast::NodeId,ast::def_id>, jdm:&JumpToDefMap,xcm:&::CrossCrateMap,lib_path:&str, options:uint) {
	
	let npl=NodesPerLinePerFile::new(dc,nim);

//	let nspl=~[~[]];
	let mut def2refs = ~MultiMap::new();
	for (&nref,&ndef) in jdm.iter() {
		if ndef.crate==0 {
			def2refs.insert(ndef.node,nref);
		}
	}

	// ew
	let mut fi=0;
	for fm in dc.sess.codemap.files.iter() {
		println("generating "+make_html_name(fm.name)+"..");
		let doc_str=make_html(dc, *fm, nim,jdm, def2refs,xcm, &npl.file[fi] , lib_path,options);
		fileSaveStr(doc_str,make_html_name(fm.name));
		fi+=1;
	}
}

fn write_head(doc:&mut HtmlWriter) {
	
	doc.begin_tag("head");
	doc.write_tag_ext("link",&[(~"href",~"css/shCore.css"),(~"rel",~"stylesheet"),(~"type",~"text/css")]);
	doc.write_tag_ext("link",&[(~"href",~"css/shThemeDefault.css"),(~"rel",~"stylesheet"),(~"type",~"text/css")]);
	doc.end_tag();
}

fn get_str_hash(s:&str)->uint{
	let mut acc=0;
	for x in s.iter() { acc=((acc<<5)-acc)^(acc>>12); acc+=x as uint;}
	acc 
}
pub fn write_styles(doc:&mut HtmlWriter,fname:&str){
	// write the styles..
	doc.begin_tag_ext("style",&[(~"type",~"text/css")]);
	doc.write_html("maintext {color:#f0f0f0; font-size:12px; font-family:\"Courier New\"}\n");
	doc.write_html("a:link{ color:#f0f0f0; font-style:normal;   text-decoration:none;}\n");
	doc.write_html("a:visited{ color:#f0f0f0; font-style:normal;   text-decoration:none;}\n");
	doc.write_html("a:link:hover{ color:#f0f0f0; font-style:normal; background-color:#606060; }\n");
	doc.write_html("pr{font-weight:bold}\n");
	doc.write_html("ln{color:#606060; -moz-user-select:-moz-none; -khtml-user-select:none; -webkit-user-select:none; -ms-user-select:none; user-select:none;}\n");
//	doc.write_html("c25{color:#ffffff; opacity:0.92}\n");
	doc.write_html("c26{color:#ffffff; font-weight:bold; }\n");
	doc.write_html("c27{color:#b0ffff; font-weight:bold; }\n");
	doc.write_html("c28{color:#b0ffb0; font-weight:bold; }\n");
	doc.write_html("c29{color:#d0e0ff; font-weight:bold; }\n");
	doc.write_html("c30{color:#fff0e0; font-weight:bold; }\n");
	doc.write_html("c31{color:#d0b0ff; font-weight:bold; }\n");
	doc.write_html("c1{color:#ffffc0;   font-weight:bold; }\n");
	doc.write_html("c33{color:#e0a0d0;  }\n");
	doc.write_html("c40{color:#ffffff; font-style:italic; opacity:0.4}\n");
	doc.write_html("c41{color:#ffffff; font-style:italic; opacity:0.5}\n");
	doc.write_html("c42{color:#ffffff; font-style:italic; opacity:0.6}\n");
	doc.write_html("c2{color:#60f0c0}\n");
	doc.write_html("c3{color:#50e0ff; }\n");
	doc.write_html("c4{color:#f090f0}\n");
	doc.write_html("c5{color:#50ff80; }\n");
	doc.write_html("c6{color:#f0f0e0}\n");
	doc.write_html("c7{color:#fff0d0}\n");
	doc.write_html("c8{color:#e0d0f0}\n");
	doc.write_html("c9{color:#70f0f0}\n");
	doc.write_html("c10{color:#f0f070}\n");
	doc.write_html("c11{color:#c0f070}\n");
	doc.write_html("c12{color:#70c0f0}\n");
	doc.write_html("c13{color:#c0f070}\n");
	doc.write_html("c14{color:#f0ffc0}\n");
	doc.write_html("c15{color:#f0f0e0}\n");
	doc.write_html("c16{color:#c0ffe0}\n");
	doc.write_html("c17{color:#90d0f0}\n");
	doc.write_html("c18{color:#f0a0d0}\n");
	doc.write_html("c19{color:#d0f0a0}\n");
	doc.write_html("c20{color:#0f0ff}\n");
	doc.write_html("c21{color:#d0d0d0; font-weight:bold}\n");
	doc.write_html("c22{color:#c0ffd0; }\n");
	doc.write_html("c23{color:#d0f0ff; }\n");

	doc.end_tag();
}


fn num_digits(a:uint)->uint{
	let mut n=1;
	let mut aa=a;
	while aa>=10 { aa/=10; n+=1;}
	n
}
fn pad_to_length(a:&str,l:uint,pad:&str)->~str {
	let mut acc=~" ";
	let mut i=(l-a.len()) as int;
	while i>0 {
		acc.push_str(pad);
		i-=1;//pad.len() as int;
	}
	acc.push_str(a);
	acc
}

fn change_file_name_ext(file_name:&str,new_ext:&str)->~str {
	let parts:~[&str]=file_name.split_iter('.').collect();
	parts[0]+"."+new_ext
}

struct FileLineNodes {
	nodes_per_line:~[~[ast::NodeId]],
	def_nodes_per_line:~[~[ast::NodeId]]
}
struct NodesPerLinePerFile {
	file :~[FileLineNodes]
}
//type NodesPerLine=&[~[ast::NodeId]];

pub fn get_file_index(dc:&RFindCtx,fname:&str)->Option<uint> {
	// todo - functional
	let mut index=0;
	while index<dc.sess.codemap.files.len() {
		if fname==dc.sess.codemap.files[index].name  { return Some(index);}
		index+=1;
	}
	None
}
pub fn get_crate_name(dc:&RFindCtx,ci:ast::CrateNum)->~str {
	::get_crate_name(dc.tycx,ci)
}


impl NodesPerLinePerFile {
	fn new(dc:&RFindCtx, nim:&FNodeInfoMap)->~NodesPerLinePerFile {
		// todo, figure this out functionally?!
		//		dc.sess.codemap.files.map(
		//				|fm:&@codemap::FileMap|{ vec::from_elem(fm.lines.len(), ~[]) }
		//			).collect();

		let mut npl=~NodesPerLinePerFile{file:~[]};
		let mut fi=0;
//		npl.file = vec::from_elem(dc.sess.codemap.files.len(),);
		while fi<dc.sess.codemap.files.len() {
			let num_lines=dc.sess.codemap.files[fi].lines.len();
			npl.file.push(FileLineNodes{
				nodes_per_line:from_elem(num_lines,~[]),
				def_nodes_per_line:from_elem(num_lines,~[])
			});
			fi+=1;
		};
		for (k,v) in nim.iter() {
			// TODO- only want the **DEF_NODES** for 'def_nodes_per_line', not all. 
			// todo, this could be more direct, file index, line index, ...
			do byte_pos_to_index_file_pos(dc.tycx,v.span.lo).for_some|ifp|{
				match byte_pos_to_index_file_pos(dc.tycx,v.span.hi) {
					None=>{	},
					Some(ifpe)=>{
						
						let mut f=&mut npl.file[ifp.file_index];
						f.def_nodes_per_line[ifp.line].push(*k);
//						dump!(ifp, f.nodes_per_line.len());
						for li in range(ifp.line,ifpe.line+1) {
							if li <f.nodes_per_line.len() {
//								dump!(li, *k)
								f.nodes_per_line[li].push(*k)
							};
						};
					}
				};
			}
		};
//		npl.dump();
		npl
	}
	fn dump(&self) {
		for f in self.file.iter() {
			print("file {");
			for l in f.nodes_per_line.iter() {
				print("line {" + l.len().to_str());
				for n in l.iter() {
					print(n.to_str()+",");
				}
				print("");
				print("line {");
			}
			print("}file");
		}
	}
}
pub fn node_color_index(ni:&FNodeInfo)->int {
	// TODO - map this a bit more intelligently..
	match ni.kind {
		~"fn"=>1,
		~"add"|~"sub"|~"mul"|~"div"|~"assign"|~"eq"|~"le"|~"gt"|~"ge"|~"ne"|~"binop"|~"assign_op"
		|~"bitand"|~"bitxor"|~"bitor"|~"shl"|~"shr"|~"not"|~"neg"|~"box"|~"uniq"|~"deref"|~"addr_of"
			=>2,
		~"ty"=>3,
		~"de"=>4,
		~"type_param"=>5,
		~"struct_field"|~"field"=>6,
		~"keyword"|~"while"|~"match"|~"loop"|~"do"|~"cast"|~"if"|~"return"|~"unsafe"|~"extern"|~"as"|~"in"|~"for"=>7,
		~"path"=>8,
		~"call"=>9,
		~"method_call"=>10,
		~"enum"=>11,
		~"lit"=>12,
		~"stmt"=>13,
		~"mod"=>14,
		~"struct"=>23,
		~"local"=>16,
		~"impl"=>17,
		~"trait"=>28,
		~"pat"=>20,
		~"block"|~"blk"|~"fn_block"=>22,
		~"method"|~"type_method"=>18,
		~"tup"=>4,
		~"arm"=>11,
		~"index"=>13,
		~"vstore"=>16,
		~"mac"=>10,

		_ =>0
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

fn sub_match(mut line:&str,x:uint,opts:&[&str])->Option<(uint,uint)>{
	let mut smp=x;
	if is_alphanumeric_u8(line[smp])==false{ return None;} // not alpha
	let mut opti=0;
	if line.is_char_boundary(x)==false { return None; }
	for o in opts.iter() {
		if (o.len()+x)>line.len() {loop;} // not enough chars in line
		if (o.len()+x+1)<line.len() {
			if is_alphanumeric_u8(line[o.len()+x]) {loop;}// not word boundary for this one
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

fn write_line_with_links(outp:&mut HtmlWriter,dc:&RFindCtx,fm:&codemap::FileMap,lib_path:&str, nim:&FNodeInfoMap,jdm:&JumpToDefMap, jrm:&JumpToRefMap,xcm:&::CrossCrateMap, line:&str, nodes:&[ast::NodeId],line_index:uint, mut multiline_comment_depth:int)->int {

/*			match line[x] as char{
			' ' =>"&nbsp;",
			'<' =>"&lt;",
			'>' =>"&gt;",
			'&' =>"&amp;",
			'\t' =>"&nbsp;&nbsp;&nbsp;&nbsp;",
			c=>str::from_char(c as char)
			}
*/


	let node_infos=nodes.map(|id|{nim.find(id)});
//	for x in node_infos.iter() { println(fmt!("%?", x));}
//	dump!(node_infos);
// todo - sorting node spans, not this "painters-algorithm" approach..

	let mut link:~[ast::NodeId] = vec::from_elem(line.len(),0 as ast::NodeId);
	let mut color:~[int] = vec::from_elem(line.len(),0 as int);
	let mut depth:~[uint]= vec::from_elem(line.len(),0x7fffffff as uint);
	let mut rndcolor=0;
	let mut no_link=0 as ast::NodeId;;

	let link_to_refs=false;
	let link_debug=true;
	for n in nodes.iter() {

		match nim.find(n) {
			None=>{},
			Some(ni)=>{
				// link_id >0 = node def link. link_id<0 = node ref link
				let link_id= match jdm.find(n) {
					None=> if link_to_refs{if jrm.find(*n).len()>0 { - *n } else { no_link }}else{no_link},
					Some(x) =>if link_debug{x.node|(x.crate<<24)} else {x.node}
				};
				
				
				let os=byte_pos_to_index_file_pos(dc.tycx, ni.span.lo);
				let oe=byte_pos_to_index_file_pos(dc.tycx, ni.span.hi);
				if os.is_some() && oe.is_some() {
					let e=oe.unwrap(); let s=os.unwrap();
					let d=*ni.span.hi-*ni.span.lo;	// todo - get the actual hrc node depth in here!
					let mut x=if line_index<=s.line {s.col}else{0};
					// TODO: instead of this brute force 'painters-algorithm',
					// clip spans, or at least have a seperate "buffer" for the line-coherent infill
					// for multi-line spans

					// 'paint' the nodes we have here.
					//if (s.line==e.line) 
					let xe = if e.line>line_index{line.len()}else{e.col};
					let ci = node_color_index(ni);
					while x<xe && x<line.len() {
						if d < depth[x] {
							color[x]=ci;
							depth[x]=d;
							if link_id!=0 {
								link[x]=link_id;
							}
						}
						if link[x]==0 { link[x]=link_id;}
						x+=1;
					}
					rndcolor+=1;
				}
			}
		}
	}


	// paint comments out, mark delimiter symbols--override what we get from buggy tree picture...
	// TODO ... need to figure out tree nodes encompasing the current line from above to 
	// propogate information properly eg brackets inside a type ..

	{
		// handle decls, todo - combine with keywords..
		let mut x=0; let mut wb=true;
		//attributes/"preprocessor"
		x=0;
		if line[0]as char =='#' {
			for x in range(0,line.len()) { color[x]=33 }
		}
		// delimiters/whitespace/special chars characters - disable links & override some coloring
		x=0; wb=false;
		while x<(line.len()) {
			let c0=line[x] as char;

			match c0 {
				' '|'\t'|'+'|'-'|'|'|':'|'*'|'&'|'\''|'/'|'@'|'~'|'^'|'%'|'$'|'!'|'>'|'<'|'.'|'#'=> {link[x]=0;}
				'{'|'}'|'['|']'|';'|',' => {color[x]=4;	link[x]=0; },
				'('|')'=> {color[x]=25;link[x]=0;},
				_=>{}
			}
			if x<(line.len()-1) {
				let c1=line[x+1] as char;
				if ((c0=='-' || c0=='=') && c1=='>') {
					color[x]=4;color[x+1]=4; link[x]=0; link[x+1]=0;
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
					is_text_here(line,x,"type",31)
					.unwrap_or_default(is_text_here(line,x,"struct",27)
					.unwrap_or_default(is_text_here(line,x,"trait",28)
					.unwrap_or_default(is_text_here(line,x,"impl",30)
					.unwrap_or_default(is_text_here(line,x,"enum",29)
					.unwrap_or_default(is_text_here(line,x,"fn",26)
					.unwrap_or_default((0,0)))))));
				if decl_color>0{
					for x in range(x,x+len) { link[x]=0;/* clear link on the keyword part..*/}
					let mut in_typaram=0;
					while (x<line.len()) && (line[x] as char)!='{' && (line[x] as char)!='('{
						in_typaram+=match line[x] as char {'<'=>1,_=>0};
						color[x]=if in_typaram==0{decl_color}else{5};
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
				match sub_match(line,x,&[&"let",&"mut", &"const", &"use", &"mod", &"match",&"if",&"else",&"break",&"return",&"while",&"loop",&"for",&"do",&"ref",&"pub",&"priv",&"unsafe",&"extern",&"in",&"as"]) {
				None=>{},
				Some((ix,len))=>{
					let me=x+len;
					while x<me { color[x]=21; link[x]=0;x+=1; }
					loop
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
				if line[x+0]=='/' as u8 && (line[x+1]=='/' as u8|| line[x+1]=='*' as u8) || multiline_comment_depth>0{
					let mut comment_color=40;
					let mut slc=false;
					if line[x]=='/' as u8 && line[x+1]=='*' as u8 {multiline_comment_depth+=1;} else {slc=true;}
					if multiline_comment_depth>0 {comment_color=41;}
					if x<line.len()-2 { if line[x+2]as char=='/' { comment_color=42 }}// doc-comments are a bit brightest
					while x<line.len() && (slc || multiline_comment_depth>0){
						color[x]=comment_color;
						link[x]=0;
						
						x+=1;
						if line[if x>=2{x-2}else{0}]=='*' as u8 && line[x-1]=='/' as u8 {multiline_comment_depth-=1;}
					}
				}
			}
			x+=1;
		}
	}

	// emit a span..
	let mut x=0;
	let no_color=-1;
	let mut curr_col=no_color;
	let mut curr_link=0;
	//let mut outp=HtmlWriter::new();
	let tag_depth=outp.tag_stack.len();
	while x<line.len() {
		// link overrides color..
		if curr_link!=link[x] {
			if curr_link!=no_link {outp.end_tag();}
			if curr_col!=no_color {outp.end_tag();}
			curr_col=no_color;
			curr_link=link[x];

			if link_debug==false {
				if curr_link!=no_link {
					if curr_link>0 /* value is link node index*/ {
						match nim.find(&curr_link) {
							None=>curr_link=no_link,	// link outside the crate?
							Some(link_node_info)=>{
								let oifp = byte_pos_to_index_file_pos(dc.tycx, link_node_info.span.lo);
								match oifp {
									Some(ifp)=>{
										let link_str="#"+(ifp.line+1).to_str();
										outp.begin_tag_link(make_html_name_rel(dc.sess.codemap.files[ifp.file_index].name,fm.name)+link_str);
									},
									None=>{
										// out of crate def node?
	//									def_node = ;
										outp.begin_tag_link("#n"+curr_link.to_str());
									}
								}
							}
						}
					} else if curr_link<0/* link to refs block,value is -(this node index)*/{
						let ifp= get_node_index_file_pos(dc,nim,-curr_link).unwrap();
						
						let ref_block_link_str="#"+(ifp.line+1).to_str()+"_"+ifp.col.to_str()+"_refs";
						outp.begin_tag_link(ref_block_link_str);
					}
				}
			} else {
				// debug - link just to node-id..
				if curr_link!=no_link {
					let def_crate = curr_link>>24;
					let def_node=curr_link&((1<<24)-1);
					let link_str=match xcm.find(&ast::def_id{crate:def_crate,node:def_node}) {
						None=>//"#n"+def_node.to_str(), by node linnk
						{
							match get_node_index_file_pos(dc,nim,def_node) {
								Some(ifp)=>make_html_name_rel(dc.sess.codemap.files[ifp.file_index].name,fm.name)+
									"#"+(ifp.line+1).to_str(),
								None=>{curr_link=no_link;~""}
							}
							
						},
						Some(a)=>{
							make_html_name_reloc(a.fname,fm.name,lib_path)+
//							"../gplsrc/rust/src/"+a.fname+".html"+
							"#n"+def_node.to_str()
						}
					};
					if curr_link!=no_link {
						outp.begin_tag_link(link_str);
					}
				}
			}
		}
		if curr_col!=color[x] {
			if curr_col!=no_color {
				outp.end_tag();
			}
			curr_col=color[x];
			if curr_col!=no_color {
				outp.begin_tag(color_index_to_tag(curr_col));
			}
		}


//		let cstr=str::from_char(line[x] as char);
//		outp.write(cstr);

		outp.write_u8_(line[x]);

/*			match line[x] as char{
			' ' =>"&nbsp;",
			'<' =>"&lt;",
			'>' =>"&gt;",
			'&' =>"&amp;",
			'\t' =>"&nbsp;&nbsp;&nbsp;&nbsp;",
			c=>str::from_char(c as char)
			}
		);
*/

/*
		outp.write(
			match line[x] as char {
			' '=>"&nbsp;",
			'<'=>"&lt;",
			'>'=>"&gt;",
			'&'=>"&amp;",
			'\t'=>"&nbsp;&nbsp;&nbsp;&nbsp;",
			c=>cstr.slice(0,1)//line.slice(x,x+1)
			}
		);
*/
		/*
		let chstr=line.slice(x,x+1);
		if chstr==" " {
			outp.write("&nbsp;");
		} else if (chstr=="\t") {
			outp.write("&nbsp;&nbsp;&nbsp;&nbsp;");
		}
		else {
			outp.write(chstr);
		}
		*/
		x+=1;
	}
	if curr_col!=no_color {outp.end_tag();}
	if curr_link!=no_link {outp.end_tag();}
	assert!(tag_depth==outp.tag_stack.len());
	//todo - we prefered this functional, "build a line, assemble lines"..
	// but we hacked it this way because of plain text->translated text issue. < > &lt; &gt; etc
	// need to split doc writerinterface into functions that take plain text and html fragments
	//outp.doc
	multiline_comment_depth
}

fn find_defs_in_file(fm:&codemap::FileMap, nim:&FNodeInfoMap)->~[ast::NodeId] {
	// todo - functional way..
	let mut acc=~[];
	for (n,info) in nim.iter() {
		if info.span.lo >= fm.start_pos && (info.span.lo < (fm.start_pos+codemap::BytePos(fm.src.len()))) {
			acc.push(*n);
		}
	}
	acc
}

/// K:[V]  insert(K, V) for many V;  find(K)->[V]
pub struct MultiMap<K,V> {
	next_index:uint,
	indices:HashMap<K,uint>,
	items:~[~[V]],
	empty:~[V]
}
impl<'self,K:IterBytes+Eq,V> MultiMap<K,V> {
	pub fn new()->MultiMap<K,V> {
		MultiMap{ next_index:0, indices:HashMap::new(), items:~[], empty:~[] }
	}
	pub fn find(&'self self, k:K)->&'self~[V] {
		// TODO - return iterator, not collection
		match self.indices.find(&k) {
			None=>&self.empty,
			Some(&ix)=>&self.items[ix]
		}
	}
	pub fn insert(&'self mut self, k:K,v:V) {
		let ix=match self.indices.find(&k) {
			None=>{ self.indices.insert(k,self.next_index); self.next_index+=1; self.items.push(~[]); self.next_index-1},
			Some(&ix)=> ix
		};
		self.items[ix].push(v);
	}
}

type JumpToRefMap = MultiMap<ast::NodeId, ast::NodeId>;

// TODO: find nodes of enclosing context.
// 'this function, called from these locations' 
// 'this function, called fromm these functions ... <<< BETTER
// 'this type, used in these functions ... '

fn get_source_line(fm:&codemap::FileMap, i:uint)->~str {

	let le=if i<(fm.lines.len()-1) { *fm.lines[i+1] } else {fm.src.len()+*fm.start_pos};
//	dump!(fm.lines[i-1],*fm.start_pos, fm.lines[i-1]-le);
	if i>=0 {
		fm.src.slice( *fm.lines[i]-*fm.start_pos, le-*fm.start_pos).to_owned()
	} else {
		~""
	}
}

//fn split_by_key<T,K>(src:&[T],f:&fn(t:&T)->K)->(K,[&T])] {
//}

fn write_references(doc:&mut HtmlWriter,dc:&RFindCtx, fm:&codemap::FileMap,lib_path:&str, nim:&FNodeInfoMap,jdm:&JumpToDefMap,jrm:&JumpToRefMap, nodes_per_line:&[~[ast::NodeId]]) {
	doc.write_tag("div");
	let file_def_nodes = find_defs_in_file(fm,nim);
	//let mut defs_to_refs=MultiMap::new::<ast::NodeId, ast::NodeId>();

	for &dn in file_def_nodes.iter() {
		let opt_def_info = nim.find(&dn);
		if !opt_def_info.is_some() {loop;}
		let def_info = opt_def_info.unwrap();
		if !(def_info.kind==~"fn" || def_info.kind==~"struct" || def_info.kind==~"trait" || def_info.kind==~"enum" || def_info.kind==~"ty") {loop}

		let refs = jrm.find(dn);
		let max_links=30;	// todo - sort..
		let max_short_links=60;	// todo - sort..

		
		if refs.len()>0 {
			let mut header_written=false;
			let opt_def_tfp = byte_pos_to_index_file_pos(dc.tycx,def_info.span.lo);
			if !opt_def_tfp.is_some() { loop;}
			let def_tfp=opt_def_tfp.unwrap();
			let mut links_written=0 as uint;

			// sort references by file [(file_index, [...])]

//			let s2=refs_by(|x|{ let 
//			let rf=refs.map(|x|{  })
//			let refs1:~[&ast::NodeId]=refs.iter().filter(|&x|{nim.find(x).is_some()}).collect();
//			let  refs1:() = refs.iter().filter(|&x|{true});	

//			let  refs1:~[int] = refs.iter().filter(|x|{true}).collect();	
//			dump!(refs1);
			// just cannot get filter working here :(


			let mut curr_file=def_tfp.file_index;
			let  mut refs2=refs.iter()
				.filter(|&id|{nim.find(id).is_some()})
				.map(|&id|{
					let oni=nim.find(&id); assert!(oni.is_some());
					let ni=oni.unwrap();
					let oifp=byte_pos_to_index_file_pos(dc.tycx, ni.span.lo);
					assert!(oifp.is_some()); let ifp=oifp.unwrap();
					(ni,ifp,id)})
				.to_owned_vec();

			let l=refs2.len();
			fn pri_of(x:&FNodeInfo)->uint{ if &"impl"==x.kind{0} else {0x8000} }
			// todo: we want to sort based on node type to find impls, but we dont quite find what we want..
			sort::qsort(refs2,0,l-1, |&(ni1,ref ifp1,_),&( ni2,ref ifp2,_)|{ ((ifp1.file_index-curr_file)&0x7fff)+pri_of(ni1)<=((ifp2.file_index-curr_file)&0x7fff)+pri_of(ni2) });
			let mut newline=true;
			for &(ref ref_info,ref ref_ifp,ref id) in refs2.iter() {
				if *id!=dn {
					//let opt_ref_info = nim.find(r);
					//if !opt_ref_info.is_some() {loop;}
					//let ref_info = opt_ref_info.unwrap();
					//let opt_ref_tfp = byte_pos_to_index_file_pos(dc.tycx,ref_info.span.lo);
					//if !opt_ref_tfp.is_some() {loop;}
					//let ref_tfp=opt_ref_tfp.unwrap();

					if header_written==false {					
						if newline==false {doc.writeln("");}
						header_written=true;
						write_refs_header(doc,dc,nim,fm,dn);
						doc.begin_tag("c24"); 
						doc.writeln("references:-");
						doc.end_tag();
						newline=true;
					};
					if curr_file!=ref_ifp.file_index {
						if newline==false {doc.writeln("");}
						curr_file=ref_ifp.file_index;
						write_file_ref(doc,dc,fm,curr_file);
						newline=true;
					}

					if links_written<(max_short_links+max_links) {
						if  links_written<max_links {
							if newline==false {doc.writeln("");newline=true;}
						}
						let rfm=&dc.sess.codemap.files[ref_ifp.file_index];
						doc.begin_tag_link( make_html_name_rel(rfm.name,fm.name)+"#"+(ref_ifp.line+1).to_str());

						if  links_written<max_links {
							doc.begin_tag("c24"); 
							doc.write((ref_ifp.line+1).to_str()+&": ");
							doc.end_tag();
							doc.writeln(get_source_line(dc.sess.codemap.files[ref_ifp.file_index],ref_ifp.line));
							newline=true;
							links_written+=1;
						} else {
							doc.begin_tag("c24"); 
							doc.write("("+(ref_ifp.line+1).to_str()+")");
							doc.end_tag();
							newline=false;
							links_written+=1;
						}
						doc.end_tag();
					}
				}
			}
			if links_written < refs2.len() { 
				doc.begin_tag("c24").writeln(".."+(refs2.len()-links_written).to_str()+"more..").end_tag();
			}
			if header_written {doc.writeln("");}
	//		info=nim.find(dn);
		}
	}

	fn write_refs_header(doc:&mut HtmlWriter,dc:&RFindCtx,nim:&FNodeInfoMap, fm:&codemap::FileMap, nid:ast::NodeId) {

		doc.writeln("");
		do nim.find(&nid).for_some |info| {
			let oifp=byte_pos_to_index_file_pos(dc.tycx, info.span.lo);//.unwrap();
			let oifpe=byte_pos_to_index_file_pos(dc.tycx, info.span.hi);//.unwrap();
			if (oifp.is_some() && oifpe.is_some())==true {
				let ifp=oifp.unwrap();
				let ifpe=oifp.unwrap();
	//		let def_info=nim.find(&nid).unwrap();
	//		let ifpe=get_node_index_file_pos(dc,nim,nid).unwrap();

				doc.begin_tag_anchor((ifp.line+1).to_str()+"_"+ifp.col.to_str() + "_refs" );
				doc.begin_tag_link( "#"+(ifp.line+1).to_str());
				doc.begin_tag("c24");
				doc.writeln(dc.sess.codemap.files[ifp.file_index].name+":"+(ifp.line+1).to_str()+":"+ifp.col.to_str()
							+"-"+(ifpe.line+1).to_str()+":"+ifpe.col.to_str() +" -" +info.kind + "- definition:");
				doc.end_tag();
				doc.begin_tag("pr");
		//			dump!(def_tfp);
				doc.writeln(get_source_line(fm,ifp.line) );
				doc.writeln(get_source_line(fm,ifp.line+1) );
				doc.end_tag();
				doc.end_tag();
				doc.end_tag();
			}
		}

	}

fn write_file_ref(doc:&mut HtmlWriter, dc:&RFindCtx,origin_fm:&codemap::FileMap, fi:uint) {
	
		let fname = dc.tycx.sess.codemap.files[fi].name;
		doc.begin_tag_link( make_html_name_rel(fname,origin_fm.name));
		doc.begin_tag("c24");
		doc.writeln(""+fname + ":");
		doc.end_tag();
	}
}

fn write_path_links(doc:&mut HtmlWriter, file_name:&str) {
	doc.writeln("");
	let file_path_col=&"c0";
	let file_delim_col=&"c1";
	let name_parts = file_name.split_iter('/').to_owned_vec();
	let num_dirs=name_parts.len()-1;
	let mut link_target=~"./";
	

	for x in range(0,num_dirs) {link_target.push_str("../");}
	doc.write("    ");
	doc.begin_tag_link(link_target+"index.html").write("(index<- )").end_tag().write("    ");
	doc.begin_tag_link(link_target).write("    ./").end_tag();

	for (i,x) in name_parts.iter().enumerate() {
		let is_dir = i < num_dirs;
		link_target.push_str(*x);
		if is_dir {link_target.push_str("/");}
		else { link_target.push_str(".html");}
		doc.begin_tag(file_path_col);
		doc.begin_tag_link(link_target); doc.write(*x);
		doc.end_tag();
		if is_dir {doc.write_tagged(file_delim_col,"/");}
	}
	doc.writeln("");
	doc.writeln("");
}

// TODO: a span index should uniquely identify the node.
// This adress form is a reasonable compromise between things we can see,
// things that are robust when some source changes, etc.
// file_index:line_index:col_index:length

fn get_node_index_file_pos(dc:&RFindCtx,nim:&FNodeInfoMap,nid:ast::NodeId)->Option<ZIndexFilePos> {
	let oni=nim.find(&nid);
//	assert!(oni.is_some());
	if oni.is_some() {
		let ni=oni.unwrap();
		byte_pos_to_index_file_pos(dc.tycx,ni.span.lo)
	} else {
		None
	}
}

fn make_html_name(f:&str)->~str { f+".html"}

fn count_chars_in(f:&str, x:char)->uint{
	let mut n=0;
	for c in f.iter() { if c==x {n+=1} }
	n
}

fn make_html_name_reloc(f:&str, origin:&str, reloc:&str)->~str {
	let mut acc=~"";
	let mut i=0;
	let depth_change=count_chars_in(origin,'/');//-count_chars_in(origin,'/')
	while i<depth_change {
		acc.push_str("../");
		i+=1;
	}
	if reloc.len()>0 { acc=reloc.to_owned(); if acc.iter().last().unwrap_or_default(' ')!='/' {acc.push_char('/')} }// replace that..
	acc.push_str(f);
	make_html_name(acc)
}
fn make_html_name_rel(f:&str, origin:&str)->~str {
	make_html_name_reloc(f,origin,"")
}



