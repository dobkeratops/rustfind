use find_ast_node::*;
use syntax::codemap;
use syntax::ast;
use ioutil::*;
use htmlwriter::*;
use std::hashmap::*;
use std::vec;
use extra::sort;

pub fn make_html(dc:&DocContext, fm:&codemap::FileMap,nim:&FNodeInfoMap,jdm:&JumpToDefMap, jrm:&JumpToRefMap, nodes_per_line:&[~[ast::NodeId]])->~str {
	// todo - Rust2HtmlCtx { fm,nim,jdm,jrm } .. cleanup common intermediates
	let mut doc= HtmlWriter::new::();
	write_head(&mut doc);
	write_styles(&mut doc,fm.name);

	let hash=get_str_hash(fm.name);
	let bg=(~[~"383838",~"34383c",~"3c3834",~"383c34",~"343c38",~"38343c",~"3a343a",
			~"3a343a",~"36363a",~"363a36",~"3a3636",~"3a3a34",~"3a333a",~"343a3a",~"343a3c",~"343838"])[hash&15];
	// write the doc lines..
	doc.begin_tag_ext("body",&[(~"style",~"background-color:#"+bg+";")]);
	doc.begin_tag("maintext");
	let mut line=0;
	let fstart = *fm.start_pos;
	let max_digits=num_digits(fm.lines.len());

	while line<fm.lines.len() {
		// todo: line numbers want to go in a seperate column so they're unselectable..
		doc.write_tagged("ln",pad_to_length((line+1).to_str(),max_digits,"&nbsp;"));
		doc.begin_tag_anchor((line+1).to_str());
		let lend=if line<(fm.lines.len()-1){*fm.lines[line+1]-fstart}else{fm.src.len()};
		doc.write("&emsp;");
		let line_str=fm.src.slice(*fm.lines[line]-fstart,lend);
		//doc.writeln(line_str);
		doc.end_tag();
		let markup_line=insert_links_in_line(dc,fm, nim, jdm,jrm, line_str, nodes_per_line[line],line);
		doc.writeln(markup_line);
		line+=1;
	}
	write_references(&mut doc,dc,fm,nim,jdm,jrm, nodes_per_line);
	
	doc.end_tag();
	doc.end_tag();

	doc.doc
}

pub fn write_source_as_html_sub(dc:&DocContext, nim:&FNodeInfoMap,ndn:&HashMap<ast::NodeId,ast::def_id>, jdm:&JumpToDefMap) {
	
	let npl=NodesPerLinePerFile::new(dc,nim);
	let mut def2refs = ~MultiMap::new();
	for (&nref,&ndef) in jdm.iter() {
		def2refs.insert(ndef,nref);
	}

	// ew
	let mut fi=0;
	for fm in dc.sess.codemap.files.iter() {
		let doc_str=make_html(dc, *fm, nim,jdm, def2refs, npl.m[fi]);
		fileSaveStr(doc_str,change_file_name_ext(fm.name,"html"));
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
	doc.write("maintext {color:#f0f0f0; font-size:12px; font-family:\"Courier New\"}\n");
	doc.write("a:link{ color:#f0f0f0; font-style:normal;   text-decoration:none;}\n");
	doc.write("a:visited{ color:#f0f0f0; font-style:normal;   text-decoration:none;}\n");
	doc.write("a:link:hover{ color:#f0f0f0; font-style:normal; background-color:#606060; }\n");
	doc.write("pr{font-weight:bold}\n");
	doc.write("ln{color:#606060; }\n");
	doc.write("c24{color:#ffffff; font-style:italic; opacity:0.5}\n");
	doc.write("c25{color:#ffffff; opacity:0.92}\n");
	doc.write("c26{color:#ffffff; font-weight:bold; }\n");
	doc.write("c27{color:#ffffa0; font-weight:bold; }\n");
	doc.write("c28{color:#afffff; font-weight:bold; }\n");
	doc.write("c29{color:#afffaf; font-weight:bold; }\n");
	doc.write("c30{color:#cfcfff; font-weight:bold; }\n");
	doc.write("c31{color:#ffffff; font-style:italic; opacity:0.6}\n");
	doc.write("c1{color:#ffffc0;   font-weight:bold; }\n");
	doc.write("c2{color:#60f0c0}\n");
	doc.write("c3{color:#50e0ff; }\n");
	doc.write("c4{color:#f090f0}\n");
	doc.write("c5{color:#50ff80; }\n");
	doc.write("c6{color:#f0f0e0}\n");
	doc.write("c7{color:#fff0d0}\n");
	doc.write("c8{color:#e0d0f0}\n");
	doc.write("c9{color:#70f0f0}\n");
	doc.write("c10{color:#f0f070}\n");
	doc.write("c11{color:#c0f070}\n");
	doc.write("c12{color:#70c0f0}\n");
	doc.write("c13{color:#c0f070}\n");
	doc.write("c14{color:#f0ffc0}\n");
	doc.write("c15{color:#f0f0e0}\n");
	doc.write("c16{color:#c0ffe0}\n");
	doc.write("c17{color:#90d0f0}\n");
	doc.write("c18{color:#f0a0d0}\n");
	doc.write("c19{color:#d0f0a0}\n");
	doc.write("c20{color:#0f0ff}\n");
	doc.write("c21{color:#dde009; font-weight:bold}\n");
	doc.write("c22{color:#e0f0d0; font-weight:bold}\n");
	doc.write("c23{color:#e0f0ff; font-weight:bold}\n");

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


struct NodesPerLinePerFile {
	m :~[~[~[ast::NodeId]]]
}
//type NodesPerLine=&[~[ast::NodeId]];

pub fn get_file_index(dc:&DocContext,fname:&str)->Option<uint> {
	// todo - functional
	let mut index=0;
	while index<dc.sess.codemap.files.len() {
		if fname==dc.sess.codemap.files[index].name  { return Some(index);}
		index+=1;
	}
	None
}

impl NodesPerLinePerFile {
	fn new(dc:&DocContext, nim:&FNodeInfoMap)->~NodesPerLinePerFile {
		// todo, figure this out functionally?!
		//		dc.sess.codemap.files.map(
		//				|fm:&@codemap::FileMap|{ vec::from_elem(fm.lines.len(), ~[]) }
		//			).collect();

		let mut npl=~NodesPerLinePerFile{m:~[]};
		let mut fi=0;
		npl.m = vec::from_elem(dc.sess.codemap.files.len(),~[]);
		while fi<dc.sess.codemap.files.len() {
			npl.m[fi] = from_elem(dc.sess.codemap.files[fi].lines.len(),~[]);
			fi+=1;
		}
		for (k,v) in nim.iter() {
			// todo, this could be more direct, file index, line index, ...
			match byte_pos_to_index_file_pos(dc.tycx,v.span.lo) {
				None=>{},
				Some(ifp)=>{
//					dump!(ifp);
					let ifpe=byte_pos_to_index_file_pos(dc.tycx,v.span.hi).unwrap();
					for li in range(ifp.line,ifpe.line+1) {
						npl.m[ifp.file_index][li].push(*k)
					}
				}
			}
		}
		npl
	}
	fn dump(&self) {
		for f in self.m.iter() {
			print("file {");
			for l in f.iter() {
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
		~"keyword"|~"while"|~"match"|~"loop"|~"do"|~"cast"|~"if"|~"return"=>7,
		~"path"=>8,
		~"call"=>9,
		~"method_call"=>10,
		~"enum"=>11,
		~"lit"=>12,
		~"stmt"=>13,
		~"mod"=>14,
		~"struct"=>15,
		~"local"=>16,
		~"impl"=>17,
		~"trait"=>18,
		~"pat"=>20,
		~"block"|~"blk"|~"fn_block"=>21,
		~"method"|~"type_method"=>22,
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
fn text_here_is(line:&str, pos:uint,reftext:&str, color:int)->int {
	if pos+reftext.len()+2>=line.len() { return 0}
	if pos>0 {
		if is_alphanumeric(line[pos-1] as char) { return 0 }// must be word start
	}
	if is_alphanumeric(line[pos+reftext.len()] as char) { return 0 }// must be word start
	let mut i=0;
	while i<reftext.len() {
		if reftext[i]!=line[i+pos] {return 0}
		i+=1;
	}

	return color;
}

fn insert_links_in_line(dc:&DocContext,fm:&codemap::FileMap, nim:&FNodeInfoMap,jdm:&JumpToDefMap, jrm:&JumpToRefMap, line:&str, nodes:&[ast::NodeId],line_index:uint)->~str {

	let node_infos=nodes.map(|id|{nim.find(id)});
//	for x in node_infos.iter() { println(fmt!("%?", x));}
//	dump!(node_infos);
// todo - sorting node spans, not this "painters-algorithm" approach..

	let mut link:~[ast::NodeId] = vec::from_elem(line.len(),0 as ast::NodeId);
	let mut color:~[int] = vec::from_elem(line.len(),0 as int);
	let mut depth:~[uint]= vec::from_elem(line.len(),0x7fffffff as uint);
	let mut rndcolor=0;
	let mut no_link=0 as ast::NodeId;;
	for n in nodes.iter() {

		match nim.find(n) {
			None=>{},
			Some(ni)=>{
				// link_id >0 = node def link. link_id<0 = node ref link
				let link_id= match jdm.find(n) {
					None=> if jrm.find(*n).len()>0 { - *n } else { no_link },
					Some(x) =>*x
				};
				
				let s=byte_pos_to_index_file_pos(dc.tycx, ni.span.lo).unwrap();
				let e=byte_pos_to_index_file_pos(dc.tycx, ni.span.hi).unwrap();
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
	// paint comments out, mark delimiter symbols--override what we get from buggy tree picture...
	// TODO ... need to figure out tree nodes encompasing the current line from above to 
	// propogate information properly eg brackets inside a type ..

	{
		let mut x=0;
		while x<(line.len()) {
			match line[x] as char{
				'{'|'}'|'['|']'|';'|',' => {color[x]=4;},
				'('|')'=> {color[x]=25;},
				_=>{}
			}

			if x<line.len()-1 {
				if line[x+0]=='/' as u8 && line[x+1]=='/' as u8 {
					let mut comment_color=24;
					if x<line.len()-2 { if line[x+2]as char=='/' { comment_color=31 }}// doc-comments are a bit brighter
					while x<line.len() {
						color[x]=comment_color;
						x+=1;
					}
				}
			}
			// override color for top level decls
			let decl_color=text_here_is(line,x,"type",28)|text_here_is(line,x,"struct",27)|text_here_is(line,x,"trait",1)|text_here_is(line,x,"impl",29)|text_here_is(line,x,"enum",30)|text_here_is(line,x,"fn",26);
			if decl_color>0{
				let mut in_typaram=0;
				while (x<line.len()) && (line[x] as char)!='{' && (line[x] as char)!='('{
					in_typaram+=match line[x] as char {'<'=>1,_=>0};
					color[x]=if in_typaram==0{decl_color}else{5};
					in_typaram+=match line[x] as char {'>'=>-1,_=>0};
					x+=1;
				}
			}
			x+=1;
		}
	}
	
	// emit a span..
	let mut x=0;
	let mut curr_col=-1;
	let mut curr_link=0;
	let mut outp=HtmlWriter::new();
	while x<line.len() {
		// link overrides color..
		if curr_link!=link[x] {
			if curr_link>0 {outp.end_tag();}
			if curr_col>=0 {outp.end_tag();}
			curr_col=-1;
			curr_link=link[x];

			if curr_link!=no_link {
				if curr_link>0 {
					match nim.find(&curr_link) {
						None=>curr_link=0,	// link outside the crate?
						Some(link_node_info)=>{
							let ifp = byte_pos_to_index_file_pos(dc.tycx, link_node_info.span.lo).unwrap();
							let link_str="#"+(ifp.line+2).to_str();
							outp.begin_tag_link(change_file_name_ext(dc.sess.codemap.files[ifp.file_index].name,"html")+link_str);
						}
					}
				} else if curr_link<0{
					let ifp= get_node_index_file_pos(dc,nim,-curr_link);
					let link_str="#"+ifp.line.to_str()+"_"+ifp.col.to_str()+"_refs";
					outp.begin_tag_link(change_file_name_ext(fm.name,"html")+link_str);
				}
			}
		}
		if curr_col!=color[x] {
			if curr_col>=0 {
				outp.end_tag();
			}
			curr_col=color[x];
			outp.begin_tag(color_index_to_tag(curr_col));
		}
		outp.write(
			match line[x] as char {
			' '=>"&nbsp;",
			'<'=>"&lt;",
			'>'=>"&gt;",
			'&'=>"&amp;",
			'\t'=>"&nbsp;&nbsp;&nbsp;&nbsp;",
			_=>line.slice(x,x+1)
			}
		);
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
	if curr_col>=0 {outp.end_tag();}
	if curr_link>0 {outp.end_tag();}
	outp.doc
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

fn write_references(doc:&mut HtmlWriter,dc:&DocContext, fm:&codemap::FileMap,nim:&FNodeInfoMap,jdm:&JumpToDefMap,jrm:&JumpToRefMap, nodes_per_line:&[~[ast::NodeId]]) {
	doc.write_tag("div");
	let file_def_nodes = find_defs_in_file(fm,nim);
	//let mut defs_to_refs=MultiMap::new::<ast::NodeId, ast::NodeId>();
	for &dn in file_def_nodes.iter() {
		let opt_def_info = nim.find(&dn);
		if !opt_def_info.is_some() {loop;}
		let def_info = opt_def_info.unwrap();
		if !(def_info.kind==~"fn" || def_info.kind==~"struct" || def_info.kind==~"trait" || def_info.kind==~"enum" || def_info.kind==~"ty") {loop}

		let refs = jrm.find(dn);
		let max_links=10;	// todo - sort..
		let max_short_links=30;	// todo - sort..

		
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
				.transform(|&id|{
					let ni=nim.find(&id).unwrap();
					let ifp=byte_pos_to_index_file_pos(dc.tycx, ni.span.lo).unwrap();
					(ni,ifp,id)})
				.to_owned_vec();

			let l=refs2.len();
			sort::qsort(refs2,0,l-1, |&(_,ref ifp1,_),&(_,ref ifp2,_)|{ ifp1.file_index-curr_file<=ifp2.file_index-curr_file });
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
						newline=true;
					};
					if curr_file!=ref_ifp.file_index {
						if newline==false {doc.writeln("");}
						curr_file=ref_ifp.file_index;
						write_file_ref(doc,dc,curr_file);
						newline=true;
					}

					if links_written<(max_short_links+max_links) {
						if  links_written<max_links {
							if newline==false {doc.writeln("");newline=true;}
						}
						let rfm=&dc.sess.codemap.files[ref_ifp.file_index];
						doc.begin_tag_link( change_file_name_ext(rfm.name,"html")+"#"+(ref_ifp.line+1).to_str());

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

	fn write_refs_header(doc:&mut HtmlWriter,dc:&DocContext,nim:&FNodeInfoMap, fm:&codemap::FileMap, nid:ast::NodeId) {
		doc.writeln("");
		let info=nim.find(&nid).unwrap();
		let ifp=byte_pos_to_index_file_pos(dc.tycx, info.span.lo).unwrap();
		let ifpe=byte_pos_to_index_file_pos(dc.tycx, info.span.hi).unwrap();
//		let def_info=nim.find(&nid).unwrap();
//		let ifpe=get_node_index_file_pos(dc,nim,nid).unwrap();

		doc.begin_tag_anchor(ifp.line.to_str()+"_"+ifp.col.to_str() + "_refs" );
		doc.begin_tag("c24");
		doc.writeln(dc.sess.codemap.files[ifp.file_index].name+":"+(ifp.line+1).to_str()+":"+ifp.col.to_str()
					+"-"+(ifpe.line+1).to_str()+":"+ifpe.col.to_str());
		doc.end_tag();
		doc.begin_tag_link( change_file_name_ext(fm.name,"html")+"#"+(ifp.line+1).to_str());
		doc.begin_tag("pr");
//			dump!(def_tfp);
		doc.writeln(get_source_line(fm,ifp.line) );
		doc.writeln(get_source_line(fm,ifp.line+1) );
		doc.end_tag();
		doc.end_tag();
		doc.end_tag();
	}

	fn write_file_ref(doc:&mut HtmlWriter, dc:&DocContext,fi:uint) {
		let fname = dc.tycx.sess.codemap.files[fi].name;
		doc.begin_tag_link( change_file_name_ext(fname, "html"));
		doc.begin_tag("c24");
		doc.writeln(""+fname + ":");
		doc.end_tag();
	}

}

// TODO: a span index should uniquely identify the node.
// This adress form is a reasonable compromise between things we can see,
// things that are robust when some source changes, etc.
// file_index:line_index:col_index:length

fn get_node_index_file_pos(dc:&DocContext,nim:&FNodeInfoMap,nid:ast::NodeId)->IndexFilePos {
	let ni=nim.find(&nid).unwrap();
	byte_pos_to_index_file_pos(dc.tycx,ni.span.lo).unwrap()
}




