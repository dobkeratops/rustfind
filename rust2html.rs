use find_ast_node::*;
use syntax::codemap;
use syntax::ast;
use ioutil::*;
use htmlwriter::*;
use std::hashmap::*;
use std::vec;

pub fn make_html(dc:&DocContext, fm:&codemap::FileMap,nim:&NodeInfoMap,jdm:&JumpToDefMap, nodes_per_line:&[~[ast::NodeId]])->~str {
	let mut doc= HtmlWriter::new::();
	write_head(&mut doc);
	write_styles(&mut doc);

	// write the doc lines..
	doc.begin_tag_ext("body",&[(~"style",~"background-color:#383c40;")]);
	doc.begin_tag("maintext");
	let mut line=0;
	let fstart = *fm.start_pos;
	let max_digits=num_digits(fm.lines.len());

	while line<fm.lines.len() {
		// todo: line numbers want to go in a seperate column so they're unselectable..
		doc.write_tagged("ln",pad_to_length(line.to_str(),max_digits,"0"));
		doc.begin_tag_anchor(line.to_str());
		let lend=if line<(fm.lines.len()-1){*fm.lines[line+1]-fstart}else{fm.src.len()};
		doc.write("&emsp;");
		let line_str=fm.src.slice(*fm.lines[line]-fstart,lend);
		//doc.writeln(line_str);
		doc.end_tag();
		let markup_line=insert_links_in_line(dc,fm, nim, jdm,line_str, nodes_per_line[line]);
		doc.writeln(markup_line);
		line+=1;
	}
	doc.end_tag();
	doc.end_tag();

	doc.doc
}

pub fn write_source_as_html_sub(dc:&DocContext, nim:&NodeInfoMap,ndn:&HashMap<ast::NodeId,ast::def_id>, jdm:&JumpToDefMap) {
	
	let npl=NodesPerLinePerFile::new(dc,nim);

	// ew
	let mut fi=0;
	for fm in dc.sess.codemap.files.iter() {
		let doc_str=make_html(dc, *fm, nim,jdm, npl.m[fi]);
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

pub fn write_styles(doc:&mut HtmlWriter){
	// write the styles..
	doc.begin_tag_ext("style",&[(~"type",~"text/css")]);
	doc.write("maintext {color:#f0f0f0; font-size:15px; font-family:\"Courier New\"}\n");
	doc.write("a:link{ color:#f0f0f0; font-style:normal;   text-decoration:none;}\n");
	doc.write("a:visited{ color:#f0f0f0; font-style:normal;   text-decoration:none;}\n");
	doc.write("a:link:hover{ color:#f0f0f0; font-style:normal; background-color:#606060; }\n");
	doc.write("pr{font-weight:bold}\n");
	doc.write("ln{color:#606060;background-color:#3c3c3c; }\n");
	doc.write("rem{color:#ffffff; font-style:italic;font-weight:bold; opacity:0.4}\n");
	doc.write("c1{color:#ffffc0;   font-weight:bold; }\n");
	doc.write("c2{color:#60f0c0}\n");
	doc.write("c3{color:#a0c0ff; font-weight:bold;}\n");
	doc.write("c4{color:#d0b0f0}\n");
	doc.write("c5{color:#a0e0e0; font-weight:bold}\n");
	doc.write("c6{color:#f0f0e0}\n");
	doc.write("c7{color:#f0f0f0}\n");
	doc.write("c8{color:#f070f0}\n");
	doc.write("c9{color:#70f0f0}\n");
	doc.write("c10{color:#f0f070}\n");
	doc.write("c11{color:#c0f070}\n");
	doc.write("c12{color:#70c0f0}\n");
	doc.write("c13{color:#c0f070}\n");
	doc.write("c14{color:#f070c0}\n");
	doc.write("c15{color:#f070c0}\n");
	doc.write("c16{color:#d0a0f0}\n");
	doc.write("c17{color:#a0d0f0}\n");
	doc.write("c18{color:#a0f0d0}\n");
	doc.write("c19{color:#d0f0a0}\n");
	doc.write("c20{color:#f0a0d0}\n");
	doc.write("c21{color:#e0e009; font-weight:bold}\n");
	doc.write("c22{color:#09f00d; font-weight:bold}\n");
	doc.write("c23{color:#b0e0c0; font-weight:bold}\n");

	doc.end_tag();
}


fn num_digits(a:uint)->uint{
	let mut n=1;
	let mut aa=a;
	while aa>10 { aa/=10; n+=1;}
	n
}
fn pad_to_length(a:&str,l:uint,pad:&str)->~str {
	let mut acc=~" ";
	let mut i=l-a.len();
	while i>0 {
		acc.push_str(pad);
		i-=pad.len();
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
	fn new(dc:&DocContext, nim:&NodeInfoMap)->~NodesPerLinePerFile {
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
					npl.m[ifp.file_index][ifp.line].push(*k)
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
pub fn node_color_index(ni:&NodeInfo)->int {
	// todo, an enum ffs..
	match ni.kind {
		~"fn"=>1,
		~"add"|~"sub"|~"mul"|~"div"|~"assign"|~"eq"|~"le"|~"gt"|~"ge"|~"ne"|~"binop"|~"assign_op"=>2,
		~"ty"=>3,
		~"de"=>4,
		~"type_param"=>5,
		~"struct_field"=>6,
		~"keyword"=>7,
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
		~"block"=>21,
		~"method"=>22,
		~"tup"=>4,

		_ =>0
	}	
}
pub fn color_index_to_tag(i:int)->~str {
	"c"+i.to_str()
}

fn insert_links_in_line(dc:&DocContext,fm:&codemap::FileMap, nim:&NodeInfoMap,jdm:&JumpToDefMap, line:&str, nodes:&[ast::NodeId])->~str {
	let node_infos=nodes.map(|id|{nim.find(id)});
//	for x in node_infos.iter() { println(fmt!("%?", x));}
//	dump!(node_infos);
// todo - sorting node spans, not this "painters-algorithm" approach..

	let mut link:~[ast::NodeId] = vec::from_elem(line.len(),0 as ast::NodeId);
	let mut color:~[int] = vec::from_elem(line.len(),0 as int);
	let mut depth:~[uint]= vec::from_elem(line.len(),0x7fffffff as uint);
	let mut rndcolor=0;
	let mut default_link_id=0 as ast::NodeId;;
	for n in nodes.iter() {
		match nim.find(n) {
			None=>{},
			Some(ni)=>{
				let link_id=jdm.find(n).get_or_default(&default_link_id);
				let s=byte_pos_to_index_file_pos(dc.tycx, ni.span.lo).unwrap();
				let e=byte_pos_to_index_file_pos(dc.tycx, ni.span.hi).unwrap();
				let d=*ni.span.hi-*ni.span.lo;	// todo - get the actual hrc node depth in here!
				let mut x=s.col;
				// 'paint' the nodes we have here.
				let ci = node_color_index(ni);
				while x<e.col && x<line.len() {
					if d < depth[x] {
						color[x]=ci;
						depth[x]=d;
						link[x]=*link_id;
					}
					x+=1;
				}
				rndcolor+=1;
			}
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
			if curr_link>0 {
				match nim.find(&curr_link) {
					None=>curr_link=0,	// link outside the crate.
					Some(link_node_info)=>{
						let tfp = byte_pos_to_text_file_pos(dc.tycx, link_node_info.span.lo);
						outp.begin_tag_link(change_file_name_ext(tfp.name,"html")+"#"+tfp.line.to_str());
					}
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
		outp.write(line.slice(x,x+1));
		x+=1;
	}
	if curr_col>=0 {outp.end_tag();}
	outp.doc
}




