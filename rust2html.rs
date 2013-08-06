use find_ast_node::*;
use syntax::codemap;
use ioutil::*;

struct HtmlWriter {
	doc:~str,
	tag_stack:~[~str]
}

impl<'self> HtmlWriter {
	fn new()->HtmlWriter { let x=HtmlWriter { doc:~"", tag_stack:~[]}; x
	}

	fn push_quoted_str(&'self mut self, a:&str)->&'self mut HtmlWriter {
		self.doc.push_str("\"");
		self.doc.push_str(a);
		self.doc.push_str("\"");
		self
	}

	fn begin_tag_ext(&'self mut self, tag_name:&str, key_values:&[(~str,~str)])->&'self mut HtmlWriter {
		self.write_tag_sub(tag_name,key_values,false);
		self.tag_stack.push(tag_name.to_str());
		self
	}
	fn begin_tag(&'self mut self, tag_name:&str)->&'self mut HtmlWriter {
		self.begin_tag_ext(tag_name,&[]);
		self
	}
	fn write_tag_ext(&'self mut self, tag_name:&str, key_values:&[(~str,~str)])->&'self mut HtmlWriter {
		self.write_tag_sub(tag_name,key_values,true);
		self
	}
	fn write_tag(&'self mut self, tag_name:&str)->&'self mut HtmlWriter {
		self.write_tag_sub(tag_name,&[],true);
		self
	}
	fn write_tag_sub(&'self mut self, tag_name:&str, key_values:&[(~str,~str)], closed:bool)->&'self mut HtmlWriter {
		self.doc.push_str("<"+tag_name);
		for &(ref k,ref v) in key_values.iter() {
			self.doc.push_str(" ");
			self.doc.push_str(*k);
			self.doc.push_str(&"=");
			self.push_quoted_str(*v);
		}
		if closed { self.doc.push_str("/"); }
		self.doc.push_str(">");
		self
	}
	fn end_tag(&'self mut self)->&'self mut HtmlWriter {
		let tag_name=self.tag_stack.pop();
		self.doc.push_str("</");
		self.doc.push_str(tag_name);
		self.doc.push_str(">");
		self
	}
	fn write(&'self mut self,t:&str)->&'self mut HtmlWriter {
		self.doc.push_str(t);
		self
	}
	fn write_tagged(&'self mut self, tagname:&str, text:&str)->&'self mut HtmlWriter {
		self.begin_tag(tagname);
		self.doc.push_str(text);
		self.end_tag();
		self
	}
	fn writeln(&'self mut self, text:&str)->&'self mut HtmlWriter {
		self.doc.push_str(text);
		self.write_tag("br");
		self
	}
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

pub fn write_html(dc:&DocContext, fm:&codemap::FileMap)->~str {
	let mut doc=HtmlWriter::new();

	doc.begin_tag("head");
	doc.write_tag_ext("link",&[(~"href",~"css/shCore.css"),(~"rel",~"stylesheet"),(~"type",~"text/css")]);
	doc.write_tag_ext("link",&[(~"href",~"css/shThemeDefault.css"),(~"rel",~"stylesheet"),(~"type",~"text/css")]);
	doc.end_tag();
	// write the styles..
	doc.begin_tag_ext("style",&[(~"type",~"text/css")]);
	doc.write("maintext {color:#f0f0f0; font-size:15px; font-family:\"Courier New\"}\n");
	doc.write("a:link{ color:#f0f0f0; font-style:normal;   text-decoration:none;}\n");
	doc.write("a:visited{ color:#f0f0f0; font-style:normal;   text-decoration:none;}\n");
	doc.write("a:link:hover{ color:#f0f0f0; font-style:normal; background-color:#606060; }\n");
	doc.write("fn {color:#ffffc0;   font-weight:bold; }\n");
	doc.write("pr{font-weight:bold}\n");
	doc.write("ty{color:#a0c0ff; font-weight:bold;}\n");
	doc.write("de{color:#d0b0f0}\n");
	doc.write("tp{color:#a0e0e0; font-weight:bold}\n");
	doc.write("impl{color:#e0f0ff; font-weight:bold}\n");
	doc.write("op{color:#60f0c0}\n");
	doc.write("rem{color:#ffffff; font-style:italic;font-weight:bold; opacity:0.4}\n");
	doc.write("ln{color:#606060;background-color:#3c3c3c; }\n");

	doc.end_tag();

	// write the doc lines..
	doc.begin_tag_ext("body",&[(~"style",~"background-color:#383c40;")]);
	doc.begin_tag("maintext");
	let mut line=0;
	let fstart = *fm.start_pos;
	let max_digits=num_digits(fm.lines.len());

	while line<fm.lines.len() {
		doc.write_tagged("ln",pad_to_length(line.to_str(),max_digits,"0"));
		let lend=if line<(fm.lines.len()-1){*fm.lines[line+1]-fstart}else{fm.src.len()};
		doc.write("&emsp;");
		doc.writeln(fm.src.slice(*fm.lines[line]-fstart,lend));
		line+=1;
	}
	doc.end_tag();
	doc.end_tag();
//	print(doc.doc);
	fileSaveStr(doc.doc,fm.name+".html");
	doc.doc
}

pub fn write_source_as_html(dc:&DocContext, filename:&str, out_dir_name:&str) {
	for fm in dc.sess.codemap.files.iter() {
		write_html(dc, *fm);
	}
}

