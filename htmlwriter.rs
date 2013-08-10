use std::str;
pub struct HtmlWriter {
	doc:~str,
	tag_stack:~[~str],
	xlat:~[~str]
}

/// convert u8 into html char.
fn mk_xlat_table()->~[~str]
{
	let mut xlat=~[];
	let mut i=0;
	while i<256 {
		
		xlat.push(
			match i as char{
			' ' =>~"&nbsp;",
			'<' =>~"&lt;",
			'>' =>~"&gt;",
			'&' =>~"&amp;",
			'\t' =>~"&nbsp;&nbsp;&nbsp;&nbsp;",
			c=>str::from_char(i as char)//x.slice(0,1)
			});

		i+=1;
	}
	xlat
}
/// helper object for building html docs, puts tags in a stack for convinient close
impl<'self> HtmlWriter {
	pub fn new()->HtmlWriter { HtmlWriter { doc:~"", tag_stack:~[], xlat:mk_xlat_table()}}

	pub fn write_quoted_str(&'self mut self, a:&str)->&'self mut HtmlWriter {
		self.doc.push_str("\"");
		self.doc.push_str(a);
		self.doc.push_str("\"");
		self
	}
	pub fn begin_tag_ext(&'self mut self, tag_name:&str, key_values:&[(~str,~str)])->&'self mut HtmlWriter {
		self.write_tag_sub(tag_name,key_values,false);
		self.tag_stack.push(tag_name.to_str());
		self
	}
	pub fn begin_tag(&'self mut self, tag_name:&str)->&'self mut HtmlWriter {
		self.begin_tag_ext(tag_name,&[]);
		self
	}
	pub fn write_tag_ext(&'self mut self, tag_name:&str, key_values:&[(~str,~str)])->&'self mut HtmlWriter {
		self.write_tag_sub(tag_name,key_values,true);
		self
	}
	pub fn write_tag(&'self mut self, tag_name:&str)->&'self mut HtmlWriter {
		self.write_tag_sub(tag_name,&[],true);
		self
	}
	fn write_tag_sub(&'self mut self, tag_name:&str, key_values:&[(~str,~str)], closed:bool)->&'self mut HtmlWriter {
		self.doc.push_str("<"+tag_name);
		for &(ref k,ref v) in key_values.iter() {
			self.doc.push_str(" ");
			self.doc.push_str(*k);
			self.doc.push_str(&"=");
			self.write_quoted_str(*v);
		}
		if closed { self.doc.push_str("/"); }
		self.doc.push_str(">");
		self
	}
	pub fn end_tag(&'self mut self)->&'self mut HtmlWriter {
		let tag_name=self.tag_stack.pop();
		self.doc.push_str("</");
		self.doc.push_str(tag_name);
		self.doc.push_str(">");
		self
	}
	pub fn end_all_tags(&'self mut self)->&'self mut HtmlWriter {
		while self.tag_stack.len()>0 {
			self.end_tag();		
		}
		self
	}
	pub fn write(&'self mut self,t:&str)->&'self mut HtmlWriter {
		for x in t.iter() {
			self.doc.push_str(self.xlat[x]);
		};
		self
	}
	// todo rename "write_raw?"
	pub fn write_html(&'self mut self,t:&str)->&'self mut HtmlWriter {
		self.doc.push_str(t);
		self
	}
	pub fn write_tagged(&'self mut self, tagname:&str, text:&str)->&'self mut HtmlWriter {
		self.begin_tag(tagname);
		self.write(text);
		self.end_tag();
		self
	}
	pub fn writeln(&'self mut self, text:&str)->&'self mut HtmlWriter {
		self.write(text);
		self.write_tag("br");
		self
	}
	pub fn write_u8_(&'self mut self, x:u8)->&'self mut HtmlWriter {
			self.doc.push_str(self.xlat[x]);
			self
	}
	pub fn begin_tag_anchor(&'self mut self, anchor:&str)->&'self mut HtmlWriter {
		self.begin_tag_ext("a",[(~"id",anchor.to_owned())])
	}
	pub fn begin_tag_link(&'self mut self, link:&str)->&'self mut HtmlWriter {
		self.begin_tag_ext("a",[(~"href",link.to_owned())])
	}
	pub fn write_space(&'self mut self)->&'self mut HtmlWriter {
		self.doc.push_str("&emsp;");
		self
	}
}

