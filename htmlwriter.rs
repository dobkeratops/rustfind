use rf_common::*;
use std::iter::range_inclusive;

pub struct HtmlWriter {
	doc:~str,
	tag_stack:~[~str],
	xlat:HashMap<char, ~str>,
}

/// convert u8 into html char.
fn mk_xlat_table()-> HashMap<char,~str>
{
	let mut xlat= HashMap::new();
// 	let mut i=0u8;
	for i in range_inclusive(0u8, 255) {

		xlat.insert(i as char,
			match i as char{
			' ' =>~"&nbsp;",
			'<' =>~"&lt;",
			'>' =>~"&gt;",
			'&' =>~"&amp;",
			'\t' =>~"&nbsp;&nbsp;&nbsp;&nbsp;",
			c => str::from_char(c)//x.slice(0,1)
			});
	}
	xlat
}
/// helper object for building html docs, puts tags in a stack for convinient close
impl<'s> HtmlWriter {
	pub fn new()->HtmlWriter { HtmlWriter { doc:~"", tag_stack:~[], xlat:mk_xlat_table()}}

	pub fn write_quoted_str(&'s mut self, a:&str)->&'s mut HtmlWriter {
		self.doc.push_str("\"");
		self.doc.push_str(a);
		self.doc.push_str("\"");
		self
	}
	pub fn begin_tag_ext(&'s mut self, tag_name:&str, key_values:&[(~str,~str)])->&'s mut HtmlWriter {
		self.write_tag_sub(tag_name,key_values,false);
		self.tag_stack.push(tag_name.to_str());
		self
	}
	pub fn begin_tag(&'s mut self, tag_name:&str)->&'s mut HtmlWriter {
		self.begin_tag_ext(tag_name,&[])
	}
	pub fn write_tag_ext(&'s mut self, tag_name:&str, key_values:&[(~str,~str)])->&'s mut HtmlWriter {
		self.write_tag_sub(tag_name,key_values,true)
	}
	pub fn write_tag(&'s mut self, tag_name:&str)->&'s mut HtmlWriter {
		self.write_tag_sub(tag_name,&[],true);
		self
	}
	fn write_tag_sub(&'s mut self, tag_name:&str, key_values:&[(~str,~str)], closed:bool)->&'s mut HtmlWriter {
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
	pub fn end_tag(&'s mut self)->&'s mut HtmlWriter {
		let tag_name=self.tag_stack.pop();
		self.doc.push_str("</");
		self.doc.push_str(tag_name);
		self.doc.push_str(">");
		self
	}
	pub fn end_all_tags(&'s mut self)->&'s mut HtmlWriter {
		while self.tag_stack.len()>0 {
			self.end_tag();
		}
		self
	}
	pub fn write(&'s mut self,t:&str)->&'s mut HtmlWriter {
		for x in t.iter() {
			self.doc.push_str(self.xlat.get(&x).clone());
		};
		self
	}
	// todo rename "write_raw?"
	pub fn write_html(&'s mut self,t:&str)->&'s mut HtmlWriter {
		self.doc.push_str(t);
		self
	}
	pub fn write_tagged(&'s mut self, tagname:&str, text:&str)->&'s mut HtmlWriter {
		self.begin_tag(tagname).write(text).end_tag()
	}
	pub fn writeln_tagged(&'s mut self, tagname:&str, text:&str)->&'s mut HtmlWriter {
		self.begin_tag(tagname).write(text).end_tag().write_tag("br")
	}
	pub fn writeln(&'s mut self, text:&str)->&'s mut HtmlWriter {
		self.write(text).write_tag("br")
	}
	pub fn write_u8_(&'s mut self, x:u8)->&'s mut HtmlWriter {
		self.doc.push_str(self.xlat.get(&(x as char)).to_owned());
		self
	}
	pub fn begin_tag_anchor(&'s mut self, anchor:&str)->&'s mut HtmlWriter {
		self.begin_tag_ext("a",[(~"id",anchor.to_owned())])
	}
	pub fn begin_tag_link(&'s mut self, link:&str)->&'s mut HtmlWriter {
		self.begin_tag_ext("a",[(~"href",link.to_owned())])
	}
	pub fn write_space(&'s mut self)->&'s mut HtmlWriter {
		self.doc.push_str("&emsp;");
		self
	}
}

