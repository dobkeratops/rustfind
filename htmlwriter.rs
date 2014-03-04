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
//  let mut i=0u8;
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
impl<'a> HtmlWriter {
    pub fn new()->HtmlWriter { HtmlWriter { doc:~"", tag_stack:~[], xlat:mk_xlat_table()}}

    pub fn write_quoted_str(&'a mut self, a:&str)->&'a mut HtmlWriter {
        self.doc.push_str("\"");
        self.doc.push_str(a);
        self.doc.push_str("\"");
        self
    }
    pub fn begin_tag_ext(&'a mut self, tag_name:&str, key_values:&[(~str,~str)])->&'a mut HtmlWriter {
        self.write_tag_sub(tag_name,key_values,false);
        self.tag_stack.push(tag_name.to_str());
        self
    }
    pub fn begin_tag(&'a mut self, tag_name:&str)->&'a mut HtmlWriter {
        self.begin_tag_ext(tag_name,&[])
    }
    pub fn write_tag_ext(&'a mut self, tag_name:&str, key_values:&[(~str,~str)])->&'a mut HtmlWriter {
        self.write_tag_sub(tag_name,key_values,true)
    }
    pub fn write_tag(&'a mut self, tag_name:&str)->&'a mut HtmlWriter {
        self.write_tag_sub(tag_name,&[],true);
        self
    }
    fn write_tag_sub(&'a mut self, tag_name:&str, key_values:&[(~str,~str)], closed:bool)->&'a mut HtmlWriter {
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
    pub fn end_tag(&'a mut self)->&'a mut HtmlWriter {
        match self.tag_stack.pop() {
            Some(tag_name) => {
                self.doc.push_str("</");
                self.doc.push_str(tag_name);
                self.doc.push_str(">");
            },
            None => ()
        };
        self
    }
    #[allow(dead_code)]
    pub fn end_all_tags(&'a mut self)->&'a mut HtmlWriter {
        while self.tag_stack.len()>0 {
            self.end_tag();
        }
        self
    }
    pub fn write(&'a mut self,t:&str)->&'a mut HtmlWriter {
        for x in t.chars() {
            self.doc.push_str(self.xlat.get(&x).clone());
        };
        self
    }
    // todo rename "write_raw?"
    #[allow(dead_code)]
    pub fn write_html(&'a mut self,t:&str)->&'a mut HtmlWriter {
        self.doc.push_str(t);
        self
    }
    pub fn write_tagged(&'a mut self, tagname:&str, text:&str)->&'a mut HtmlWriter {
        self.begin_tag(tagname).write(text).end_tag()
    }
    pub fn writeln_tagged(&'a mut self, tagname:&str, text:&str)->&'a mut HtmlWriter {
        self.begin_tag(tagname).write(text).end_tag().write_tag("br")
    }
    pub fn writeln(&'a mut self, text:&str)->&'a mut HtmlWriter {
        self.write(text).write_tag("br")
    }
    pub fn write_u8_(&'a mut self, x:u8)->&'a mut HtmlWriter {
        self.doc.push_str(self.xlat.get(&(x as char)).to_owned());
        self
    }
    pub fn begin_tag_anchor(&'a mut self, anchor:&str)->&'a mut HtmlWriter {
        self.begin_tag_ext("a",[(~"id",anchor.to_owned())])
    }
    pub fn begin_tag_link(&'a mut self, link:&str)->&'a mut HtmlWriter {
        self.begin_tag_ext("a",[(~"href",link.to_owned())])
    }
    #[allow(dead_code)]
    pub fn write_space(&'a mut self)->&'a mut HtmlWriter {
        self.doc.push_str("&emsp;");
        self
    }
}

