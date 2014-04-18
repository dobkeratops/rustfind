
/// tags: pretty print,code formatting,brace indentation, json braces, brackets,nesting
pub trait Indent {
    fn indent(&self,tab_size:int,max_line_size:int)->Self;
}

impl Indent for ~str {
    fn indent(&self,tabsize:int, linesize:int)->~str {
        // todo-write as iterators.
        fn change_indent(c:u8)->int{
            match c as char {
                '{'|'('|'['=> 1,
                '}'|')'|']'=> -1,
                _ => 0
            }
        }
        let mut a=StrBuf::new();
        let mut i=0;
        let mut indent=0;
        let len=self.len();
        while i<len {
            // skip leading whitespace. we are on a newline.
            while i<len && (self[i]==' 'as u8 || self[i]=='\t'as u8) { i+=1;}
            // measure line size from here
            let mut dii=0;
            if change_indent(self[i])<0 { indent-=1;dii=1}/*TODO-more elegant*/
            let mut cur_linesize=indent*tabsize;
            let mut ii=i;
            let mut inner_brace_level=indent;
            let mut last_open=len;
            let first_base_delim=len;

            while inner_brace_level>=indent && ii<len  {
                let c=self[ii];
                if c=='\n'as u8 {break};
                if cur_linesize >= linesize {break};
                cur_linesize+=1;
                let di=change_indent(c);
                if di>0 && inner_brace_level==indent {last_open=ii;};
                inner_brace_level+=di;
                if inner_brace_level==indent {
                    if di<0 ||c==','as u8||c==';'as u8 {
                        last_open=ii;
                    }

                    if c==','as u8||c==';'as u8 && first_base_delim==len {
                        //first_base_delim=ii;
                    }
                }
                ii+=1
            }
            {
                let mut ii=0;
                while ii<tabsize*indent { a.push_char(' '); ii+=1 }
            }
            indent+=dii;    // the extra one we considered.
            let init_indent=indent;

            if cur_linesize<linesize
            {
                // copy the line. we dont overstep
                while i<len && self[i]!='\n'as u8 && indent>=init_indent && i<=first_base_delim && i<=last_open{
                    let c=self[i];
                    indent+=change_indent(c);
                    a.push_char(c as char);
                    i+=1;
                }
            }
            else {
                // copy the line until the lines' last opening
                while i<len && indent>=init_indent && i<=last_open && i<=first_base_delim{
                    let c=self[i];
                    indent+=change_indent(c);
                    a.push_char(c as char);
                    i+=1;
                }
            }
            a.push_char('\n');
        }
        a.into_owned()
    }
}

