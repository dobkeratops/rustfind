use std::vec::Vec;


pub fn text_line_pos_to_offset(text: &[u8], (line, ofs_in_line): (u32, u32))->Option<u32> {
    // line as reported by grep & text editors,counted from '1' not '0'
    let mut pos = 0;
    let tlen=text.len() as u32;
    let mut tline=0;
    let mut line_start_pos = 0;
    while pos<tlen{
        match text[pos as uint] as char{
            '\n' => {tline+=1; line_start_pos=pos;},
//          "\a" => {tpos=0;line_pos=pos;},
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

pub fn get_filename_only(fnm:&str)->~str {
    let toks:~[&str]=fnm.split(':').collect();
    return toks[0].to_str();
}


pub fn text_offset_to_line_pos(text:&[u8], src_ofs: u32)-> Option<(u32, u32)> {
    // line as reported by grep & text editors,counted from '1' not '0'
    let mut pos = 0;
    let tlen = text.len() as u32;
    let mut tline=0;
    let mut line_start_pos=0;
    while pos<tlen{
        match text[pos as uint] as char{
            '\n' => {
                if src_ofs<=pos && src_ofs>line_start_pos {
                    return Some((tline+1,src_ofs-line_start_pos));
                }
                tline+=1; line_start_pos=pos;
            },
//          "\a" => {tpos=0;line_pos=pos;},
            _ => {}
        }
        // todo - clamp line end
        pos+=1;
    }
    return None;
}
pub fn flatten_to_str<T,U:ToStr>(xs:&[T],f: |&T| -> U, sep:&str)->~str {
    let mut acc=StrBuf::new();
    let mut i=0; // TODO - functional way.
    while i<xs.len() {
        if i>0 {acc.push_str(sep);}
        acc.push_str( f(&xs[i]).to_str() );

        i+=1;
    }
    acc.into_owned()
}

pub fn flatten_to_str_ng<T, U:ToStr>(xs: &Vec<T>, f: |&T| -> U, sep: &str) -> ~str {
    if xs.is_empty() { return ~""; }

    // this calculation is not right, but not sure what the best approximation is
    // code taken originally from StrVector code for collect()
    let len = sep.len() * (xs.len() * 2 - 1);
    let mut result = StrBuf::with_capacity(len);
    let mut first = true;

    for s in xs.iter() {
        if first {
            first = false;
        } else {
            result.push_str(sep);
        }
        result.push_str(f(s).to_str());
    }
    result.into_owned()
}
