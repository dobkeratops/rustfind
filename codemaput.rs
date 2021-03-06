use rf_common::*;
use rustfindctx::RustFindCtx;
use std::from_str::FromStr;
use syntax::ast;
use syntax::codemap;
use syntax::codemap::Pos;
use rustc::middle::ty;
use util::text_offset_to_line_pos;//todo - why is qualifying manually not working?!

// TODO:
// we've done many permuations of how to represent code position,
// 'codemap.rs' uses Loc { file:@FileMap, line:uint /* one based*/ col: CharPos
// we use zero-based indices, and a file index requiring caller passes a ty::cx around
// and a version where we propogate a filename instead.
//
// should we simplify out by reusing the codemap version ?
// or keep going with code here that is more  indexed / non shared-ptr based?
//
// also codemap.rs has 'span' which contains extra info , out use of span is really a subset
// Extents<BytePos> could replace it here ?


pub macro_rules! if_some {
    ($b:ident in $a:expr then $c:expr)=>(
        match $a {
            Some($b) => $c,
            None => {}
        }
    );
}

//pub type ZeroBasedIndex=uint;
pub struct ZTextFilePos {
    pub name: StrBuf,
    pub line: u32,
    pub col: u32
}

pub trait ToZTextFilePos {
    fn to_text_file_pos(self, cx: &ty::ctxt) -> Option<ZTextFilePos>;
}

impl ZIndexFilePos {
    pub fn to_scalar(&self) -> u64 {
        // TODO - safety assert
        (self.file_index as u64 << 48) | (self.line as u64 << 23) | (self.col as u64)
    }
}

impl Eq for ZIndexFilePos {
	fn eq(&self, other:&ZIndexFilePos)->bool { self.file_index==other.file_index && self.line==other.line && self.col==other.col }
	fn ne(&self, other:&ZIndexFilePos)->bool { self.file_index!=other.file_index || self.line!=other.line || self.col!=other.col}
}

impl Ord for ZIndexFilePos {
    // todo: as fixed width bignum? or int64 from int32 components?
    fn lt(&self, other: &ZIndexFilePos) -> bool {
        self.to_scalar() < other.to_scalar()
    }

    fn gt(&self, other: &ZIndexFilePos) -> bool {
        self.to_scalar() > other.to_scalar()
    }

    fn le(&self, other: &ZIndexFilePos) -> bool {
        self.to_scalar() <= other.to_scalar()
    }

    fn ge(&self, other: &ZIndexFilePos) -> bool {
        self.to_scalar() >= other.to_scalar()
    }
}

impl ToZTextFilePos for codemap::BytePos {
    fn to_text_file_pos(self, cx: &ty::ctxt) -> Option<ZTextFilePos> {
        let files = cx.sess.codemap().files.borrow();
        let mut i = files.len();

        while i > 0 {
            i -= 1;
            let fm = &files.get(i);
            if fm.start_pos <= self {
                let lines = fm.lines.borrow();
                let mut line = lines.len() as u32;
                while line > 0 {
                    line -= 1;
                    let line_start = lines.get(line as uint);
                    if line_start <= &self {
                        return Some(ZTextFilePos::new(fm.name.to_owned(), line, (self-*line_start).to_uint() as u32))
                    }
                }
            }
        }
        None
    }
}

impl FromStr for ZTextFilePos {
    fn from_str(file_pos_str: &str) -> Option<ZTextFilePos> {
        let toks: Vec<&str> = file_pos_str.split(':').collect();

        if toks.len() <= 0 {
            None
        } else if toks.len() == 1 {
            Some(ZTextFilePos::new(*toks.get(0), 0, 0))
        } else {
            match from_str::<u32>(*toks.get(1)) {
                None => None,
                Some(editor_line_number) => match FromStr::from_str(*toks.get(2)) {
                    None => Some(ZTextFilePos::new(*toks.get(0), editor_line_number - 1, 0)),
                    Some(col) => Some(ZTextFilePos::new(*toks.get(0), editor_line_number, col))
                }
            }
        }
    }
}

impl ZTextFilePos {
    pub fn new(filename: &str, _line: u32, _col: u32) -> ZTextFilePos {
        ZTextFilePos {name: filename.to_strbuf(), line: _line, col: _col}
    }

    pub fn to_str(&self) -> StrBuf {
        StrBuf::new().append(self.name.as_slice()).append(":")
			.append( (self.line + 1).to_str().as_slice())
			.append(":")
			.append(self.col.to_str().as_slice())
			.append(":")
    }

    pub fn to_byte_pos(&self, tc: &ty::ctxt) -> Option<codemap::BytePos> {
        let files = tc.sess.codemap().files.borrow();
        let mut i = files.len();
        while i > 0 {   // caution, need loop because we return, wait for new foreach ..in..
            i -= 1;
            let fm = &files.get(i);
            let filemap_filename: &str = fm.name.as_slice();
            if filemap_filename == self.name.as_slice() {
                let lines = fm.lines.borrow();
                if self.line as uint >= lines.len() {
                    return None;
                }
                return Some(codemap::BytePos(lines.get(self.line as uint).to_uint() as u32 + self.col));
            }
        }
        return None;
    }

    pub fn to_byte_pos_len(&self, tc: &ty::ctxt, len: u32)
    -> Option<(codemap::BytePos, codemap::BytePos)> {
        match self.to_byte_pos(tc) {
            None => None,
            Some(lo) => {
                // hmm, we dont know about clipping TODO: check its in range? clamp it?
                // is it line length, or length in file?
                Some((lo, lo + codemap::BytePos(len)))
            }
        }
    }

    pub fn get_str_at(&self, tc: &ty::ctxt, len: u32) -> StrBuf {
        let a = self.to_byte_pos_len(tc, len);
        match a {
            Some((bp_lo, bp_hi)) => get_span_str(tc,
                &codemap::Span {lo: bp_lo, hi: bp_hi, expn_info: None}),
            None => StrBuf::from_str("")
        }
    }
}

pub struct ZTextFilePosLen {
    pub tfp: ZTextFilePos,
    pub len: u32
}

impl ZTextFilePosLen {
    pub fn new(file_name: &str, _line: u32, _col: u32, _len: u32) -> ZTextFilePosLen {
        ZTextFilePosLen{ tfp: ZTextFilePos::new(file_name, _line, _col), len: _len }
    }

    pub fn to_byte_pos(&self, tc: &ty::ctxt) -> Option<(codemap::BytePos, codemap::BytePos)> {
//      text_file_pos_len_to_byte_pos(tc,&self.tfp, self.len)
        self.tfp.to_byte_pos_len(tc, self.len)
    }
    pub fn get_str(&self, tc: &ty::ctxt) -> StrBuf {
        self.tfp.get_str_at(tc, self.len)
    }
}

pub fn get_span_str(tc :&ty::ctxt, sp: &codemap::Span) -> StrBuf {
    let loc_lo = tc.sess.codemap().lookup_char_pos(sp.lo);
    // TODO-assert both in same file!
    let file_org = loc_lo.file.start_pos;
    let slice = loc_lo.file.src.as_slice().slice((sp.lo - file_org).to_uint(), (sp.hi - file_org).to_uint());
    slice.to_strbuf()
}


// fn get_str_at_text_file_pos_len(cx:ty::ctxt, tfp:&ZTextFilePos,len:uint)->StrBuf {


//}


/*
fn text_file_pos_len_to_byte_pos(c:ty::ctxt,tfp:&ZTextFilePos,len:uint=0 )->Option<(codemap::BytePos,codemap
::BytePos)>

{
//  for c.sess.codemap.files.rev_iter().advance |fm:&codemap::FileMap| {
    let mut i=c.sess.codemap.files.len();
    while i>0 { // caution, need loop because we return, wait for new foreach ..in..
        i-=1;
        let fm=&c.sess.codemap.files[i];
        let filemap_filename:&str=fm.name;
        if filemap_filename==tfp.name {
            let line_pos=*fm.lines[tfp.line];
            let bp_start=*fm.lines[tfp.line]+tfp.col;
            let bp_end=(bp_start+len).min(&(*fm.start_pos+fm.src.len()));
            return Some((codemap::BytePos(bp_start), codemap::BytePos(bp_end)))
        }
    }
    return None;
}
*/

/*
pub fn byte_pos_to_text_file_pos(c:ty::ctxt, pos:codemap::BytePos)->Option<ZTextFilePos> {
    // TODO: cleanup with byte_pos_to_index_file_pos, one in terms of the other.
    // TODO - functional, and with binary search or something ..
    let mut i=c.sess.codemap.files.len();
    while i>0 {
            // caution, need loop because we return, wait for new foreach ..in..
        i-=1;
        let fm=&c.sess.codemap.files[i];
        let filemap_filename:&str=fm.name;
        if *pos >= *fm.start_pos && *pos < *fm.start_pos+fm.src.len(){
            let mut line=fm.lines.len();
            while line>0 {
                line-=1;
                let lstart=*fm.lines[line];
                if lstart < *pos {
                    return Some(ZTextFilePos::new(fm.name, line, *pos-lstart))
                }
            }
        }
    }
    None
//  TextFilePos::new(c.sess.codemap.files[0].name,0,0)
}
*/

pub struct ZIndexFilePos {
    pub file_index: u32,
    pub line: u32,
    pub col: u32
}
pub trait ToZIndexFilePos {
    fn to_index_file_pos(&self, c: &ty::ctxt) -> Option<ZIndexFilePos>;
}

impl ToZIndexFilePos for codemap::BytePos {
    fn to_index_file_pos(&self, c: &ty::ctxt) -> Option<ZIndexFilePos> {
        // TODO: cleanup with byte_pos_to_text_file_pos, one in terms of the other.
        // TODO - functional, and with binary search or something ..
        let files = c.sess.codemap().files.borrow();
        let mut i = files.len() as u32;
        while i > 0 {
                // caution, need loop because we return, wait for new foreach ..in..
            i -= 1;
            let fm = &files.get(i as uint);
            if *self >= fm.start_pos && self.to_uint() < fm.start_pos.to_uint() + fm.src.len() {
                let lines = fm.lines.borrow();
                let mut line = lines.len() as u32;
                while line > 0 {
                    line -= 1;
                    let lstart = lines.get(line as uint);
                    if lstart < self {
                        return Some(
                            ZIndexFilePos{ file_index: i, line: line, col: (*self - *lstart).to_uint() as u32});
                    }
                }
            }
        }
        None
    }
}

pub fn get_crate_name(tc: &ty::ctxt, i: ast::CrateNum) -> StrBuf {
    if i > 0 {
        let cd = tc.sess.cstore.get_crate_data(i);
        cd.name.to_strbuf()
    } else {
        StrBuf::from_str("")
    }
}

pub fn text_span<'a, 'b>(text: &'a [u8], s: &'b codemap::Span) -> &'a[u8] {
    text.slice(s.lo.to_uint(), s.hi.to_uint())
}


pub fn dump_cstore_info(tc: &ty::ctxt) {
//struct ctxt_ {
//    cstore: @mut metadata::cstore::CStore,
//    def_map: resolve::DefMap,
//      tc.cstore.
// home/walter/gplsrc/rust/src/librustc/metadata/cstore.rs:37:
//pub struct CStore {
//    priv metas: HashMap <ast::CrateNum, @crate_metadata>,
//    priv extern_mod_crate_map: extern_mod_crate_map,
//    priv used_crate_files: ~[Path],
//    priv used_libraries: ~[@str],
//    priv used_link_args: ~[@str],
//    intr: @ident_interner
//}
// home/walter/gplsrc/rust/src/librustc/metadata/cstore.rs:30:
//pub struct crate_metadata {
//    name: @str,
//    data: @~[u8],
//    cnum_map: cnum_map,
//    cnum: ast::CrateNum
//}

    //println!("crate files");
    //let ucf = tc.cstore.get_used_crate_source();
    //for x in ucf.iter() {
    //  dump!(x);
    //}
/*  println!("crate metadata");
    for i in range(1,num_crates) {
        let cd= cstore::get_crate_data(tc.cstore, i as int);

        dump!(i, cd.name, cd.data.len(), cd.cnum_map, cd.cnum);
    }
*/
    println!("crate metadata");
    tc.sess.cstore.iter_crate_data(|i,md| {
        dump!(i, md.name, md.data.as_slice().len(), md.cnum);
    });
}
/*
pub fn flatten_to_str<T,U:ToStr>(xs:&[T],f:&fn(x:&T)->U, sep:&str)->StrBuf {
    let mut acc=~"";
    let mut i=0; // TODO - functional way.
    while i<xs.len() {
        if i>0 {acc.push_str(sep);}
        acc.push_str( f(&xs[i]).to_str() );

        i+=1;
    }
    acc
}
*/
pub fn loc_to_str(loc:codemap::Loc) -> StrBuf {
    StrBuf::from_str(loc.file.name.as_slice()).append(":").append(loc.line.to_str().as_slice()).append(":").append( loc.col.to_uint().to_str().as_slice() ).append( ":" )
}

pub fn zget_file_line_str(_: &ty::ctxt, _: &str, _: u32) -> StrBuf {
//  for c.sess.codemap.files.rev_iter().advance |fm:&codemap::FileMap| {
//  let mut i = cx.sess.codemap.files.len();
//  while i > 0 {   // caution, need loop because we return, wait for new foreach ..in..
//      i -= 1;
//      let fm = &cx.sess.codemap.files[i];
//      let filemap_filename: &str = fm.name;
//      if filename == filemap_filename {
//          let s = *fm.lines[src_line];
//          let e = if (src_line + 1) as uint >= fm.lines.len() {
//              *fm.start_pos + fm.src.len() as u32
//          } else {
//              *fm.lines[src_line + 1]
//          };
//      }
//  }
    return StrBuf::from_str("");
}

pub fn dump_span(text: &[u8], sp: &codemap::Span) {

    let line_col = text_offset_to_line_pos(text, sp.lo.to_uint() as u32);
    logi!(" line,ofs=", line_col.to_str(), " text=\'", str::from_utf8(text_span(text,sp)),"\'");
}


pub fn byte_pos_from_text_file_pos_str(dc:&RustFindCtx,filepos:&str)->Option<codemap::BytePos> {
    let toks=filepos.split(':').collect::<Vec<_> >();
    if toks.len()<3 { return None; }
//  let t0:()=toks[0];

//  let line:Option<uint> = FromStr::from_str(toks[1]);
//  if_some!(line in FromStr::from_str(toks[1]) then {
//      if_some!(col in FromStr::from_str(toks[2]) then {
    let line: Option<u32> = from_str(*toks.get(1));
    let col:Option<u32> = from_str(*toks.get(2));
    if line.is_some() && col.is_some() {
        //todo - if no column specified, just lookup everything on that line!
        let l0 = line.unwrap()-1;
        let c0= col.unwrap()-1;
        let foo= ZTextFilePos::new(*toks.get(0),l0,c0).to_byte_pos(dc.tycx_ref());
        return foo;
    }
    return None;
}
