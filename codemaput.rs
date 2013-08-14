use syntax::ast;
use syntax::codemap;
use rustc::middle::ty;

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
			Some($b)=>$c,
			None=>{}
		}
	);
}

//pub type ZeroBasedIndex=uint;
pub struct ZTextFilePos {
	name:~str,
	line:uint,
	col:uint
}
pub trait ToZTextFilePos {
	fn to_text_file_pos(self,cx:ty::ctxt)->Option<ZTextFilePos>;
}
impl ZIndexFilePos {
	pub fn to_scalar(&self)->u64 { 
		// TODO - safety assert
		(self.file_index as u64 <<48) | (self.line as u64<<23) | (self.col as u64)
	}
}

impl Ord for ZIndexFilePos {
	// todo: as fixed width bignum? or int64 from int32 components?
	fn lt(&self, other:&ZIndexFilePos)->bool { self.to_scalar()<other.to_scalar()}
	fn gt(&self, other:&ZIndexFilePos)->bool { self.to_scalar()>other.to_scalar()}
	fn le(&self, other:&ZIndexFilePos)->bool { self.to_scalar()<=other.to_scalar()}
	fn ge(&self, other:&ZIndexFilePos)->bool { self.to_scalar()>=other.to_scalar()}
}

impl ToZTextFilePos for codemap::BytePos {
	fn to_text_file_pos(self, cx:ty::ctxt)->Option<ZTextFilePos> {
		let mut i=cx.sess.codemap.files.len();
		while i>0 {
			i-=1;
			let fm=&cx.sess.codemap.files[i];
			if *fm.start_pos <= *self {
				let mut line=fm.lines.len();
				while line>0 {
					line-=1;
					let line_start=*fm.lines[line];
					if line_start<=*self {
						return Some(ZTextFilePos::new(fm.name.to_owned(), line,*self-line_start))
					}
				}
			}
		}
		None
	}
}

impl FromStr for ZTextFilePos {
	fn from_str(file_pos_str:&str)->Option<ZTextFilePos> {
		let toks:~[&str]=file_pos_str.split_iter(':').collect();
		if toks.len()<=0 {
			None 
		} else if toks.len()==1 {
			Some(ZTextFilePos::new(toks[0],0,0))
		} else {
			match FromStr::from_str::<uint>(toks[1]) {
				None=>None,
				Some(editor_line_number)=>match FromStr::from_str(toks[2]) {
					None=>Some(ZTextFilePos::new(toks[0],editor_line_number-1,0)),
					Some(col)=>Some(ZTextFilePos::new(toks[0],editor_line_number,col))
				}
			}
		}
	}
}

impl ZTextFilePos {
	pub fn new(filename:&str,_line:uint,_col:uint)->ZTextFilePos { ZTextFilePos{name:filename.to_owned(),line:_line,col:_col}}

	pub fn to_str(&self)->~str {
		self.name+":"+(self.line+1).to_str()+":"+self.col.to_str()+":"		
	}

	pub fn to_byte_pos(&self,cx:ty::ctxt)->Option<codemap::BytePos> {
		let mut i=cx.sess.codemap.files.len();
		while i>0 {	// caution, need loop because we return, wait for new foreach ..in..
			i-=1;
			let fm=&cx.sess.codemap.files[i];
			let filemap_filename:&str=fm.name;	
			if filemap_filename==self.name {
				if self.line>=fm.lines.len() { return None;}
				return Some(codemap::BytePos(*fm.lines[self.line]+self.col));
			}
		}
		return None;
	}
	pub fn to_byte_pos_len(&self, cx:ty::ctxt,len:uint)->Option<(codemap::BytePos,codemap::BytePos)> {
		match self.to_byte_pos(cx) {
			None=>None,
			Some(lo)=>{
				// hmm, we dont know about clipping TODO: check its in range? clamp it?
				// is it line length, or length in file?
				Some((lo,lo+codemap::BytePos(len)))
			}
		}
	}
	
	
	pub fn get_str_at(&self, cx:ty::ctxt, len:uint)->~str {
		let a=//text_file_pos_len_to_byte_pos(cx, self,len);
				self.to_byte_pos_len(cx,len);
		match  a  {
			Some((bp_lo,bp_hi))=>get_span_str(cx,
				&codemap::span{lo:bp_lo,hi:bp_hi,expn_info:None}
			),
			None=>~""
		}
	}
}

pub struct ZTextFilePosLen {
	tfp:ZTextFilePos,
	len:uint
}
impl ZTextFilePosLen {
	pub fn new(file_name:&str,_line:uint,_col:uint, _len:uint)->ZTextFilePosLen {
		ZTextFilePosLen{ tfp:ZTextFilePos::new(file_name,_line,_col), len:_len }
	}
	
	pub fn to_byte_pos(&self,tc:ty::ctxt)->Option<(codemap::BytePos,codemap::BytePos)> {
//		text_file_pos_len_to_byte_pos(tc,&self.tfp, self.len)
		self.tfp.to_byte_pos_len(tc,self.len)
	}
	pub fn get_str(&self, tc:ty::ctxt)->~str {
		self.tfp.get_str_at(tc,self.len)
	}
}

pub fn get_span_str(c:ty::ctxt, sp:&codemap::span)->~str {
	let loc_lo=c.sess.codemap.lookup_char_pos(sp.lo);
	let loc_hi=c.sess.codemap.lookup_char_pos(sp.hi);
	// TODO-assert both in same file!
	let file_org=*loc_lo.file.start_pos;
	let slice=loc_lo.file.src.slice(*sp.lo-file_org, *sp.hi-file_org );
	slice.to_str()
}


// fn get_str_at_text_file_pos_len(cx:ty::ctxt, tfp:&ZTextFilePos,len:uint)->~str {


//}


/*
fn text_file_pos_len_to_byte_pos(c:ty::ctxt,tfp:&ZTextFilePos,len:uint=0 )->Option<(codemap::BytePos,codemap::BytePos)>

{
//	for c.sess.codemap.files.rev_iter().advance |fm:&codemap::FileMap| {
	let mut i=c.sess.codemap.files.len();
	while i>0 {	// caution, need loop because we return, wait for new foreach ..in..
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
//	TextFilePos::new(c.sess.codemap.files[0].name,0,0)
}
*/

pub struct ZIndexFilePos {
	file_index:uint,
	line:uint,
	col:uint
}
pub trait ToZIndexFilePos {
	fn to_index_file_pos(&self,c:ty::ctxt)->Option<ZIndexFilePos>;
}

impl ToZIndexFilePos for codemap::BytePos {
	fn to_index_file_pos(&self, c:ty::ctxt)->Option<ZIndexFilePos> {
		// TODO: cleanup with byte_pos_to_text_file_pos, one in terms of the other.
		// TODO - functional, and with binary search or something ..
		let mut i=c.sess.codemap.files.len();
		while i>0 {
				// caution, need loop because we return, wait for new foreach ..in..
			i-=1;
			let fm=&c.sess.codemap.files[i];
			let filemap_filename:&str=fm.name;
			if **self >= *fm.start_pos && **self < *fm.start_pos+fm.src.len(){
				let mut line=fm.lines.len();
				while line>0 {
					line-=1;
					let lstart=*fm.lines[line];
					if lstart < **self {
						return Some(ZIndexFilePos{ file_index:i, line:line, col:**self-lstart});
					}
				}
			}
		}	
		None
	}
}





