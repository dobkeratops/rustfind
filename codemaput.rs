use syntax::ast;
use syntax::codemap;
use rustc::middle::ty;

// todo .. find if these helper structs/impls exist already in the rust sourcebase
//

pub macro_rules! if_some {
	($b:ident in $a:expr then $c:expr)=>(
		match $a {
			Some($b)=>$c,
			None=>{}
		}
	);
}

pub struct TextFilePos {
	name:~str,
	line:uint,
	col:uint
}
pub trait ToTextFilePos {
	pub fn to_text_file_pos(self,cx:ty::ctxt)->Option<TextFilePos>;
}

impl ToTextFilePos for codemap::BytePos {
	pub fn to_text_file_pos(self, cx:ty::ctxt)->Option<TextFilePos> {
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
						return Some(TextFilePos::new(fm.name.to_owned(), line+1,*self-line_start))
					}
				}
			}
		}
		None
	}
}

impl FromStr for TextFilePos {
	pub fn from_str(file_pos_str:&str)->Option<TextFilePos> {
		let toks:~[&str]=file_pos_str.split_iter(':').collect();
		if toks.len()<=0 {
			None 
		} else if toks.len()==1 {
			Some(TextFilePos::new(toks[0],1,0))
		} else {
			match FromStr::from_str(toks[1]) {
				None=>None,
				Some(line)=>match FromStr::from_str(toks[2]) {
					None=>Some(TextFilePos::new(toks[0],line,0)),
					Some(col)=>Some(TextFilePos::new(toks[0],line,col))
				}
			}
		}
	}
}

impl TextFilePos {
	pub fn new(filename:&str,_line:uint,_col:uint)->TextFilePos { TextFilePos{name:filename.to_owned(),line:_line,col:_col}}

	pub fn to_str(&self)->~str {
		self.name+":"+self.line.to_str()+":"+self.col.to_str()+":"		
	}

	pub fn to_byte_pos(&self,cx:ty::ctxt)->Option<codemap::BytePos> {
		let mut i=cx.sess.codemap.files.len();
		while i>0 {	// caution, need loop because we return, wait for new foreach ..in..
			i-=1;
			let fm=&cx.sess.codemap.files[i];
			let filemap_filename:&str=fm.name;	
			if filemap_filename==self.name {
				if self.line>fm.lines.len() { return None;}
				return Some(codemap::BytePos(*fm.lines[self.line-1]+self.col));
			}
		}
		return None;
	}
}


pub fn text_file_pos_to_byte_pos(c:ty::ctxt,tfp:&TextFilePos)->Option<codemap::BytePos>
{
//	for c.sess.codemap.files.rev_iter().advance |fm:&codemap::FileMap| {
	let mut i=c.sess.codemap.files.len();
	while i>0 {	// caution, need loop because we return, wait for new foreach ..in..
		i-=1;
		let fm=&c.sess.codemap.files[i];
		let filemap_filename:&str=fm.name;	
		if filemap_filename==tfp.name {
			let line_pos=*fm.lines[tfp.line-1];
			let bp_start=*fm.lines[tfp.line-1]+tfp.col;
			return Some(codemap::BytePos(bp_start))
		}
	}
	return None;
}

pub fn byte_pos_to_text_file_pos(c:ty::ctxt, pos:codemap::BytePos)->TextFilePos {
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
					return TextFilePos::new(fm.name, line, *pos-lstart)
				}
			}
		}
	}	
	TextFilePos::new(c.sess.codemap.files[0].name,0,0)
}

pub struct IndexFilePos {
	file_index:uint,
	line:uint,
	col:uint
}

pub fn byte_pos_to_index_file_pos(c:ty::ctxt, pos:codemap::BytePos)->Option<IndexFilePos> {
	// TODO: cleanup with byte_pos_to_text_file_pos, one in terms of the other.
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
					return Some(IndexFilePos{ file_index:i, line:line, col:*pos-lstart});
				}
			}
		}
	}	
	None
}




