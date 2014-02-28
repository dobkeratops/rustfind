#[macro_escape];

pub use std;
pub use std::io;
pub use std::io::{stdout, stdin,println};
pub use std::libc::{fwrite, fread, fseek, fopen, ftell, fclose, FILE, c_void, c_char, SEEK_END,
	SEEK_SET};
pub use std::mem::size_of;	// for size_of
pub use std::vec::from_elem;
pub use std::num::Zero;
use std::io::buffered::BufferedReader;

pub type Size_t=u64;	// todo - we're not sure this should be u64
						// as the libc stuff seems to want.
						// should it be uint?


macro_rules! logi{
	($($a:expr),*)=>(std::io::println(file!()+":"+line!().to_str()+": " $(+$a.to_str())* ))
}
//macro_rules! dump{ ($a:expr)=>(logi!(fmt!("%s=%?",stringify!($a),$a).indent(2,160));)}
fn newline_if_over(a:~str,l:uint)->~str{if a.len()>l {a+"\n"}else{a}}
macro_rules! dump{ ($($a:expr),*)=>
	(	{	let mut txt=~"";
			$( txt=txt.append(
				format!("{:s}={:?}",stringify!($a),$a) + ",")
			);*;
			logi!(txt);
		}
	)
}


macro_rules! trace{
	()=>(
		std::io::println(file!().to_str()+":"+line!().to_str()+": ");
	);
}

pub trait	Dbprint {fn dbprint(&self);}

pub trait EndianSwap {
	fn endian_swap(&self)->Self;
}

// dbprint postfix form means we can print tuples?
impl<T:ToStr> Dbprint for T {
	fn dbprint(&self) {
		println(self.to_str());
	}
}

pub fn promptInput(prompt:&str)->~str {
	stdout().write(prompt.as_bytes());
	BufferedReader::new(stdin()).read_line().expect("read_line failure")
}

pub fn as_cvoid_ptr<T>(a:&T)->*c_void { a.as_void_ptr(a) as *c_void}
pub fn as_mut_void_ptr<T>(a:&T)->*mut c_void { a.as_void_ptr() as *mut c_void}

// this doest work?
pub trait VoidPtr {
	fn as_void_ptr(&self)->*c_void;
	fn as_mut_void_ptr(&self)->*mut c_void;
}
impl<T> VoidPtr for T {
	fn as_void_ptr(&self)->*c_void { self.as_void_ptr() as *c_void}
	fn as_mut_void_ptr(&self)->*mut c_void { self.as_void_ptr() as *mut c_void}
}

pub fn printStr<T:ToStr>(a:&T){println(a.to_str());}

pub fn c_str(rustStr:&str)->*c_char {
	unsafe {
//	as_c_str(rustStr,|x|x)
		rustStr.to_c_str().unwrap()
	}
}


pub unsafe fn fileOpen(filename:&str,mode:&str)-> *FILE {
	fopen(c_str(filename),c_str(mode))
}
/*
pub fn fileLoadArray<T>(filename:&str)->~[T] {
	unsafe {
		let fp=fopen(c_str_from(filename),as_c_str("rb",|x|x));
	}
}
*/


pub unsafe fn fileWrite<T>(fp:*FILE, array:&[T]) {
	printStr(&sizeofArray(array));
	fwrite(as_cvoid_ptr(&array[0]),sizeofArray(array),1,fp);
}


pub unsafe fn fileWriteStruct<T>(fp:*FILE, s:&T) {
	fwrite(as_cvoid_ptr(s),size_of::<T>() as Size_t,1,fp);
}


pub unsafe fn fileRead<T:Zero+Clone>(fp:*FILE,numElems:Size_t)->~[T] {
	let buffer=from_elem(numElems as uint, Zero::zero());
	fread(as_mut_void_ptr(&buffer[0]),numElems,size_of::<T>() as Size_t,fp);
	buffer
}


pub unsafe fn fileReadBytes(fp:*FILE,numBytes:Size_t)->~[u8] {
	// todo - simply express as the above..
	let buffer=from_elem(numBytes as uint,0 as u8);
	fread(as_mut_void_ptr(&buffer[0]),numBytes,1,fp);
	buffer
}


pub unsafe fn fileSize(fp:*FILE)->Size_t {
	fseek(fp,0,SEEK_END);
	let pos=ftell(fp);
	fseek(fp,0,SEEK_SET);
	pos as Size_t
}


pub fn fileLoad(filename:&str)->~[u8] {
	unsafe {
		// TODO - should do with patter match null, fp?
		let fp= fileOpen(filename,"rb");
		if fp==0 as *FILE {
			printStr(&("could not read "+filename)); ~[]
		}
		else
		{	let buffer=fileReadBytes(fp,fileSize(fp));
			fclose(fp);
			buffer
		}
	}
}


pub unsafe fn fileWriteRange<T>(fp:*FILE, array:&[T],start:uint,end:uint) {
	printStr(&sizeofArray(array));
	fwrite(as_cvoid_ptr(&array[start]),sizeofArrayElem(array)*(end-start) as Size_t,1,fp);
}

pub fn sizeofArray<T>(a:&[T])->Size_t { (size_of::<T>() * a.len()) as Size_t }
pub fn sizeofArrayElem<T>(_:&[T])->Size_t { size_of::<T>() as Size_t }


pub fn fileSaveArray<T>(buffer:&[T],filename:&str) {
	unsafe {
		let fp=fileOpen(filename,"wb");
		if fp!=(0 as *FILE) {
			fileWrite(fp,buffer);
			//fwrite(to_void_ptr(&buffer[0]),sizeofArray(buffer),1,fp);
			fclose(fp);
		} else {
			printStr(&("could not write "+filename));
		}
	}
}


pub fn fileSaveStr(text:&str,filename:&str) {
	unsafe {
		let fp=fileOpen(filename,"wb");
		if fp!=(0 as *FILE) {
			fwrite(c_str(text) as *c_void,text.len() as u64,1,fp);

			//fwrite(to_void_ptr(&buffer[0]),sizeofArray(buffer),1,fp);
			fclose(fp);
		} else {
			printStr(&("could not write "+filename));
		}
	}
}




