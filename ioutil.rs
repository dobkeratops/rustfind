#[macro_escape];

pub use std::io::*;
pub use std::str::*;
pub use std::libc::*;
pub use std::ptr::*;
pub use std::sys::*;	// for size_of
pub use std::vec::*;
pub use std::num::*;
pub type Size_t=u64;	// todo - we're not sure this should be u64 
						// as the libc stuff seems to want.
						// should it be uint?


macro_rules! logi{ 
	($($a:expr),*)=>(println(file!()+":"+line!().to_str()+": " $(+$a.to_str())* ))
}
//macro_rules! dump{ ($a:expr)=>(logi!(fmt!("%s=%?",stringify!($a),$a).indent(2,160));)}
fn newline_if_over(a:~str,l:uint)->~str{if a.len()>l {a+~"\n"}else{a}}
macro_rules! dump{ ($($a:expr),*)=>
	(	{	let mut txt=~""; 
			$( txt=txt.append(
				fmt!("%s=%?",stringify!($a),$a)+~",") 
			);*; 
			logi!(txt); 
		}
	)
}


macro_rules! trace{
	()=>(
		println(file!().to_str()+":"+line!().to_str()+": ");
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
	stdout().write(prompt.as_bytes()); stdin().read_line()
}

pub fn as_void_ptr<T>(a:&T)->*c_void { to_unsafe_ptr(a) as *c_void}
pub fn as_mut_void_ptr<T>(a:&T)->*mut c_void { to_unsafe_ptr(a) as *mut c_void}

// this doest work?
pub trait VoidPtr {
	fn as_void_ptr(&self)->*c_void;
	fn as_mut_void_ptr(&self)->*mut c_void;
}
impl<T> VoidPtr for T {
	fn as_void_ptr(&self)->*c_void { to_unsafe_ptr(&self) as *c_void}
	fn as_mut_void_ptr(&self)->*mut c_void { to_unsafe_ptr(self) as *mut c_void}
}

pub fn printStr<T:ToStr>(a:&T){println(a.to_str());}

pub fn c_str(rustStr:&str)->*c_char {
//	as_c_str(rustStr,|x|x)
	rustStr.as_c_str(|x|x)
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
	fwrite(as_void_ptr(&array[0]),sizeofArray(array),1,fp);
}
pub unsafe fn fileWriteStruct<T>(fp:*FILE, s:&T) {
	fwrite(as_void_ptr(s),size_of::<T>() as Size_t,1,fp);
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
	fwrite(as_void_ptr(&array[start]),sizeofArrayElem(array)*(end-start).to_u64(),1,fp);
}

pub fn sizeofArray<T>(a:&[T])->Size_t { (size_of::<T>() * a.len()).to_u64() }
pub fn sizeofArrayElem<T>(_:&[T])->Size_t { size_of::<T>().to_u64() }

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



