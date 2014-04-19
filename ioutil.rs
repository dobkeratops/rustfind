#![macro_escape]

use std::io::{File, UserDir};
use std::io::fs::{mkdir_recursive,copy,walk_dir};
use std::io::{BufferedReader, IoResult};
use std::cast;
pub use std::io::{stdout, stdin};
pub use libc::{fwrite, fread, fseek, fopen, ftell, fclose, FILE, c_void, c_char, size_t,
  SEEK_END, SEEK_SET};
pub use std::mem::size_of;  // for size_of
pub use std::default::Default;
// TODO cleanup, we can just use the mature rust fileio now.

macro_rules! logi{
    ($($a:expr),*)=>(println!("{}", file!()+":"+line!().to_str()+": " $(+$a.to_str())* ))
}
//macro_rules! dump{ ($a:expr)=>(logi!(fmt!("%s=%?",stringify!($a),$a).indent(2,160));)}
/*fn newline_if_over(a:~str,l:uint) -> ~str {
    if a.len()>l {
        a+"\n"
    } else {
        a
    }
}*/
macro_rules! dump{ ($($a:expr),*)=>
    (   {   let mut txt=~"";
            $( txt=txt.append(
                format!("{:s}={:?}",stringify!($a),$a) + ",")
            );*;
            logi!(txt);
        }
    )
}


macro_rules! trace{
    ()=>(
        println!("{}", file!().to_str()+":"+line!().to_str()+": ");
    );
}

pub trait   Dbprint {fn dbprint(&self);}

pub trait EndianSwap {
    fn endian_swap(&self)->Self;
}

// dbprint postfix form means we can print tuples?
impl<T:ToStr> Dbprint for T {
    fn dbprint(&self) {
        println!("{}", self.to_str());
    }
}

pub fn promptInput(prompt:&str)->~str {
    println!("{}", prompt);
    BufferedReader::new(stdin()).read_line().unwrap() // TODO add error handling
}

pub fn as_void_ptr<T>(a:&T)->*c_void { unsafe {cast::transmute(a) } }
pub fn as_mut_void_ptr<T>(a:&T)->*mut c_void {unsafe { cast::transmute(a) } }

// this doest work?
pub trait VoidPtr {
    fn as_void_ptr(&self)->*c_void;
    fn as_mut_void_ptr(&self)->*mut c_void;
}
impl<T> VoidPtr for T {
    fn as_void_ptr(&self)->*c_void {unsafe { cast::transmute(self) } }
    fn as_mut_void_ptr(&self)->*mut c_void {unsafe { cast::transmute(self) } }
}

pub fn printStr<T:ToStr>(a:&T){println!("{}", a.to_str());}

pub fn c_str(rustStr:&str)->*c_char {
    unsafe {
        rustStr.to_c_str().unwrap()
    }
}


pub unsafe fn fileOpen(filename:&str,mode:&str)-> *FILE {
    fopen(c_str(filename),c_str(mode))
}

pub fn file_create_with_dirs(file_path: &Path) -> IoResult<File> {
    use std::io::{File, UserDir};
    use std::io::fs::mkdir_recursive;

    mkdir_recursive(&file_path.dir_path(), UserDir).and_then(|()| {
        File::create(file_path)
    }).map_err(|e| {
        println!("error: could not write to {} - {}", file_path.display(), e);
        e
    })
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
    fwrite(s as *T as *c_void, size_of::<T>() as size_t, 1, fp);
}


pub unsafe fn fileRead<T:Default+Clone>(fp:*FILE,numElems:size_t)->~[T] {
    let mut buffer: Vec<T> = Vec::from_elem(numElems as uint, Default::default());
    fread(buffer.as_mut_ptr() as *mut c_void,numElems,size_of::<T>() as size_t,fp);
    buffer.move_iter().collect()
}


pub unsafe fn fileReadBytes(fp:*FILE,numBytes:size_t)->~[u8] {
    fileRead(fp, numBytes)
}


pub unsafe fn fileSize(fp:*FILE)->size_t {
    fseek(fp,0,SEEK_END);
    let pos=ftell(fp);
    fseek(fp,0,SEEK_SET);
    pos as size_t
}


pub fn fileLoad(filename:&str)->~[u8] {
    unsafe {
        // TODO - should do with patter match null, fp?
        let fp= fileOpen(filename,"rb");
        if fp==0 as *FILE {
            printStr(&("could not read "+filename)); ~[]
        }
        else
        {   let buffer=fileReadBytes(fp,fileSize(fp));
            fclose(fp);
            buffer
        }
    }
}


pub unsafe fn fileWriteRange<T>(fp:*FILE, array:&[T],start:uint,end:uint) {
    printStr(&sizeofArray(array));
    fwrite(as_void_ptr(&array[start]),sizeofArrayElem(array)*(end-start) as size_t,1,fp);
}

pub fn sizeofArray<T>(a:&[T])->size_t { (size_of::<T>() * a.len()) as size_t }
pub fn sizeofArrayElem<T>(_:&[T])->size_t { size_of::<T>() as size_t }


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


pub fn fileSaveStr(text:&str, file_path: &Path) {
    let res = mkdir_recursive(&file_path.dir_path(), UserDir).and_then(|()| {
        let mut file = File::create(file_path);
        file.write_str(text)
    });
    match res {
        Ok(()) => (),
        Err(e) => println!("error: could not write to {} - {}", file_path.display(), e)
    };
}

pub fn copy_folder(source_dir: &Path, dest_dir: &Path)->Result<(),()> {
    let directories = walk_dir(source_dir);
    let res = match directories {
        Ok(mut directories) => {
            let mut result = Ok(());
            for file in directories {
                let mut dest_path = Path::new(dest_dir);
                for c in file.components().skip(1) {
                    dest_path.push(c);
                }
                let res = if file.is_dir () {
                    mkdir_recursive(&dest_path, UserDir)
                } else {
                    copy(&file, &dest_path)
                };
                match res {
                    Err(e) => {
                        result =  Err(e);
                        break;
                    },
                    _ => ()
                }
            };
            result
        },
        Err(e) => {
            println!("Unable to copy directory `{}`: {}", source_dir.display(), e);
            return Err(());
        }
    };
    match res {
        Err(e) => {println!("Error while copying: {}", e)return Err(()) },
        _ => return Ok(())
    }
}

pub trait ResultUtil<T> {
    fn expect(self, error_message: &'static str) -> T;
}

impl<T, U> ResultUtil<T> for Result<T, U> {
    fn expect(self, error_message: &'static str) -> T {
        match self {
            Ok(res) => res,
            Err(_) => fail!(error_message)
        }
    }
}
