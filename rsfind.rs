extern crate syntax;
extern crate rustc;
//extern crate extra;

//pub static ctxtkey: local_data::Key<@DocContext> = &local_data::Key;

pub macro_rules! if_some {
    ($b:ident in $a:expr then $c:expr)=>(
        match $a {
            Some($b)=>$c,
            None=>{}
        }
    );
}
pub macro_rules! tlogi{
    ($($a:expr),*)=>(println((file!()+":"+line!().to_str()+": " $(+$a.to_str())*) ))
}
pub macro_rules! logi{
    ($($a:expr),*)=>(println(""$(+$a.to_str())*) )
}
//macro_rules! dump{ ($a:expr)=>(logi!(fmt!("%s=%?",stringify!($a),$a).indent(2,160));)}
macro_rules! dump{ ($($a:expr),*)=>
    (   {   let mut txt=~"";
            $( { txt=txt.append(
                 format!("{:s}={:?}",stringify!($a),$a)+",")
                }
            );*;
            logi!(txt);
        }
    )
}

pub macro_rules! if_some {
    ($b:ident in $a:expr then $c:expr)=>(
        match $a {
            Some($b)=>$c,
            None=>{}
        }
    );
    ($b:ident in $a:expr then $c:expr _else $d:expr)=>(
        match $a {
            Some($b)=>$c,
            None=>{$d}
        }
    );
}


#[deriving(Clone, Eq, Encodable, Decodable)]
pub enum ShowDefMode {
    SDM_Line=0,
    SDM_LineCol=1,
    SDM_Source=2,
    SDM_GeditCmd=3
}


pub trait MyOption<T> {
    fn for_some(&self, f: |&T|);
    fn do_some<R>(&self, f: |&T| -> R) -> Option<R>;
}
impl<T> MyOption<T> for Option<T>{
    fn for_some(&self, f: |&T|) {
        match self {
            &None=>{},
            &Some(ref t)=>f(t)
        }
    }
    fn do_some<R>(&self, f: |&T| -> R)->Option<R> {
        match self {
            &None=>None,
            &Some(ref t)=>Some(f(t))
        }
    }
}








