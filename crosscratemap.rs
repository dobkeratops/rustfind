use std::io::println;
use std::path::posix;
use rf_common::*;
use syntax::ast;
use syntax::codemap::Pos;
use find_ast_node::FNodeInfoMap;
use jumptodefmap::{JumpToDefMap};
use codemaput::ToZTextFilePos;
use ioutil;
use rfindctx::{RustFindCtx, str_of_opt_ident};

/*new file*/

pub type ZeroBasedIndex=uint;

/// cross crate map, extra info written out when compiling links of a crate
/// allows sunsequent crates to jump to definitions in that crate
/// TODO - check if this already exists in the 'cstore/create metadata'
/// specificially we need node->span info
#[deriving(Clone)]
pub struct CrossCrateMapItem {
	pub item_name:Option<~str>,
    pub file_name:~str,
    pub line:ZeroBasedIndex,
    pub col:uint,
    pub len:uint
}

/// Cross Crate Map - resolves file locations from DefIds, across crates
/// also caches item paths
pub type CrossCrateMap = HashMap<ast::DefId,CrossCrateMapItem>;


fn get_def_id_name(xcm:&CrossCrateMap, def_id:&ast::DefId)->~str {
	xcm.find(def_id).map(|x|x.item_name.clone().unwrap_or(~"")).unwrap_or(~"")
}



pub fn read_cross_crate_map(crate_num:int, crate_name:&str,lib_path:&str)->~CrossCrateMap {
    let mut raw_bytes=ioutil::fileLoad(crate_name);
    if raw_bytes.len()==0 {
        println("loading lib crosscratemap "+lib_path+"/"+crate_name);
        raw_bytes=ioutil::fileLoad(lib_path+"/"+crate_name);
		if raw_bytes.len()<=0 {
			println("must run rustfind in library directories if you want links to work (needs html & .rfx files)\n");
		}
    }
    let rfx=str::from_utf8(raw_bytes);
    println("loaded cratemap "+rfx.get_ref().len().to_str()+" bytes"+" as crate "+crate_num.to_str());
//  for &x in raw_bytes.iter() { rfx.push_char(x as char); }

    let mut xcm=~HashMap::new();
    for s in rfx.get_ref().lines() {
//      println(s.to_str());
        let toks=s.split('\t').collect::<~[&str]>();
        if toks.len()>=6 {
            match toks[0] {
                "jdef"=> {
                    // jimp- to def info, we dont need this here as we already generated it
                    // for the current crate. TODO , genarlized rfx would use it..
                }
                "node"=> {
                    // node cratename nodeid parentid sourcefile line col len type [ident]
                    //cratename is ignoredd, because we already know it.
                    // parent id ignored, we use span information to reconstruct AST

                    let node_id: int= from_str::<int>(toks[2]).unwrap_or(0);
                    xcm.insert(ast::DefId{krate:crate_num as u32, node:node_id as u32,},
                        CrossCrateMapItem{
							item_name:	toks.iter().nth(9).map(|x|x.to_owned()),
                            file_name:  toks[4].to_owned(),
                            line:   from_str(toks[5]).unwrap_or(0)-1,
                            col:    from_str(toks[6]).unwrap_or(0),
                            len:    from_str(toks[7]).unwrap_or(0)
                        }
                    );
                }
                // legacy noode definitons,no keyword
                _=>{

                    let node_id:int=from_str(toks[1]).unwrap_or(0);
                    xcm.insert(ast::DefId{krate:crate_num as u32, node:node_id as u32,},
                        CrossCrateMapItem{
							item_name:	toks.iter().nth(9).map(|x|x.to_owned()),
                            file_name:  toks[2].to_owned(),
                            line:   from_str(toks[3]).unwrap_or(0)-1,
                            col:    from_str(toks[4]).unwrap_or(0),
                            len:    from_str(toks[5]).unwrap_or(0)
                        }
                    );
                }
            }
        }
    }
    //dump!(xcm);
    println("from cratemap "+rfx.get_ref().len().to_str()+" bytes");
    xcm
}
pub fn cross_crate_map_combine(dst:&mut CrossCrateMap, src:&CrossCrateMap) {
	for (k,v) in src.iter() {
		dst.insert(*k,(*v).clone());
	}
}
pub fn cross_crate_map_read_into(dst:&mut CrossCrateMap,crate_num:int, crate_name:&str,lib_path:&str){
	let xcm_sub=read_cross_crate_map(crate_num, crate_name, lib_path);
	cross_crate_map_combine(dst, xcm_sub);
}

pub fn cross_crate_map_combine_current_crate(dst:&mut CrossCrateMap,dc:&RustFindCtx, nim:&FNodeInfoMap, def_map:&HashMap<ast::NodeId, ast::DefId>, jdm:&JumpToDefMap) {
/*
	for (k,ni) in nim.iter() {
		match ni.span.lo.to_text_file_pos(dc.tycx_ref()) {
			Some(tfp)=>{
			// new format, a little more verbose,
			// and includes parent id for easier reconstruction of full AST
			// "node" cratename id parent_id filename line col len type [ident]
			if new_format {
//				try!(writeln!(&mut out_file, "node\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
					curr_crate_name_only, 
					k, 
					ni.parent_id, 
					tfp.name, 
					(tfp.line + 1), 
					tfp.col,
		            (ni.span.hi - ni.span.lo).to_uint(),
					ni.kind,
					str_of_opt_ident(ni.ident)));
			}
		}
	}
*/
}

fn get_crate_name(dc:&RustFindCtx)->(posix::Path,~str) {
    let crate_rel_path_name= dc.codemap().files.borrow();
    let crate_rel_path_name = Path::new(crate_rel_path_name.get(0).name.as_slice());
    let curr_crate_name_only = crate_rel_path_name.filestem_str().unwrap_or("");
	(crate_rel_path_name.clone(),curr_crate_name_only.to_owned())
}

pub fn cross_crate_map_write(dc:&RustFindCtx, _:&str,nim:&FNodeInfoMap, _:&HashMap<ast::NodeId, ast::DefId>, jdm:&JumpToDefMap) {
    // write inter-crate node map
    let new_format:bool=true;

	let (crate_rel_path_name, curr_crate_name_only) = get_crate_name(dc);
    println!("Writing rustfind cross-crate link info for {}", curr_crate_name_only);
    let out_path = crate_rel_path_name.with_extension("rfx");
    let out = ioutil::file_create_with_dirs(&out_path).map(|out| {
        let mut out_file = out;
        // todo - idents to a seperate block, they're rare.
        for (k,ni) in nim.iter() {
            match ni.span.lo.to_text_file_pos(dc.tycx_ref()) {
                Some(tfp)=>{
                    // new format, a little more verbose,
                    // and includes parent id for easier reconstruction of full AST
                    // "node" cratename id parent_id filename line col len type [ident]
                    if new_format {
                        try!(writeln!(&mut out_file, "node\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
                            curr_crate_name_only, k, ni.parent_id, tfp.name, (tfp.line + 1), tfp.col,
                            (ni.span.hi - ni.span.lo).to_uint(), ni.kind, str_of_opt_ident(ni.ident)));
                    } else  {
                        // old format, relies on spans to reconstruct AST.
                        // cratename id filename line col len type [ident]
                        try!(writeln!(&mut out_file, "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
                            curr_crate_name_only, k, tfp.name, (tfp.line+1), tfp.col,
                            (ni.span.hi-ni.span.lo).to_uint(), ni.kind, str_of_opt_ident(ni.ident)));
                    }
                },
                None=>{}
            }
        }

        for (k,v) in jdm.iter()  {
            let cname: ~str = if v.krate > 0 {
                dc.cstore().get_crate_data(v.krate).name.to_str()
            } else {
                curr_crate_name_only.to_str()
            };
            //println(cdata.name);
            try!(writeln!(&mut out_file, "jdef\t{}\t{}\t{}", k, cname, v.node));
        }

        Ok(())
    });

    match out {
        Err(e) => println!("Error while writing to {}: {}", out_path.display(), e),
        _ => ()
    };

//  for (k,v) in ndm.iter()  {
//      outp.push_str("def\t"+k.to_str()+"\t"+dc.tycx.cstore.crate() +v.node.to_str()+"\n");
//  }
}
