use syntax::ast;
use std::num;
use std::num::*;
use std::str;
use std::int;
use std::uint;
use std::hashmap::HashMap;
use rustc::metadata::cstore;
use rfindctx::{str_of_opt_ident};
use find_ast_node::FNodeInfoMap;
use jumptodefmap::*;
use ioutil::*;
use rfindctx::*;
/*new file*/  

pub type ZeroBasedIndex=uint;

/// cross crate map, extra info written out when compiling links of a crate
/// allows sunsequent crates to jump to definitions in that crate
/// TODO - check if this already exists in the 'cstore/create metadata'
/// specificially we need node->span info
#[deriving(Clone)]
pub struct CrossCrateMapItem {
	fname:~str,
	line:ZeroBasedIndex,	
	col:uint,
	len:uint
}

pub type CrossCrateMap = HashMap<ast::DefId,CrossCrateMapItem>;


pub fn read_cross_crate_map(dc:&RFindCtx, crate_num:int, crate_name:&str,lib_path:&str)->~CrossCrateMap {
	let mut raw_bytes=fileLoad(crate_name);
	if (raw_bytes.len()==0) {
		println("loading lib crosscratemap "+lib_path+"/"+crate_name);
		raw_bytes=fileLoad(lib_path+"/"+crate_name);
	}
	let rfx=str::from_utf8(raw_bytes);
	println("loaded cratemap "+rfx.len().to_str()+"bytes"+" as crate "+crate_num.to_str());
//	for &x in raw_bytes.iter() { rfx.push_char(x as char); }

	let mut xcm=~HashMap::new();
	for s in rfx.line_iter() {
//		println(s.to_str());
		let toks=s.split_iter('\t').to_owned_vec();
		if toks.len()>=6 {
			match toks[0] {
				"jdef"=> {
					// jimp- to def info, we dont need this here as we already generated it
					// for the current crate. TODO , genarlized rfx would use it..
				}
				"node"=> {// node cratename nodeid parentid sourcefile line col len type [ident]
					//cratename is ignoredd, because we already know it.
					// pareent id ignored, we use span information to reconstruct AST

					let node_id:int=int::from_str(toks[2]).unwrap_or_default(0);
					xcm.insert(ast::DefId{crate:crate_num, node:node_id,},
						CrossCrateMapItem{
							fname:	toks[4].to_owned(),
							line:   uint::from_str(toks[5]).unwrap_or_default(0)-1,
							col:	uint::from_str(toks[6]).unwrap_or_default(0),
							len:	uint::from_str(toks[7]).unwrap_or_default(0)
						}
					);
				}
				// legacy noode definitons,no keyword
				_=>{

					let node_id:int=int::from_str(toks[1]).unwrap_or_default(0);
					xcm.insert(ast::DefId{crate:crate_num, node:node_id,},
						CrossCrateMapItem{
							fname:	toks[2].to_owned(),
							line:   uint::from_str(toks[3]).unwrap_or_default(0)-1,
							col:	uint::from_str(toks[4]).unwrap_or_default(0),
							len:	uint::from_str(toks[5]).unwrap_or_default(0)
						}
					);
				}
			}
		}
	}
	//dump!(xcm);
	println("from cratemap "+rfx.len().to_str()+"bytes");
	xcm
}



pub fn write_cross_crate_map(dc:&RFindCtx,lib_html_path:&str,nim:&FNodeInfoMap, ndm:&HashMap<ast::NodeId, ast::DefId>, jdm:&JumpToDefMap) {
	// write inter-crate node map
	let crate_rel_path_name= dc.sess.codemap.files[0].name;
	let new_format:bool=true;

	let curr_crate_name_only=crate_rel_path_name.split_iter('/').last().unwrap_or_default("").split_iter('.').nth(0).unwrap_or_default("");
	println("writing rustfind cross-crate link info for "+curr_crate_name_only);
	let mut outp=~"";
	// todo - idents to a seperate block, they're rare.
	for (k,ni) in nim.iter() {
		match ni.span.lo.to_text_file_pos(dc.tycx) {
			Some(tfp)=>{
				// new format, a little more verbose, 
				// "node" cratename id parent_id filename line col len type [ident]
				// and includes parnet id for easier reconstruction of full AST
				if new_format {
					outp.push_str(
							"node\t"+
							curr_crate_name_only+"\t"+k.to_str()+"\t"+ni.parent_id.to_str()+"\t"+tfp.name+"\t"+(tfp.line+1).to_str()+"\t"+tfp.col.to_str()+"\t"+(*ni.span.hi-*ni.span.lo).to_str() + "\t"+ni.kind+ "\t"+str_of_opt_ident(dc,ni.ident)+"\n");
				} else 	{
					// old format, relies on spans to reconstruct AST.
					// cratename id filename line col len type [ident]
					outp.push_str(curr_crate_name_only+"\t"+k.to_str()+"\t"+tfp.name+"\t"+(tfp.line+1).to_str()+"\t"+tfp.col.to_str()+"\t"+(*ni.span.hi-*ni.span.lo).to_str() + "\t"+ni.kind+ "\t"+str_of_opt_ident(dc,ni.ident)+"\n");
				}
			},
			None=>{}
		}
	}
	
	for (k,v) in jdm.iter()  {
		let cname:~str= if v.crate>0 {
			cstore::get_crate_data(dc.tycx.cstore,v.crate).name.to_str()
		} else {
			curr_crate_name_only.to_str()
		};
		//println(cdata.name);
		outp.push_str("jdef\t"+k.to_str()+"\t"+cname+"\t" +v.node.to_str()+"\n");
	}	

//	for (k,v) in ndm.iter()  {
//		outp.push_str("def\t"+k.to_str()+"\t"+dc.tycx.cstore.crate() +v.node.to_str()+"\n");
//	}	

	
	{	let x=curr_crate_name_only+~".rfx";
		println("writing "+x);
		fileSaveStr(outp, x);
	}	
}
