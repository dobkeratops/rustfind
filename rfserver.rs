use rfindctx::{RFindCtx,find_file_name_in,first_file_name};
use jumptodefmap::{dump_json,lookup_def_at_text_file_pos_str};
use std::io;
use std::io::BufferedReader;
use rsfind::{SDM_Source};
use ioutil::ResultUtil;
/*
interactive mode, also server for IDE integration with command interface
*/

pub fn run_server(dc:&RFindCtx) {
	// TODO - check if RUSTI can already do this.. it would be better there IMO
    // todo _ why is super needed here?!

    // Currently unused
// 	let (node_spans,node_def_node,_)=jumptodefmap::make_jdm(dc);

	let mut curr_file=first_file_name(dc);

	loop {
		print!("rustfind {}>", curr_file);
		let input_line=BufferedReader::new(io::stdin()).read_line().expect("read_line failed on stdin");
		let toks:~[&str]=input_line.split(' ').collect();
		if toks.len()>0 {
			match toks[0] {
				"h"|"help"=> println!("interactive mode\n - enter file:line:pos or line:pos for current file\n - show location & def of symbol there\n j-dump json q-quit i-info"),
				"i"=> {
					println!("files in current crate_:-");
                    let files = dc.tycx.sess.codemap.files.borrow();
                    let files = files.get();
					for x in files.iter() {
						println!("\t{}", x.name);
					}
				}
				"j"=> dump_json(dc),
				"q"=> break,
				_ =>{
					// todo - if you just supply line, lookup defs on that line
					// todo - lookup defs from symbol, remembering context of previous lookups?
					let cmd=toks[0];
					let cmd1=match cmd[0] as char { '0'..'9'=>curr_file+":"+cmd,_=>cmd.to_str() };
					let subtoks:~[&str]=cmd1.split(':').collect();
					curr_file=find_file_name_in(dc, subtoks[0].to_str()).unwrap_or(curr_file);
					//dump!(cmd1,subtoks,curr_file);
					let def=lookup_def_at_text_file_pos_str(dc, cmd1,SDM_Source);
					println!("{}", def.unwrap_or(~"no def found"));
					//println(def_of_symbol_to_str(dc,node_spans,node_def_node,toks[0]));
				}
			}
		}
	}
}

