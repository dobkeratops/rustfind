use rfindctx::{RFindCtx,find_file_name_in,first_file_name};
use crosscratemap::CrossCrateMap;
use find_ast_node::{build_node_info_map,build_node_def_node_table};
use jumptodefmap::dump_json;
use super::lookup_def_at_text_file_pos_str;
use std::io;
use rsfind::{SDM_Source};

pub fn rustfind_interactive(dc:&RFindCtx) {
	// TODO - check if RUSTI can already do this.. it would be better there IMO
	let node_spans=build_node_info_map(dc.crate);

	let node_def_node = build_node_def_node_table(dc);
	let mut curr_file=first_file_name(dc);

	loop {
		print("rustfind "+curr_file+">");
		let input_line=io::stdin().read_line();
		let toks:~[&str]=input_line.split_iter(' ').collect();
		if toks.len()>0 {
			match toks[0] {
				"h"|"help"=> print("interactive mode\n - enter file:line:pos or line:pos for current file\n - show location & def of symbol there\n j-dump json q-quit i-info\n"),
				"i"=> {
					println("files in current crate:-\n");
					for x in dc.tycx.sess.codemap.files.iter() {
						println("\t"+x.name);
					}
				}
				"j"=> dump_json(dc),
				"q"=> break,
				_ =>{
					// todo - if you just supply line, lookup defs on that line
					// todo - lookup defs from symbol, remembering context of previous lookups?
					let cmd=toks[0];
					let cmd1=match cmd[0] as char { '0'..'9'=>curr_file+":"+cmd,_=>cmd.to_str() };
					let subtoks:~[&str]=cmd1.split_iter(':').collect();
					curr_file=find_file_name_in(dc, subtoks[0].to_str()).unwrap_or_default(curr_file);
					//dump!(cmd1,subtoks,curr_file);
					let def=lookup_def_at_text_file_pos_str(dc, cmd1,SDM_Source);
					print(def.unwrap_or_default
					(~"no def found\n"));
					//println(def_of_symbol_to_str(dc,node_spans,node_def_node,toks[0]));
				}
			}
		}
	}
}

