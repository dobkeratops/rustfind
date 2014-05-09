use rustfindctx::{RustFindCtx,find_file_name_in,first_file_name};
use jumptodefmap::{dump_json,lookup_def_at_text_file_pos_str, make_jump_to_def_map};
use find_ast_node::def_of_symbol_to_str;
use std::io;
use std::io::BufferedReader;
use rsfind::{SDM_Source};
use ioutil::ResultUtil;
/*
interactive mode, also server for IDE integration with command interface
*/

pub fn run_server(dc:&RustFindCtx) {
    // TODO - check if RUSTI can already do this.. it would be better there IMO
    // todo _ why is super needed here?!

    // Currently unused
    let (node_spans,node_def_node,_)=make_jump_to_def_map(dc);

    let mut curr_file=first_file_name(dc);

    println!("rustfind interactive server; type h for help");
    loop {
        print!("rustfind {}> ", curr_file);
        ::std::io::stdio::flush();
        let input_line=BufferedReader::new(io::stdin()).read_line().expect("read_line failed on stdin");
        let toks:Vec<&str> =input_line.words().collect();
        if toks.len()>0 {
            match *toks.get(0) {
                "h" | "help" | "?" => 
                    println!("interactive mode\n - enter file:line:pos or line:pos for current file\n - show location & def of symbol there\n j-dump json q-quit i-info"),
                "i"=> {
                    println!("files in current crate:-");
                    let files = dc.codemap().files.borrow();
                    for x in files.iter() {
                        println!("\t{}", x.name);
                    }
                }
                "j"=> dump_json(dc),
                "q"=> break,
                _ =>{
                    // todo - if you just supply line, lookup defs on that line
                    // todo - lookup defs from symbol, remembering context of previous lookups?
                    let cmd=*toks.get(0);
                    let cmd1 = match cmd.chars().nth(0).unwrap_or('\0') { 
                        '0'..'9' => StrBuf::new().append(curr_file.as_slice()).append(":").append(cmd),
                        _ => cmd.to_strbuf() 
                    };
                    let subtoks:Vec<&str> =cmd1.as_slice().split(':').collect();
                    curr_file=find_file_name_in(dc, *subtoks.get(0)).unwrap_or(curr_file);
                    //dump!(cmd1,subtoks,curr_file);
                    let def=lookup_def_at_text_file_pos_str(dc, cmd1.as_slice(), SDM_Source);
                    println!("{}", def.unwrap_or(StrBuf::from_str("no def found")));
                    println!("{}", def_of_symbol_to_str(dc, &node_spans,node_def_node,*toks.get(0)));
                }
            }
        }
    }
}

