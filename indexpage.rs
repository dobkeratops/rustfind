use rust2html::htmlwriter::HtmlWriter;
use std::str;
use std::io::{File, UserDir};
use std::io::fs::{mkdir_recursive,copy,walk_dir};
use rust2html::RF_Options;
use collections::hashmap::HashMap;
use collections::hashmap::HashSet;
use rust2html;
use std::cmp;


// create index page. Show subdirectories and files matching extentions.
// each subdirectory links to the subdir.html
// each listed file links to its 'file.html'.

pub fn write_index_html(source_dir: &Path,extentions:&[~str], options:&RF_Options) {

	//println!("output dir={}",options.output_dir.as_str().unwrap_or(""));
	println!("Generating index page:-");

	// the index page shows: all source in root dir,
	// all dirs containing source,
	// direct link to any 'mod.rs' or 'lib.rs'

	// hmm, why dont we just make a treeview, ffs.
	let mut source_dirs= HashSet::<(Path,~str)>::new();
	let mut source_files=HashSet::<(Path,~str)>::new();
	match walk_dir(source_dir) {
		Ok(mut dirs)=> {
//			write_index_html_sub(source_dir, dirs, extentions);
//			println!("dir contents {:?} {:?}", source_dir, dirs);
			for path in dirs {
				match path.as_str() {
					Some( ref s)=> {
						let ext=path.extension_str().unwrap_or(&"");;	
						let filename=path.filename_str().unwrap_or(&"");
						let dirname=path.dirname_str().unwrap_or(&"");
						if (ext=="rs" ||
							ext=="cpp"||
							ext=="c"||
							ext=="h") {
							source_dirs.insert((path.clone(), dirname.to_owned()));
							source_files.insert((path.clone(), filename.to_owned()));
						}
					}
					None=>{},
				}
			}
		},
		_=>{}
	};

	let mut doc=HtmlWriter::new();
	let index_path = options.output_dir.join(Path::new("index.html"));
	rust2html::write_head(&mut doc, &index_path, options);

	doc.begin_tag_check("body");
	doc.begin_tag("div");
	doc.begin_tag_check("maintext");

	doc.writeln("Index of " + source_dir.as_str().unwrap_or(""));
	doc.writeln(rust2html::get_git_branch_info());
	doc.writeln("");

	// write as grid, todo abstract it ffs.
	// TODO we're writing HTML, ffs, its got tables and all sorts..
	// TODO: Grid Layout - seperate path/filename to make squarer items.

	let mut max_line_len:uint=0;
	for &(ref sp,ref sf) in source_files.iter() {
		max_line_len =cmp::max(max_line_len, sf.len())
	}
	let desired_width=100;
	let num_cols = desired_width / max_line_len;

	for &(ref sp,ref sf) in source_files.iter() {
		doc.begin_tag_link( sp.as_str().unwrap_or("") + ".html");
		doc.writeln(*sf);
		doc.end_tag();
	}

    doc.end_tag();
    doc.end_tag();
    doc.end_tag();

	rust2html::file_write_bytes_as(&index_path, doc.as_bytes());
}


/*
pub fn write_index_html_sub(source_dir:&Path, contents:[&Path]{ 
	doc = htmlwrite::HtmlWriter::new();
	::rust2html::write_head(doc, source_dir+"/index.html");
		
}*/
