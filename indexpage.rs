use rust2html::htmlwriter::HtmlWriter;
use std::str;
use std::io::{File, UserDir};
use std::io::fs::{mkdir_recursive,copy,walk_dir};
use collections::hashmap::HashMap;
use collections::hashmap::HashSet;
use rust2html;
use std::cmp;
use std::vec::Vec;


// create index page. Show subdirectories and files matching extentions.
// each subdirectory links to the subdir.html
// each listed file links to its 'file.html'.


pub fn write_index_html(source_dir: &Path,extentions:&[~str], options:&::RF_Options) {

	//println!("output dir={}",options.output_dir.as_str().unwrap_or(""));
	println!("Generating index page:-");

	// Display all files in the directory tree, 
	// under the containing directory paths.
	let mut files_per_dir=HashMap::<~str,Vec<(~str,~str)> >::new();
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
						if ext=="rs" { 
							let link_target= path.as_str().unwrap_or("").to_owned()+".html";
							if Path::new(link_target.as_slice()).exists() {
								let bucket=files_per_dir.find_or_insert(
									dirname.to_owned(),
									Vec::<(~str,~str)>::new()
								);
// does the file we link to actually exist?
								bucket.push( (filename.to_owned(), link_target)  );
							}
			
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


	doc.begin_tag("c40").writeln(rust2html::get_git_branch_info()).end_tag();

	if options.write_callgraph {
		doc.begin_tag_link("callgraph.html");
		doc.write("callgraph");
		doc.end_tag();
	}


	for (dir, files) in files_per_dir.iter() {
		doc.writeln("");
		doc.begin_tag("c30"); doc.writeln(*dir); doc.end_tag();
	
		write_grid_of_text_links(&mut doc,files);	
	}

	// write as grid, todo abstract it ffs.
	// TODO we're writing HTML, ffs, its got tables and all sorts..
	// TODO: Grid Layout - seperate path/filename to make squarer items.
	

//	write_grid_of_text_links(&mut doc,&links);

	doc.end_tag();
    doc.end_tag();
    doc.end_tag();

	rust2html::file_write_bytes_as(&index_path, doc.as_bytes());
}

fn write_grid_of_text_links(doc:&mut HtmlWriter, links:&Vec<(~str,~str)>) {
	let mut max_line_len:uint=1;
	for &(ref name,ref link) in links.iter() {
		max_line_len =cmp::max(max_line_len, name.len()+1)
	}
	let desired_width=120;
	let num_cols = desired_width / max_line_len;

	let mut column=0;
	for &(ref name,ref link) in links.iter() {
		doc.begin_tag_link(*link);
		doc.write(*name);
		doc.end_tag();
		let mut i=name.len();
		while i<max_line_len { doc.write(" "); i+=1;}
		column+=1;
		if column>=num_cols {
			doc.write_tag("br");
			column=0;
		}
	}
	if column!=0 {
		doc.write_tag("br");
	}

}


/*
pub fn write_index_html_sub(source_dir:&Path, contents:[&Path]{ 
	doc = htmlwrite::HtmlWriter::new();
	::rust2html::write_head(doc, source_dir+"/index.html");
		
}*/
