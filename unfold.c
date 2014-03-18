#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef DEBUG2
	#define dbprintf printf
#else
	#define dbprintf(...) 
#endif

#define MALLOCS(TYPE,NUM) ((TYPE*)malloc(sizeof(TYPE)*NUM))
#define SAFE_FREE(ptr) { if (ptr) {free((void*)ptr); ptr=NULL;} }
#if defined(DEBUG) || defined(DEBUG2)
	#define ASSERT(COND) if (!(COND)) {printf("%s:%s:%d error:-\n %s",__FILE__,__FUNCTION__,__LINE__,#COND);exit (0);}
#else
	#define ASSERT(C)
#endif
#define REALLOCS(PTR,NEWNUM) \
	/*dbprintf("Realloc %x[%d] %s:%d",(size_t)(PTR),(int)(NEWNUM),__FILE__,__LINE__);*/\
	PTR=(typeof(PTR)) realloc((void*)(PTR),sizeof(*PTR)*(NEWNUM));

#define MAX_DEPTH 256

enum Opts 
{
	OPT_CLICKABLE			=	0x0001,
	OPT_UNFOLD		=	0x0002,
	OPT_UNFOLD_ENCLOSING		=	0x0004,
	OPT_SINGLE_LINE	=	0x0008,
	OPT_SHOW_FILENAME		=	0x0010,
	OPT_UNFOLD_ABOVE		=	0x0020,
	OPT_COLOR_CODING		=	0x0040,
	OPT_SHOW_FILENAME_ALL		=	0x0080
};

int	gOptions=OPT_SHOW_FILENAME|OPT_UNFOLD_ABOVE|OPT_COLOR_CODING|OPT_UNFOLD;


enum Color {
	COLOR_DEFAULT,
	COLOR_BOLD_RED,
	COLOR_PURPLE,
	COLOR_GREY,
};
const char* g_Colors[]={
	"\x1b[0m",
	"\x1b[31;1m",
	"\x1b[35m",
	"\x1b[37m",
};
const char* getColor(int color) { return (gOptions&OPT_COLOR_CODING)?g_Colors[color]:"";}


const char* help=
"unfold\n"
"\n"
"Display enclosing brace context of grep search results for c-like languages\n"
"e.g. to find enclosing classes, traits, etc..\n\n"
"can also display contents of blocks after found items (eg class contents)\n"
"Useage:-\n\n"
"grep -rn something_interesting . --include <desired source> |unfold\n"
"\n"
"requires filenames and line numbers emitted by grep\n"
"\n"
"output of grep\n"
"somefile.cpp:75:             void foo();\n"
"another.rs:130:        fn foo();\n"
"\n"
"output of unfold\n"
"somefile.cpp:60-     namespace Yada\n"
"somefile.cpp:61-     {\n"
"somefile.cpp:68-         class Foo {\n"
"somefile.cpp:75:             void foo();\n"
"another.rs:38-     mod apple {\n"
"another.rs:68-         trait Banana {\n"
"another.rs:130:            fn foo();\n"
"\n"
"Options:-\n"
"-K/-k\tdo/dont emit clickable links for the brace context(default,no)\n"
"-N/-n\tdo/dont emit filenames(default-yes)\n"
"-S/-s\tdo/dont place all context information on the same line (for further grep?)\n"
"-U/-u\tdo/dont unfold below found lines (default,yes), eg display class contents..\n"
"-E/-e\tdo/dont unfold enclosing context of lines (default,no)\n"
"-A/-a\tdo/dont unfold all enclosing blocks above lines (default,yes)\n"
"-C/-c\tdo/dont color code output (default,yes)\n"
"\n"
"Examples:- \n"
"grep -rn \"fn\\s*baz\" . --include *.c | unfold -Su\n"
"./test.rs:4:	mod foo {	trait bar {		fn baz {  ....yada ...}\n"
"./test.rs:16:	mod foo {	impl bar for Yada 	{		fn baz}\n"
"\n"
"grep -rn \"struct\\s*Whatever\" . --include *.c | unfold -Uaf\n"
"./test.rs:2:	struct Whatever {\n"
"./test.rs:3:	   x\n"
"./test.rs:4:	   y\n"
"./test.rs:5:	   z\n"
"./test.rs:6:	}\n"


"\n";



size_t read_file_as_c_str(char** ppOutput,const char* filename) {
	SAFE_FREE(*ppOutput);
	FILE* fp = fopen(filename,"rb");
	if (!fp) return 0;
	size_t sz = (fseek(fp,0,SEEK_END),ftell(fp)); fseek(fp,0,SEEK_SET);
	*ppOutput=(char*) malloc(sz+1);
	fread(*ppOutput, sz,1, fp);
	(*ppOutput)[sz]=0;
	fclose(fp);
	return sz+1;
}
int	count_lines(const char* txt) {
	int	num=0;
	const char* src=txt;
	char c;
	while (c=*src++) {if (c=='\n') num++;}
	return num;
}
int visLineIndex(int index){
	// irritatingly grep and text editos number from '1'
	// the code numbers from zero and translates to emit
	return index+1;
}

void output_lines(FILE* out,char** lines,int num) {
	for (int i=0; i<num; i++){
		fprintf(out,"%d:%s\n",visLineIndex(i),lines[i]);
	}
}

void split_lines(char*** pppLines, int* pNumLines,char* text,size_t textSize) {
	ASSERT(textSize>0)
	if (!text) {
		SAFE_FREE(*pppLines);
		*pNumLines=0;
		return;
	}
	char *src=text;
	int numLines=count_lines(text);
	REALLOCS(*pppLines,numLines);

	int	lineOut=0;
	
	while (*src && (lineOut<numLines))  {
		ASSERT((src-text)<textSize);
		ASSERT(lineOut<numLines);
		//dbprintf("line%d/%d\n", lineOut,numLines);
		char c;
		(*pppLines)[lineOut++]=src;
		while (c=*src++) {
			if (c=='\n') {
				src[-1]=0;
				break;
			}
		}
	}
	ASSERT(numLines==lineOut);
	if (pNumLines) *pNumLines=numLines;
}

int	get_filename_and_line(char* filename, int maxlen,const char* line) {
	const char* s; char *d=filename;
	bool found=false;
	int numColons=0;
	s=line; 
	while (*s && *s!='\n' && (d-filename)<(maxlen-1)) {
		*d++=*s++;
		if (d[-1]==':') {
			numColons++;
			// next is a line location
			d[-1]=0;
			int	index=0;
			do {
				char c=*s++; 
				if (c==':') numColons++;
				if (!c || c=='\n' || c==':') break;
				index*=10 ;index+=c-'0';
			} while (1);
			if (numColons!=2) /* we did't have filename:lineIndex:*/
			{
				filename[0]=0;
				return -1;
			}
			return index;
		}
	}
	*d++=0;
	return -1;
};

void calc_line_brace_depth(int** lineDepth,  char** lines,int numLines, const char* filename) 
{
//	int	lineIndex;
	int	depth=0;
	REALLOCS(*lineDepth,numLines);
	bool string=false;	
	bool inSingleQuotedStr=false;
	bool comment=false;
	for (int lineIndex=0; lineIndex<numLines; lineIndex++) {
		const char* src=lines[lineIndex],*s2=src;
		(*lineDepth)[lineIndex]=depth;
		inSingleQuotedStr=false;	//can't span lines..
		int numSingleQuotesOnLine=0;
		char c=0;
		while (c=*s2++) {if (c=='\'') numSingleQuotesOnLine++; }
//		bool ignoreSingleQuotes=numSingleQuotesOnLine&1; 
		if (src[0]!='#') 
		{	// ignore preprocessor
			
			while (c=*src++) {
				char c1=*src;
				// todo: can't parse singlequote in rust easily
				// due to use as lifetime param
				// todo: nested strings with
				if (c=='\\' && c1 && (string || inSingleQuotedStr)) {src++; continue;}
				if (c=='\"' && !inSingleQuotedStr) string^=true;//ignore string
				if (!string) {
					if (c=='/' && c1=='/') break; //ignore C++ comment
					if (c=='/' && c1=='*') comment=true;
					if (c=='*' && c1=='/') comment=false;
				}
				if (c=='\'') { // single quoted character? 'x' or '\x' but not rust lifetime eg 'self,
					if (c1=='\\') { // skip escaped quote..
						src++;
						c1=*src;
					}
					if (c1){
						if (src[1]=='\'') {
							src+=2;	// skip the char and close single quote.
							continue;
						}
					}
				}

				if (!(string || comment||inSingleQuotedStr)) {
					if (c=='{') depth++;
					if (c=='}') depth--;
					if (depth<0) {
						printf("fatal, unhandled case can't parse something\n%s:%d:\n",filename,visLineIndex(lineIndex));
						exit(0);
						depth=0;
					}
				}
			}
		}
		dbprintf("line %d depth %d\n",lineIndex,depth);
	}
}
void calc_parent_line(int** lineParent, int* lineDepth, int numLines) {
	int	i;
	int	parent[MAX_DEPTH];
	int	stack=0;
	int	 currDepth=0;
	int p=-1;
	REALLOCS(*lineParent,numLines);
	for (i=0; i<numLines; i++) {
		if (lineDepth[i]>currDepth) {
			ASSERT(currDepth-(MAX_DEPTH-1));
			parent[currDepth++] = p;
			p=i-1;
		} else if (lineDepth[i]<currDepth){
			ASSERT(currDepth>0);
			p=parent[--currDepth];
		}
		(*lineParent)[i]=p;
		dbprintf("line %d\tdepth=%d %d\tparent=%d\n",
			i,lineDepth[i],currDepth,p);
	}
}
int emit_depth_change_lines(FILE* dst,int currDepth, char* filename,int currLine,char** lines,int* depthOfLine,int lastEmitedLine) 
{
	int	srcLine=currLine;
	int	newDepth=depthOfLine[currLine];
	int tmpDepth=newDepth;
	if (currDepth!=newDepth) {
		fprintf(dst,"depth change %d->%d\n",currDepth,newDepth);
	}
	while(currDepth<tmpDepth) {
		int output[MAX_DEPTH];
		int	numOutput=0;
		// collect line indiecs of depth increase..
		while (srcLine>lastEmitedLine) {
			// scan backwards, and show all depth increases
			if (depthOfLine[srcLine-1]<tmpDepth) {
				if (numOutput<MAX_DEPTH && (srcLine-1)!=currLine)
					output[numOutput++]=srcLine-1;
				tmpDepth--;
			}
			srcLine--;
		}
		// write results
		for (int i=numOutput-1;i>=0;i--)
			fprintf(dst,"%s-%d-\t%s\n",filename,visLineIndex(output[i]),lines[output[i]]);
	}
	// todo: brace closes too,might not be obvious from indent

	return newDepth;
}

bool str_contains_char(const char* src, const char x){
	const char* s=src; char c;
	while (c=*s++) { if  (c==x) return true; }
	return false;
}
int get_depth_change(const char* src) {
	//TODO: should preprocess file to ignore braces in strings/comments
	char c;
	int depthChange=0;
	while (c=*src++) {
		if (c=='{') depthChange++;
		if (c=='}') depthChange--;
	}
	return depthChange;
}

bool str_contains_char_of(const char* src,const char* xs) {
	const char* s=src; char c;
	while (c=*s++) {
		if (str_contains_char(xs,c))
			return true;
	}
	return false;
}

bool is_whitespace(char c) {
	return str_contains_char(" \t\n",c);
}
bool is_first_char(const char* src,char x) {
	const char* s=src;char c;
	while (c=*s++){
		if (!is_whitespace(c)) {

			break;
		}
	}
	if (c==x) 
		return true;
	return false;
}
bool is_all_whitespace(const char* src){
	const char* s=src; char c;
	while (c=*s++) if (!is_whitespace(c)) return false;
	return true;
}
void emit_stuff_before_brace(FILE* dst, const char* filename, int currLine, char** lines)
{
	// todo - could use brace & semicolon locations to actually get multiline declarations.
	// a rewrite would probably better be brace based rather than line based.
		//int visIndex=visLineIndex(currLine);
		//fprintf(dst,"%s-%d-%s\n",filename,visLineIndex(currLine),lines[currLine]);
//	return;
	int	lastLine=currLine;
	
	if (is_first_char(lines[currLine],'{')) {
		const char* terminators="};";
		while (currLine>0) {
			const char* l=lines[currLine];
			if (is_all_whitespace(l))
				break;
			if (str_contains_char_of(l,terminators))
				break;
			terminators="};{";	// TODO {..} on one line wont work
			currLine--;
		}
	}

	for(; currLine<=lastLine; currLine++) {
		int visIndex=visLineIndex(currLine);
		char sep = (gOptions & OPT_CLICKABLE)?':':'-';
		if ((gOptions & OPT_SHOW_FILENAME_ALL) && !(gOptions&OPT_SINGLE_LINE))
			/*(gOptions & OPT_SHOW_FILENAME_ALL)*/ 
		{
			fprintf(dst,"%s%s%c%d%c%s\t",getColor(COLOR_PURPLE),filename,sep,visLineIndex(currLine),sep,getColor(COLOR_DEFAULT));
		}
		fprintf(dst,"%s%s%s",getColor(COLOR_GREY),lines[currLine],getColor(COLOR_DEFAULT));
		if (!(gOptions & OPT_SINGLE_LINE)) {
			fprintf(dst,"\n",lines[currLine]);
		}
	}
}



void emit_parent_lines(FILE* dst, char**lines, int* lineParents,int numLines, int currLine,int srcLine, const char*filename,int *lastParent) {
	if (!(gOptions & OPT_UNFOLD_ABOVE)) return;
//	dbprintf("%d->%d\n",currLine,lineParents[currLine]);
	ASSERT(currLine<numLines && currLine>=0);
	if (lineParents[currLine]>=0 && lineParents[currLine]!=currLine) {
		emit_parent_lines(dst,lines,lineParents,numLines,lineParents[currLine],srcLine,filename,lastParent);
//	fprintf(dst,"%s-%d-%s\n",filename,visLineIndex(currLine),lines[currLine]);
	}
	if (currLine!=srcLine && currLine>*lastParent) {
		emit_stuff_before_brace(dst,filename,currLine,lines);
		//fprintf(dst,"%s-%d-%s\n",filename,visLineIndex(currLine),lines[currLine]);
		*lastParent=currLine;
	}
}
void use_bool_option(char c, char x, int opt,int alt_opt) {
	if (c==x) {gOptions&=~opt;}
	else if (c==(x+'A'-'a')) {gOptions|=(alt_opt)?alt_opt:opt;}
}
void parse_opts(int argc, const char* argv[]){
	int	i;	
	for (i=1; i<argc; i++) {
		const char* s=argv[i];char c;
		if (*s++=='-') {
			while (c=*s++) {
				use_bool_option(c,'k',OPT_CLICKABLE,0);
				use_bool_option(c,'f',OPT_SHOW_FILENAME,OPT_SHOW_FILENAME_ALL);
				use_bool_option(c,'s',OPT_SINGLE_LINE,0);
				use_bool_option(c,'u',OPT_UNFOLD,0);
				use_bool_option(c,'e',OPT_UNFOLD_ENCLOSING,0);
				use_bool_option(c,'a',OPT_UNFOLD_ABOVE,0);
				use_bool_option(c,'c',OPT_COLOR_CODING,0);
				if (c=='h') {
					{
						printf("%s",help);
						exit(0);
					}
				}
			}
		}
	}
}

void fprint_file_pos(FILE* fdst,const char* filename,int i,int click) {
	char sep = ((gOptions & OPT_CLICKABLE) || click)?':':'-';
	fprintf(fdst,"%s%s%c%d%c%s\t",getColor(COLOR_PURPLE),filename,sep,visLineIndex(i),sep,getColor(COLOR_DEFAULT));
}

void fprint_line(FILE* fdst,const char* filename, char** currFileLines, int i) { 
	if ((gOptions & OPT_SHOW_FILENAME_ALL) && !(gOptions&OPT_SINGLE_LINE))
		fprint_file_pos(fdst,filename,i,0);
	fprintf(fdst,"%s\n",currFileLines[i]);
}

// show unfolded lines of the line of interest, within a range
void emit_unfolding(FILE* fdst, char**currFileLines, int* parentLines,int numLines, int beginIndex,int endIndex, const char* filename) {
	ASSERT(endIndex<=numLines)
	int	i;
	//dbprintf("emit-unfolding %d-%d\n",beginIndex+1,endIndex+1);
	int	 surroundingParent=(beginIndex>=0 && parentLines && beginIndex<numLines)?parentLines[beginIndex]:-1;

	// dont unfold something that is a statement (prototype?, call?) - only something that opens a block
	// or that is a single-line block

	if (beginIndex>=0 && endIndex>beginIndex && beginIndex<numLines)
	{
		const char* s=	currFileLines[beginIndex];
		if (str_contains_char(s,';') ||
			str_contains_char(s,'{') && get_depth_change(s)==0)
			return;
	}
	//if (!(gOptions & OPT_UNFOLD)) return;
	// Needed to handle non k&r brace style
			
	while (beginIndex<(endIndex-1) && beginIndex>=0 && beginIndex<numLines) {
		int dd=get_depth_change(currFileLines[beginIndex]);
		if (dd<0) break;
		if (dd>0) break;
		beginIndex++;
		if (gOptions & OPT_UNFOLD) {
			fprint_line(fdst,filename,currFileLines,beginIndex);
		}
	};
	int begin=(beginIndex>=0)?(beginIndex+1):0;
	for (i=begin; i<endIndex; i++) {		
		
		if (((parentLines[i]==beginIndex &&beginIndex>=0) && (gOptions & OPT_UNFOLD)) ||	// unfold below hit
			(parentLines[i]==surroundingParent && (gOptions & OPT_UNFOLD_ENCLOSING))//unfold all above hit
			) 
		{
			fprint_line(fdst,filename,currFileLines,i);
		}
		//else dbprintf("[%d]parent[%d]\n",i+1,parentLines[i]+1);
	}
}
enum {MAX_LINE_SIZE=4096,MAX_FILENAME=1024};
int main(int argc, const char* argv[]) {

	parse_opts(argc,argv);
	int totalRead=0;
	FILE* fsrc=stdin; FILE* fdst=stdout;
	char *line=(char*)0;
	size_t sz=0,read=0;
	int*	lineParents=0;
	int*	currFileLineDepth=0;
	char currFilename[MAX_FILENAME]="";
	char*	currFile=0;
	char**	currFileLines=0;
	int	numLines=0;
	int	currDepth=0;
	int lastEmittedLine=0;
	int	lastParent=-1;

	while(-1!=(read=getline(&line,&sz,fsrc))) {
		//output_brace_context(fdst,line);
		char refFilename[MAX_FILENAME]="";
		int lineIndex=get_filename_and_line(refFilename,sizeof(refFilename), line)-1;
		// Change current file if we need to
		if (strcmp(refFilename,currFilename)) {
			emit_unfolding(fdst,currFileLines,lineParents,numLines,lastEmittedLine,numLines,currFilename);	// finish up
			lastParent=-1;
			currDepth=0;
			lastEmittedLine=-1;
			strcpy(currFilename,refFilename);
			dbprintf("New File: %s\n",currFilename);
			int sz=read_file_as_c_str(&currFile, currFilename);
			if (sz) {
				split_lines(&currFileLines,&numLines,currFile,sz);
				calc_line_brace_depth	(&currFileLineDepth,currFileLines,numLines,currFilename);
				calc_parent_line(&lineParents,currFileLineDepth,numLines);
			} else {
				printf("could not read file %s\n",currFilename);
				exit(0);
				numLines=0;
			}
//			output_lines(fdst,currFileLines,numLines);
		}
		// Emit unfolding of previous until here
		emit_unfolding(fdst,currFileLines,lineParents,numLines,lastEmittedLine,lineIndex,currFilename);
		// Emit bracedepth, if we have a valid file..
		if (numLines && lineIndex>=0) {
//			currDepth=emit_depth_change_lines2(fdst,currDepth,currFilename,lineIndex,currFileLines,currFileLineDepth,lineParents,lastEmittedLine);
			if (((gOptions & OPT_SHOW_FILENAME_ALL) && (gOptions&OPT_SINGLE_LINE)) ||
				(gOptions & OPT_SHOW_FILENAME)) 
			{
				// show preceeding filename, with extra space if its the only one
				if (gOptions & OPT_SHOW_FILENAME && !(gOptions&OPT_SINGLE_LINE))
					fprintf(fdst,"\n");
				fprint_file_pos(fdst,currFilename,lineIndex,1);
				if (gOptions & OPT_SHOW_FILENAME && !(gOptions&OPT_SINGLE_LINE))
					fprintf(fdst,"\n");
			}
			//on single line, make it display context from root
			if (gOptions & OPT_SINGLE_LINE) lastParent=-1;	
			emit_parent_lines(fdst,currFileLines,lineParents,numLines, lineIndex,lineIndex,currFilename,
				&lastParent);
			if ((gOptions & OPT_SHOW_FILENAME_ALL) && !(gOptions&OPT_SINGLE_LINE))
				fprint_file_pos(fdst,currFilename,lineIndex,1);
//				fprintf(fdst,"%s%s:%d:%s\t",getColor(COLOR_PURPLE),currFilename,visLineIndex(lineIndex),getColor(COLOR_DEFAULT));
			fprintf(fdst,"%s%s%s\n",getColor(COLOR_BOLD_RED),currFileLines[lineIndex],getColor(COLOR_DEFAULT));
		}
		else
			fprintf(fdst,"%s\n",line);
		lastEmittedLine=lineIndex;
		totalRead+=read;
	}
	emit_unfolding(fdst,currFileLines,lineParents,numLines, lastEmittedLine,numLines,currFilename);	// finish up
	SAFE_FREE(currFileLineDepth);
	SAFE_FREE(currFile);
	SAFE_FREE(currFileLines);
	SAFE_FREE(lineParents);
	if (line) free(line);
	return	0;
}


