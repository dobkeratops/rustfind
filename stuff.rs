extern mod syntax;

use std::hashmap::HashMap;

// Sketching out how this could work
// but this information is within libsyntax, ast
//

//namespace path should include struct,trait,enum
enum IdentifierType {
    dirname,
    filename,
    module_def,
    type_def,
    enum_def,
    struct_def,
    trait_def,
    function_def,
    type_param,
    arg_name,
    var_name
}

type NamespaceName=(StrBuf,IdentifierType);
type NamespacePath=Vec<NamespaceName>;
type Line=int;
type Column=int;
type Filename=StrBuf;
type SourceLocation=(Filename,(Line,Column));
type SourceSpan=(Filename,(Line,Column),(Line,Column));
type Identifier=StrBuf;


type SymbolDef=(IdentifierType,SourceLocation,~NamespacePath);
type SymbolDefs=HashMap<Identifier,SymbolDef>;

fn namespace_path_at(s:&SourceLocation)->~[NamespacePath]{
    ~[]
}

fn find_symbol_at(ds:&SymbolDefs,sl:&SourceLocation)->Option<SymbolDef>
{
    let path=namespace_path_at(sl);
    let identifier=get_identifier_at(sl);
    let syms=
        match identifier{
            Some(id)=>get_identifier_def(ds,&id),
            None=>None
        };
    // find the symbol definition most specific to the namepath here.
    // = the most matches moving back
    None
}

fn get_identifier_def(ds:&SymbolDefs, id:&Identifier)->Option<&SymbolDef> {
    None
}

fn get_identifier_at(sl:&SourceLocation)->Option<Identifier> {
    Some(~"")
}

fn find_symbol(ds:&SymbolDefs, id:&Identifier)->Option<SymbolDef>
{
// look at the current source location, look down the dir hrc
// look at standard paths
// repeat from current source location, back a dir..
    None
}









