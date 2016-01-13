namespace Fabel

type CompilerOptions = {
    sourceRootPath: string
    targetRootPath: string
    environment: string
    jsLibFolder: string
}

type ICompiler =
    abstract Options: CompilerOptions

module Literals =
    let [<Literal>] coreLibIdent = "$Fabel"

    // See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#Keywords
    let jsKeywords =
        set["abstract"; "await"; "boolean"; "break"; "byte"; "case"; "catch"; "char"; "class"; "const"; "continue"; "debugger"; "default"; "delete"; "do"; "double";
            "else"; "enum"; "export"; "extends"; "false"; "final"; "finally"; "float"; "for"; "function"; "goto"; "if"; "implements"; "import"; "in"; "instanceof"; "int"; "interface";
            "let"; "long"; "native"; "new"; "null"; "package"; "private"; "protected"; "public"; "return"; "self"; "short"; "static"; "super"; "switch"; "synchronized";
            "this"; "throw"; "throws"; "transient"; "true"; "try"; "typeof"; "undefined"; "var"; "void"; "volatile"; "while"; "with"; "yield" ]

    