/* global process */
var ts = require("typescript");
var fs = require("fs");
var path = require("path");

var templates = {
file:
`namespace Fable.Import
open System

[MODULES]
`,

interface:
`    and [NAME] =
`,

module:
`module [NAME] =
    type private ImportAttribute(path) =
        inherit System.Attribute()

[INTERFACES]

[TYPEALIAS]
`,

moduleProxy:
`
    type Global =
[MEMBERS]

    [<Import("[NAME]")>]
    let Global: Global = failwith "JS only"`,

property:
`        abstract [NAME]: [TYPE][OPTION] with get, set`,

method:
`        abstract [NAME]: [PARAMETERS] -> [TYPE]`,

constructor:
`        abstract createNew: [PARAMETERS] -> [TYPE]`
}

var reserved = [
    "atomic",
    "break",
    "checked",
    "component",
    "const",
    "constraint",
    "constructor",
    "continue",
    "eager",
    "event",
    "external",
    "fixed",
    "functor",
    "include",
    "method",
    "mixin",
    "object",
    "parallel",
    "process",
    "protected",
    "pure",
    "sealed",
    "tailcall",
    "trait",
    "virtual",
    "volatile",
    "asr",
    "land",
    "lor",
    "lsl",
    "lsr",
    "lxor",
    "mod",
    "sig"
]

var keywords = [
    "abstract",
    "and",
    "as",
    "assert",
    "base",
    "begin",
    "class",
    "default",
    "delegate",
    "do",
    "done",
    "downcast",
    "downto",
    "elif",
    "else",
    "end",
    "exception",
    "extern",
    "false",
    "finally",
    "for",
    "fun",
    "function",
    "global",
    "if",
    "in",
    "inherit",
    "inline",
    "interface",
    "internal",
    "lazy",
    "let",
    "match",
    "member",
    "module",
    "mutable",
    "namespace",
    "new",
    "null",
    "of",
    "open",
    "or",
    "override",
    "private",
    "public",
    "rec",
    "return",
    "sig",
    "static",
    "struct",
    "then",
    "to",
    "true",
    "try",
    "type",
    "upcast",
    "use",
    "val",
    "void",
    "when",
    "while",
    "with",
    "yield"
]

function escapeKeyword(x) {
    return keywords.indexOf(x) == -1 && reserved.indexOf(x) == -1
        ? x
        : "``" + x + "``";
}


function ensureName(x) {
    if (typeof x === "object"){
        return x.text;
    }
    return x;
}

function getTypeName(x, typeParameters){

    if (!typeParameters) {
        throw "ERROR"
    }

	return typeParameters.length === 0
		? escapeKeyword(ensureName(x.name))
		: escapeKeyword(ensureName(x.name)) + "<" + typeParameters.map(function (x) { return "'" + escapeKeyword(ensureName(x)) }).join(", ") + ">";
}

function printParameters(parameters, typeParameters) {
    if (!typeParameters) {
        throw "ERROR"
    }

    function printParameter(x) {
        if (x.rest) {
            var execed = /ResizeArray<(.*?)>/.exec(x.type)[1];
            var type = (execed == null ? "obj" : execed) + "[]";
            return "[<ParamArray>] " + escapeKeyword(x.name) + ": " + (typeParameters.indexOf(type) > -1 ? "'" : "") + type;
        }
        else {
            return (x.optional ? "?" : "") + escapeKeyword(x.name) + ": " + (typeParameters.indexOf(x.type) > -1 ? "'" : "") + x.type;
        }
    }
    return Array.isArray(parameters) && parameters.length > 0
        ? parameters.map(printParameter).join(" * ")
        : "unit";
}

function printConstructor(x, typeParameters) {
    return templates.constructor
        .replace("[TYPE]", escapeKeyword(escapeKeyword(x.type)))
        .replace("[PARAMETERS]", printParameters(x.parameters, typeParameters));
}

function printMethod(x, typeParameters) {
    return templates.method
        .replace("[NAME]", escapeKeyword(x.name))
        .replace("[TYPE]", escapeKeyword((typeParameters.indexOf(x.type) > -1 ? "'" : "") + x.type))
        .replace("[PARAMETERS]", printParameters(x.parameters, typeParameters));
}

function printProperty(x, typeParameters) {
    return templates.property
        .replace("[NAME]", escapeKeyword(x.name))
        .replace("[TYPE]", escapeKeyword((typeParameters.indexOf(x.type) > -1 ? "'" : "") + x.type))
        .replace("[OPTION]", x.optional ? " option" : "");
}

function printParent(name) {
    return "        inherit " + name;
}

function printMembers(ent, typeParameters) {
    return [
        ent.parents && ent.parents.length > 0
            ? ent.parents.map(printParent).join("\n") : "",
        ent.constructors && ent.constructors.length > 0
            ? ent.constructors.map(function (x) { return printConstructor(x, typeParameters); }).join("\n") : "",
        ent.properties && ent.properties.length > 0
            ? ent.properties.map(function (x) { return printProperty(x, typeParameters); } ).join("\n") : "",
        ent.methods && ent.methods.length > 0
            ? ent.methods.map(function (x) { return printMethod(x, typeParameters); }).join("\n") : "",
    ].filter(x => x.length > 0).join("\n");
}

function printInterface(ifc) {
    var template = templates.interface.replace("[NAME]", getTypeName(ifc, ifc.typeParameters));
    var members = printMembers(ifc, ifc.typeParameters);
    return template += (members.length == 0
        ? "        interface end"
        : members);
}

function printTypeAlias(a){
    var template = templates.interface.replace("[NAME]", getTypeName(a, []));

template += a.types.length == 1
? "        " + a.types[0]

: a.types.map(function (x){

        var name = x.indexOf("<") == -1
            ? x
            : x.substring(0, x.indexOf("<"))

        name = (name.charAt(0).toUpperCase() + name.slice(1)).replace("[]", "Array");
if (name.indexOf('.') != -1) {
name = name.slice(name.lastIndexOf('.') + 1)
}

        return "        | " + name + " of " + x }).join("\n");

    return template
}

function printModule(mod) {
    var template = templates.module
        .replace(/\[NAME\]/g, mod.name)
        .replace("[INTERFACES]", mod.interfaces.map(printInterface).join("\n\n"))
        .replace("[TYPEALIAS]", mod.typeAlias.map(printTypeAlias).join("\n\n"));

    var members = printMembers(mod, []);
    if (members.length > 0) {
        template += templates.moduleProxy
            .replace("[NAME]", mod.name)
            .replace("[MEMBERS]", members)
    }
    return template;
}

function printFile(modules) {
    return templates.file.replace("[MODULES]", modules.map(printModule).join("\n\n"));
}

function hasFlag(flags, flag) {
    return flags != null && (flags & flag) == flag;
}

function getName(node) {
    return (node.name == undefined)
            ? "Global"
            : ((keywords.indexOf(node.name.text) > 0 || reserved.indexOf(node.name.text) > 0)
                ? "``" + node.name.text + "``"
                : node.name.text);
}

var domMappings = [
"DataTransfer",
"Event",
"EventTarget",
"Document",
"HTMLElement",
"Element",
"StyleMedia"
];


function getType(type, typeParameters) {
    switch (type.kind) {
        case ts.SyntaxKind.StringKeyword:
            return "string";
        case ts.SyntaxKind.NumberKeyword:
            return "float";
        case ts.SyntaxKind.BooleanKeyword:
            return "bool";
        case ts.SyntaxKind.VoidKeyword:
            return "unit";
        case ts.SyntaxKind.ArrayType:
            return "ResizeArray<" + getType(type.elementType, typeParameters) + ">";
        case ts.SyntaxKind.FunctionType:
            var cbParams = type.parameters.map(function (x) {
                return getType(x.type, typeParameters);
            }).join(", ");
            cbParams = cbParams.length > 0 ? cbParams + ", " : "";
            return "Func<" + cbParams + getType(type.type, typeParameters) + ">";
        case ts.SyntaxKind.FirstTypeNode:
            var name = type.typeName.text;
            if (!name){
                return "obj"
            }
            if (typeParameters.indexOf(name) > -1
                || name == "T"/*BIT OF A HACK*/){
                return "'" + name
            }

            if (domMappings.indexOf(name) > -1){
                return "___Dom." + name;
            }

            if (name == "Date") { return "DateTime"; }
            var arrMatch = /Array<(.*?)>/.exec(name);
            if (arrMatch != null) { return  "ResizeArray<"+arrMatch[1]+">"; }
            var typeArguments = type.typeArguments ? type.typeArguments : []
            var result =  name + (typeArguments.length == 0 ? "" : "<" + typeArguments.map(function (x) {
                    if (!x.typeName)
                    {
                        return "obj"
                    }
                var t = getType(x, typeArguments)

                return t.charAt(0) == "'" ? t : (typeParameters.indexOf(x.typeName.text) > -1 ? "'" : "") + t;
            }).join(", ") + ">");

if (result == "Array<obj>")
{
    return "obj[]"
}
            return result
        // TODO: Functions
        default:
            return "obj";
    }
}

function getParents(node) {
    var parents = [];
    if (Array.isArray(node.heritageClauses)) {
        for (var i = 0; i < node.heritageClauses.length; i++) {
            var types = node.heritageClauses[i].types;
            for (var j = 0; j < types.length; j++) {
                parents.push(types[j].expression.text);
            }
        }
    }
    return parents;
}

// TODO: get comments
function getProperty(node, typeParameters) {
    return {
        name: getName(node),
        type: getType(node.type, typeParameters),
        optional: node.questionToken != null
        // static: hasFlag(node.name.parserContextFlags, ts.NodeFlags.Static)
    };
}

// TODO: Check if it's const
function getVariables(node) {
    var typeParameters = getTypeParameters(node)
    var variables = [];
    var declarationList = Array
    .isArray(node.declarationList)
        ? node.declarationList : [node.declarationList];
    for (var i = 0; i < declarationList.length; i++) {
        var declarations = declarationList[i].declarations;
        for (var j = 0; j < declarations.length; j++) {
            variables.push({
                name: declarations[j].name.text,
                type: getType(declarations[j].type, typeParameters),
                static: true,
                parameters: []
            });
        }
    }
    return variables;
}

function getTypeParameters(x) {
	return (x.typeParameters || []).map(function(x){ return x.name.text})
}

// TODO: get comments
function getMethod(node, a, typeParameters) {
    if (!typeParameters) {
        throw "ERROR"
    }

    return {
        name: node.name ? getName(node) : null,
        type: getType(node.type, typeParameters),
        // static: hasFlag(node.name.parserContextFlags, ts.NodeFlags.Static),
        typeParameters: getTypeParameters(node),
        parameters: node.parameters.map(function (param) {
            return {
                name: param.name.text,
                type: getType(param.type, typeParameters),
                optional: param.questionToken != null,
                rest: param.dotDotDotToken != null,
            };
        })
    };
}

function getInterface(node) {
    return {
      name: getName(node),
      parents: [],
      properties: [],
      methods: [],
      constructors: [],
      typeParameters: getTypeParameters(node)
    };
}

function getModule(node) {
    return {
      name: getName(node),
      interfaces: [],
      properties: [],
      methods: [],
      typeAlias: []
    };
}

function visitInterface(node) {
    var ifc = getInterface(node, false);
    var typeParameters = getTypeParameters(node);
    node.members.forEach(function(node) {
        switch (node.kind) {
            case ts.SyntaxKind.PropertySignature:
                ifc.properties.push(getProperty(node, typeParameters));
                break;
            case ts.SyntaxKind.PropertyDeclaration:
                ifc.properties.push(getProperty(node));
                break;
            case ts.SyntaxKind.MethodSignature:
                ifc.methods.push(getMethod(node, false, typeParameters));
                break;
            case ts.SyntaxKind.MethodDeclaration:
                ifc.methods.push(getMethod(node));
                break;
            case ts.SyntaxKind.ConstructSignature:
                ifc.constructors.push(getMethod(node, true, typeParameters));
                break;
        }
    });
    return ifc;
}

function getConstructor(parent, node, typeParameters){
    if (!typeParameters) {
        throw "ERROR"
    }

    return {
        type: getTypeName(parent, typeParameters),
        parameters: node.parameters.map(function (param) {
            return {
                name: param.name.text,
                type: getType(param.type, typeParameters),
                optional: param.questionToken != null,
                rest: param.dotDotDotToken != null,
            };
        })
    };
}

function visitClass(node) {
    var ifc = getInterface(node, false);
    var typeParameters = getTypeParameters(node);
    var parent = node;
    node.members.forEach(function(node) {
        switch (node.kind) {
            case ts.SyntaxKind.PropertyDeclaration:
                ifc.properties.push(getProperty(node, typeParameters));
                break;
            case ts.SyntaxKind.MethodDeclaration:
                ifc.methods.push(getMethod(node, false, typeParameters));
                break;
            case ts.SyntaxKind.Constructor:
                ifc.constructors.push(getConstructor(parent, node, typeParameters));
                break;
        }
    });
    return ifc;
}

function visitTypeAlias(node) {
    var a = {
        name: node.name.text,
        types: []
    }

    if (node.type && node.type.types){

        a.types = node.type.types.map(function(t) {
           return getType(t, t.typeArguments || [])
        });
    } else if (node.type) {
        a.types = [getType(node.type, node.type.typeArguments || [])];
    }

    return a;
}

function visitModule(node, modules) {
    var mod = getModule(node, true);

    node.body.statements.forEach(function(node) {
        // TODO: Classes
        switch (node.kind) {
            case ts.SyntaxKind.InterfaceDeclaration:
                mod.interfaces.push(visitInterface(node));
                break;
            case ts.SyntaxKind.VariableStatement:
                getVariables(node).forEach(x =>
                    mod.properties.push(x));
                break;
            case ts.SyntaxKind.FunctionDeclaration:
                var typeParameters = getTypeParameters(node);
                mod.methods.push(getMethod(node, true, typeParameters));
                break;
            case ts.SyntaxKind.ClassDeclaration:
                mod.interfaces.push(visitClass(node));
                break;
            case ts.SyntaxKind.TypeAliasDeclaration:
                mod.typeAlias.push(visitTypeAlias(node));
                break;
            case ts.SyntaxKind.ModuleDeclaration:
                modules.push(visitModule(node, modules));
                break;
            case ts.SyntaxKind.ClassDeclaration:
                mod.interfaces.push(visitInterface(node));
                break;
        }
    });
    return mod;
}

function visitFile(node) {
    var modules = [];
	var emptyModule = getModule(node, true);
    ts.forEachChild(node, function(node) {
        switch (node.kind) {
            case ts.SyntaxKind.InterfaceDeclaration:
				if (modules.length == 0 ) { modules.push( emptyModule ); }
                emptyModule.interfaces.push(visitInterface(node));
                break;
            case ts.SyntaxKind.VariableStatement:
				if (modules.length == 0 ) { modules.push( emptyModule ); }
                getVariables(node).forEach(x =>
                    emptyModule.properties.push(x));
                break;
            case ts.SyntaxKind.FunctionDeclaration:
				if (modules.length == 0 ) { modules.push( emptyModule ); }
                emptyModule.methods.push(getMethod(node, true));
                break;
            case ts.SyntaxKind.ModuleDeclaration:
                modules.push(visitModule(node, modules));
                break;
            case ts.SyntaxKind.ClassDeclaration:
				if (modules.length == 0 ) { modules.push( emptyModule ); }
                emptyModule.interfaces.push(visitInterface(node));
                break;
        }
    });
    return modules;
}

var fileNames = process.argv.slice(2);
fileNames.forEach(function(fileName) {
    var code = fs.readFileSync(fileName).toString();
    var sourceFile = ts.createSourceFile(fileName, code, ts.ScriptTarget.ES6, /*setParentNodes */ true);
    var modules = visitFile(sourceFile);
    var ffi = printFile(modules, path.basename(fileName).replace(".d.ts",""))
    console.log(ffi);
});
process.exit(0);
