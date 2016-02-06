/* global process */
var ts = require("typescript");
var fs = require("fs");
var path = require("path");

var templates = {
file:
`namespace Fabel.Import.[FILENAME]

type ImportAttribute(path: string) =
    inherit System.Attribute()
    
[DECLARATIONS]

[<AutoOpen>]
module Extensions =
    let ___<'T> : 'T = failwith "JS only"
    
[EXTENSIONS]
`, 
    
interface:
`type [NAME] =
[PARENTS]`,

module:
`// Use $(lib) macro to reference lib folder, e.g. "$(lib)jquery" 
[<Import("[NAME]")>]
type [NAME] =
[PARENTS]`,
    
extension:
`    type [NAME] with
[MEMBERS]`,

property:
`        [STATIC]member [SELF][NAME] with get(): [TYPE] = ___ and set(v) = ___`,
    
method:
`        [STATIC]member [SELF][NAME]([PARAMETERS]): [TYPE] = ___`,

constructor:
`        static member createNew([PARAMETERS]): [TYPE] = ___`
}

function printExtensions(ast) {
    // TODO: rest params
    function printParameter(x) {
        return (x.optional ? "?" : "") +
            x.name + ": " + x.type;
    }
    function printMember(x, template) {
        return template
            .replace("[STATIC]", x.static ? "static " : "")
            .replace("[SELF]", x.static ? "" : "__.")
            .replace("[NAME]", x.name)
            .replace("[TYPE]", x.type)
            .replace("[PARAMETERS]", x.parameters.map(printParameter).join(", "));
    }
    return ast.entities.map(function(x) {
        var members = x.constructors.map(function(x) {
           return printMember(x, templates.constructor); 
        })
        .concat(x.properties.map(function(x) {
           return printMember(x, templates.property); 
        }))
        .concat(x.methods.map(function(x) {
           return printMember(x, templates.method); 
        }));
        return templates.extension
            .replace("[NAME]", x.name)
            .replace("[MEMBERS]", members.join("\n"));
    }).join("\n\n");
}

function printDeclarations(ast) {
    function printDeclaration(decl) {
        var template = decl.module ? templates.module : templates.interface;
        return template
            .replace(/\[NAME\]/g, decl.name)
            .replace("[PARENTS]", decl.parents.length === 0
                ? "    interface end"
                : decl.parents.map(function(x) {
                    return "    inherit " + x;
                }).join("\n"));
    }
    return ast.entities.map(printDeclaration).join("\n\n");
}

function printFile(ast, filename) {
    return templates.file
        .replace("[FILENAME]", filename)
        .replace("[DECLARATIONS]", printDeclarations(ast))
        .replace("[EXTENSIONS]", printExtensions(ast));
}

function hasFlag(flags, flag) {
    return flags != null && (flags & flag) == flag;
}

function getType(type) {
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
            return "ResizeArray<"+getType(type.elementType)+">";
        case ts.SyntaxKind.FirstTypeNode:
            // TODO: Array<T>
            return type.typeName.text;
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
function getProperty(node, isStatic) {
    return {
        name: node.name.text,
        type: getType(node.type),
        static: isStatic || hasFlag(node.name.parserContextFlags, ts.NodeFlags.Static),
        parameters: []
    };
}

function getVariables(node) {
    var variables = [];
    if (Array.isArray(node.declarationList)) {
        for (var i = 0; i < node.declarationList.length; i++) {
            var declarations = node.declarationList[i].declarations;
            for (var j = 0; j < declarations.length; j++) {
                variables.push({
                    name: declarations[j].name.text,
                    type: getType(declarations[j].type),
                    static: true,
                    parameters: []
                });
            }
        }
    }
    return variables;
}

// TODO: get comments
function getMethod(node, isStatic) {
    return {
        name: node.name.text,
        type: getType(node.type),
        static: isStatic || hasFlag(node.name.parserContextFlags, ts.NodeFlags.Static),
        parameters: node.parameters.map(function (param) {
            return {
                name: param.name.text,
                type: getType(param.type),
                optional: param.questionToken != null,
                rest: param.dotDotDotToken != null
            };
        })
    };
}

function getEntity(node, isModule) {
    return {
      name: node.name.text,
      properties: [],
      methods: [],
      constructors: [],  
      parents: [],
      module: isModule
    };   
}

function visitInterface(node) {
    var ifc = getEntity(node, false);
    node.members.forEach(function(node) {
        switch (node.kind) {
            case ts.SyntaxKind.PropertySignature:
                ifc.properties.push(getProperty(node));
                break;
            case ts.SyntaxKind.MethodSignature:
                ifc.methods.push(getMethod(node));
                break;
            case ts.SyntaxKind.ConstructSignature:
                ifc.constructors.push(getMethod(node, true));
                break;
        }
    });
    return ifc;
}

function visitModule(node, file) {
    var mod = getEntity(node, true);
    node.body.statements.forEach(function(node) {
        switch (node.kind) {
            case ts.SyntaxKind.InterfaceDeclaration:
                file.entities.push(visitInterface(node));
                break;
            case ts.SyntaxKind.VariableStatement:
                mod.properties.push(getVariables(node, true));
                break;
            case ts.SyntaxKind.FunctionDeclaration:
                mod.methods.push(getMethod(node, true));
                break;
        }
    });
    return mod;
}

function visitFile(node) {
    var file = {
      entities: []
    };
    ts.forEachChild(node, function(node) {
        switch (node.kind) {
            case ts.SyntaxKind.InterfaceDeclaration:
                file.entities.push(visitInterface(node));
                break;
            case ts.SyntaxKind.ModuleDeclaration:
                file.entities.push(visitModule(node, file));
                break;
        }
    });
    return file;
}

var fileNames = process.argv.slice(2);
fileNames.forEach(function(fileName) {
    var code = fs.readFileSync(fileName).toString();
    var sourceFile = ts.createSourceFile(fileName, code, ts.ScriptTarget.ES6, /*setParentNodes */ true);
    var file = visitFile(sourceFile);
    var ffi = printFile(file, path.basename(fileName).replace(".d.ts",""))
    console.log(ffi);
});
process.exit(0);
