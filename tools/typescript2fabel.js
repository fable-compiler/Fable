/* global process */
var ts = require("typescript");
var fs = require("fs");
var path = require("path");

var templates = {
file:
`namespace Fabel.Import
open System
    
[MODULES]
`, 
    
interface:
`    type [NAME] =
`,

module:
`module [NAME] =
    type private ImportAttribute(path) =
        inherit System.Attribute()

[INTERFACES]
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

function printParameters(parameters) {
    function printParameter(x) {
        if (x.rest) {
			var execed = /ResizeArray<(.*?)>/.exec(x.type);
            var type = (execed == null) ? "unknown" : execed[1] + "[]";
            return "[<ParamArray>] " + x.name + ": " + type;
        }
        else {
            return (x.optional ? "?" : "") + x.name + ": " + x.type;
        }
    }
    return Array.isArray(parameters) && parameters.length > 0
        ? parameters.map(printParameter).join(" * ")
        : "unit";
}

function printConstructor(x) {
    return templates.constructor
        .replace("[TYPE]", x.type)
        .replace("[PARAMETERS]", printParameters(x.parameters));
}

function printMethod(x) {
    return templates.method
        .replace("[NAME]", x.name)
        .replace("[TYPE]", x.type)
        .replace("[PARAMETERS]", printParameters(x.parameters));
}

function printProperty(x) {
    return templates.property
        .replace("[NAME]", x.name)
        .replace("[TYPE]", x.type)
        .replace("[OPTION]", x.optional ? " option" : "");
}

function printParent(name) {
    return "        inherit " + name;
}

function printMembers(ent) {
    return [
        ent.parents && ent.parents.length > 0
            ? ent.parents.map(printParent).join("\n") : "",
        ent.constructors && ent.constructors.length > 0
            ? ent.constructors.map(printConstructor).join("\n") : "",
        ent.properties && ent.properties.length > 0
            ? ent.properties.map(printProperty).join("\n") : "",
        ent.methods && ent.methods.length > 0
            ? ent.methods.map(printMethod).join("\n") : "",
    ].filter(x => x.length > 0).join("\n");
}
function printInterface(ifc) {
    var template = templates.interface.replace("[NAME]", ifc.name);
    var members = printMembers(ifc);
    return template += (members.length == 0
        ? "        interface end"
        : members);
}

function printModule(mod) {
    var template = templates.module
        .replace(/\[NAME\]/g, mod.name)
        .replace("[INTERFACES]", mod.interfaces.map(printInterface).join("\n\n"));
        
    var members = printMembers(mod);
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
    // TODO wrap keywords: ``keyword``
    return (node.name == undefined) ? "Unknown" : node.name.text;
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
            return "ResizeArray<" + getType(type.elementType) + ">";
        case ts.SyntaxKind.FunctionType:
            var cbParams = type.parameters.map(x=>getType(x.type)).join(", ");
            cbParams = cbParams.length > 0 ? cbParams + ", " : "";
            return "Func<" + cbParams + getType(type.type) + ">";
        case ts.SyntaxKind.FirstTypeNode:
            var name = type.typeName.text;
            if (name == "Date") { return "DateTime"; }            
            var arrMatch = /Array<(.*?)>/.exec(name);
            if (arrMatch != null) { return  "ResizeArray<"+arrMatch[1]+">"; }
            return name;
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
function getProperty(node) {
    return {
        name: getName(node),
        type: getType(node.type),
        optional: node.questionToken != null
        // static: hasFlag(node.name.parserContextFlags, ts.NodeFlags.Static)
    };
}

// TODO: Check if it's const
function getVariables(node) {
    var variables = [];
    var declarationList = Array.isArray(node.declarationList)
        ? node.declarationList : [node.declarationList];
    for (var i = 0; i < declarationList.length; i++) {
        var declarations = declarationList[i].declarations;
        for (var j = 0; j < declarations.length; j++) {
            variables.push({
                name: declarations[j].name.text,
                type: getType(declarations[j].type),
                static: true,
                parameters: []
            });
        }
    }
    return variables;
}

// TODO: get comments
function getMethod(node) {
    return {
        name: node.name ? getName(node) : null,
        type: getType(node.type),
        // static: hasFlag(node.name.parserContextFlags, ts.NodeFlags.Static),
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

function getInterface(node) {
    return {
      name: getName(node),
      parents: [],
      properties: [],
      methods: [],
      constructors: []
    };
}

function getModule(node) {
    return {
      name: getName(node),
      interfaces: [],
      properties: [],
      methods: [],
    };
}

function visitInterface(node) {
    var ifc = getInterface(node, false);
    node.members.forEach(function(node) {
        switch (node.kind) {
            case ts.SyntaxKind.PropertySignature:
                ifc.properties.push(getProperty(node));
                break;
            case ts.SyntaxKind.PropertyDeclaration:
                ifc.properties.push(getProperty(node));
                break;
            case ts.SyntaxKind.MethodSignature:
                ifc.methods.push(getMethod(node));
                break;
            case ts.SyntaxKind.MethodDeclaration:
                ifc.methods.push(getMethod(node));
                break;
            case ts.SyntaxKind.ConstructSignature:
                ifc.constructors.push(getMethod(node, true));
                break;
        }
    });
    return ifc;
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
                mod.methods.push(getMethod(node, true));
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
