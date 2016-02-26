/// <reference path="../typings/node.d.ts"/>
/// <reference path="../typings/typescript.d.ts"/>

/* global process */
var ts = require("typescript");
var fs = require("fs");
var path = require("path");

var templates = {
file:
`namespace Fable.Import
open System
open Fabel.Core
open Fabel.Import.JS

`,

interface:
`[TYPE_KEYWORD] [DECORATOR][NAME][CONSTRUCTOR] =
`,

classDecorator:
`[<Import("[MOD_NAME]?get=[CLASS_NAME]")>] `,

classProperty:
`[STATIC]member [INSTANCE][NAME] with get(): [TYPE][OPTION] = failwith "JS only" and set(v: [TYPE][OPTION]): unit = failwith "JS only"`,

classMethod:
`[STATIC]member [INSTANCE][NAME]([PARAMETERS]): [TYPE] = failwith "JS only"`,

module:
`module [NAME] =
`,

moduleProxyType:
`type Globals =
`,

moduleProxyDeclaration:
`let [<Import("[NAME]")>] Globals: Globals = failwith "JS only"
`,

globalModule:
`module Globals =
`,

globalProperty:
`    let [<Global>] [NAME]: [TYPE] = failwith "JS only"`,

property:
`abstract [NAME]: [TYPE][OPTION] with get, set`,

method:
`abstract [NAME]: [PARAMETERS] -> [TYPE]`,

constructor:
`abstract createNew: [PARAMETERS] -> [TYPE]`,

enum:
`type [NAME] = `,

enumCase:
`    | [NAME] = [ID]`
};

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
];

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
];

var mappedTypes = {
  Date: "DateTime",
  Object: "obj",
  Function: "(obj->obj)",
  Array: "ResizeArray"
};

function escapeKeyword(x) {
    return keywords.indexOf(x) == -1 && reserved.indexOf(x) == -1 && x.indexOf('$') == -1
        ? x
        : "``" + x + "``";
}

function printParameters(parameters, sep, def) {
    sep = sep || ", ", def = def || "";
    function printParameter(x) {
        if (x.rest) {
            var execed = /ResizeArray<(.*?)>/.exec(x.type)[1];
            var type = (execed == null ? "obj" : execed) + "[]";
            return "[<ParamArray>] " + escapeKeyword(x.name) + ": " + type;
        }
        else {
            return (x.optional ? "?" : "") + escapeKeyword(x.name) + ": " + x.type;
        }
    }
    return Array.isArray(parameters) && parameters.length > 0
        ? parameters.map(printParameter).join(sep)
        : def;
}

function printConstructor(prefix) {
    return function (x) {
        return prefix + templates.constructor
            .replace("[TYPE]", escapeKeyword(escapeKeyword(x.type)))
            .replace("[PARAMETERS]", printParameters(x.parameters, " * ", "unit"));
    }
}

function printMethod(prefix) {
    return function (x) {
        return prefix + templates.method
            .replace("[NAME]", escapeKeyword(x.name))
            .replace("[TYPE]", escapeKeyword(x.type))
            .replace("[PARAMETERS]", printParameters(x.parameters, " * ", "unit"));
    }
}

function printEnum(prefix) {
    return function (x) {
        var cases = x.cases.reduce(function(previousValue, currentValue, index, array) {
            var cv = templates.enumCase
                        .replace("[NAMME]", currentValue)
                        .replace("[ID]", index)
            return previousValue + "\n" + cv;
        });
        var e = prefix + templates.enum
                    .replace("[NAME]", x.Name)
        return e + "\n" + cases;
    }
}

function printProperty(prefix) {
    return function (x) {
        return prefix + templates.property
            .replace("[NAME]", escapeKeyword(x.name))
            .replace("[TYPE]", escapeKeyword(x.type))
            .replace("[OPTION]", x.optional ? " option" : "");
    }
}

function printParent(prefix, kind) {
    return function(name) {
        return prefix + "inherit " + name + (kind == "class" ? "()" : "");
    }
}

function printMembers(ent, prefix) {
    return [
        ent.parents && ent.parents.length > 0
            ? ent.parents.map(printParent(prefix)).join("\n") : "",
        ent.constructors && ent.constructors.length > 0
            ? ent.constructors.map(printConstructor(prefix)).join("\n") : "",
        ent.properties && ent.properties.length > 0
            ? ent.properties.map(printProperty(prefix)).join("\n") : "",
        ent.methods && ent.methods.length > 0
            ? ent.methods.map(printMethod(prefix)).join("\n") : "",
        ent.enums && ent.enums.length > 0
            ? ent.enums.map(printEnum(prefix)).join("\n") : "",
    ].filter(x => x.length > 0).join("\n");
}

function printClassMethod(prefix) {
    return function (x) {
        return prefix + templates.classMethod
            .replace("[STATIC]", x.static ? "static " : "")
            .replace("[INSTANCE]", x.static ? "" : "__.")
            .replace("[NAME]", escapeKeyword(x.name))
            .replace("[TYPE]", escapeKeyword(x.type))
            .replace("[PARAMETERS]", printParameters(x.parameters));
    }
}

function printClassProperty(prefix) {
    return function (x) {
        return prefix + templates.classProperty
            .replace("[STATIC]", x.static ? "static " : "")
            .replace("[INSTANCE]", x.static ? "" : "__.")
            .replace("[NAME]", escapeKeyword(x.name))
            .replace(/\[TYPE\]/g, escapeKeyword(x.type))
            .replace(/\[OPTION\]/g, x.optional ? " option" : "");
    }
}

function printClassMembers(ent, prefix) {
    return [
        // TODO: class parents and interfaces
        ent.parents && ent.parents.length > 0
            ? ent.parents.map(printParent(prefix, "class")).join("\n") : "",
        ent.properties && ent.properties.length > 0
            ? ent.properties.map(printClassProperty(prefix)).join("\n") : "",
        ent.methods && ent.methods.length > 0
            ? ent.methods.map(printClassMethod(prefix)).join("\n") : "",
    ].filter(x => x.length > 0).join("\n");
}

function printClassDecorator(ifc, modName) {
    return ifc.kind == "class"
        ? templates.classDecorator
            .replace("[MOD_NAME]", modName)
            .replace("[CLASS_NAME]", ifc.name.replace(/<.*>/, ""))
        : "";
}

function printInterface(prefix, modName) {
    return function (ifc, i) {
        var template = prefix + templates.interface
            .replace("[TYPE_KEYWORD]", i === 0 ? "type" : "and")
            .replace("[NAME]", escapeKeyword(ifc.name))
            .replace("[DECORATOR]", printClassDecorator(ifc, modName))
            .replace("[CONSTRUCTOR]", ifc.kind == "class"
                ? "(" + printParameters(ifc.constructorParameters) + ")" : "");

        if (ifc.kind == "alias") {
            return template += prefix + "    " + ifc.alias;
        }
        else if (ifc.kind == "class") {
            var classMembers = printClassMembers(ifc, prefix + "    ");
            return template += (classMembers.length == 0
                ? prefix + "    class end"
                : classMembers);
        }
        else {
            var members = printMembers(ifc, prefix + "    ");
            return template += (members.length == 0
                ? prefix + "    interface end"
                : members);
        }
    }
}

function append(template, txt) {
    return txt.length > 0 ? template + txt + "\n\n" : template;
}

// TODO: Import path for nested modules
function printModule(prefix) {
    return function(mod) {
        var template = prefix + templates.module
            .replace("[NAME]", escapeKeyword(mod.name));

        var interfaces = mod.interfaces && mod.interfaces.length > 0
                            ? mod.interfaces.map(printInterface(prefix + "    ", mod.name)).join("\n\n")
                            : "";

        template = append(template, interfaces);

        var members = printMembers(mod, prefix + "        ");
        if (members.length > 0) {
            template +=
                prefix + "    " + templates.moduleProxyType +
                members + "\n\n" +
                prefix + "    " + templates.moduleProxyDeclaration.replace("[NAME]", mod.name);
        }

        template += mod.modules && mod.modules.length > 0 ? mod.modules.map(printModule(prefix + "    ")).join("\n\n") : "";

        return template;
    }
}

function printGlobalProperty(x) {
    return templates.globalProperty
        .replace("[NAME]", escapeKeyword(x.name))
        .replace("[TYPE]", escapeKeyword(x.type));
}

function printGlobalProperties(properties) {
    if (properties.length == 0) {
        return "";
    }
    else {
        return templates.globalModule +
            properties.map(printGlobalProperty).join("\n");
    }
}

function printFile(file) {
    var template = templates.file;
    template = append(template, file.interfaces.map(printInterface("")).join("\n\n"));
    template = append(template, printGlobalProperties(file.properties));
    return template + file.modules.map(printModule("")).join("\n\n");
}

function hasFlag(flags, flag) {
    return flags != null && (flags & flag) == flag;
}

function getName(node) {
    // TODO: Throw exception if there's no name?
    return node.name ? node.name.text : null;
}

function printTypeArguments(typeArgs) {
    typeArgs = typeArgs || [];
    return typeArgs.length == 0 ? "" : "<" + typeArgs.map(getType).join(", ") + ">";
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
        case ts.SyntaxKind.SymbolKeyword:
            return "Symbol";
        case ts.SyntaxKind.ArrayType:
            return "ResizeArray<" + getType(type.elementType) + ">";
        case ts.SyntaxKind.FunctionType:
            var cbParams = type.parameters.map(function (x) {
                return x.dotDotDotToken ? "obj" : getType(x.type);
            }).join(", ");
            cbParams = cbParams.length > 0 ? cbParams + ", " : "";
            return "Func<" + cbParams + getType(type.type) + ">";
        case ts.SyntaxKind.UnionType:
            return "U" + type.types.length + printTypeArguments(type.types);
        case ts.SyntaxKind.ParenthesizedType:
            return getType(type.type);
        default:
            if (type.expression && type.expression.kind == ts.SyntaxKind.PropertyAccessExpression) {
                return type.expression.expression.text + "." + type.expression.name.text;
            }

            var name = type.typeName ? type.typeName.text : (type.expression ? type.expression.text : null)
            if (!name) {
                if (type.typeName && type.typeName.left && type.typeName.right) {
                    return type.typeName.left.text + "." + type.typeName.right.text;
                }
                return "obj"
            }

            if (name in mappedTypes) {
                name = mappedTypes[name];
            }

            var result = name + printTypeArguments(type.typeArguments);
            // HACK: Consider one-letter identifiers as type arguments
            return result.length > 1 ? result : "'" + result;
    }
}

function getParents(node) {
    var parents = [];
    if (Array.isArray(node.heritageClauses)) {
        for (var i = 0; i < node.heritageClauses.length; i++) {
            var types = node.heritageClauses[i].types;
            for (var j = 0; j < types.length; j++) {
                parents.push(getType(types[j]));
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
        optional: node.questionToken != null,
        static: node.name ? hasFlag(node.name.parserContextFlags, ts.NodeFlags.Static) : false
    };
}

function getEnum(node) {
    return {
        name : getName(node),
        cases : node.members.map(function (n){ getName(n)})
    }
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

function getParameter(param) {
    return {
        name: param.name.text,
        type: getType(param.type),
        optional: param.questionToken != null,
        rest: param.dotDotDotToken != null,
    };
}

// TODO: get comments
function getMethod(node) {
    return {
        name: getName(node),
        type: getType(node.type),
        static: node.name ? hasFlag(node.name.parserContextFlags, ts.NodeFlags.Static) : false,
        parameters: node.parameters.map(getParameter)
    };
}

function getInterface(node, kind) {
    function printTypeParameters(typeParams) {
        typeParams = typeParams || [];
        return typeParams.length == 0 ? "" : "<" + typeParams.map(function (x) {
            return "'" + x.name.text
        }).join(", ") + ">";
    }
    return {
      name: getName(node) + printTypeParameters(node.typeParameters),
      kind: kind || "interface",
      alias: kind == "alias" ? getType(node.type) : null,
      parents: getParents(node),
      properties: [],
      methods: [],
      constructors: []
    };
}

function getModule(name) {
    return {
      name: name,
      interfaces: [],
      properties: [],
      methods: [],
      modules: [],
      enums: []
    };
}

function visitInterface(node, kind) {
    var ifc = getInterface(node, kind);
    (node.members || []).forEach(function(node) {
        switch (node.kind) {
            case ts.SyntaxKind.PropertySignature:
            case ts.SyntaxKind.PropertyDeclaration:
                // TODO: How can we handle computed property names?
                if (node.name.kind != ts.SyntaxKind.ComputedPropertyName) {
                    ifc.properties.push(getProperty(node));
                }
                break;
            case ts.SyntaxKind.CallSignature:
                // TODO
                break;
            case ts.SyntaxKind.MethodSignature:
            case ts.SyntaxKind.MethodDeclaration:
                // TODO: How can we handle computed property names?
                if (node.name.kind != ts.SyntaxKind.ComputedPropertyName) {
                    ifc.methods.push(getMethod(node));
                }
                break;
            case ts.SyntaxKind.ConstructSignature:
                ifc.constructors.push(getMethod(node));
                break;
            case ts.SyntaxKind.Constructor:
                ifc.constructorParameters = node.parameters.map(getParameter);
                break;
        }
    });
    return ifc;
}

function visitModule(node) {
    var mod = getModule(getName(node));
    node.body.statements.forEach(function(node) {
        switch (node.kind) {
            case ts.SyntaxKind.InterfaceDeclaration:
                mod.interfaces.push(visitInterface(node));
                break;
            case ts.SyntaxKind.ClassDeclaration:
                mod.interfaces.push(visitInterface(node, "class"));
                break;
            case ts.SyntaxKind.TypeAliasDeclaration:
                mod.interfaces.push(visitInterface(node, "alias"));
                break;
            case ts.SyntaxKind.VariableStatement:
                getVariables(node).forEach(x =>
                    mod.properties.push(x));
                break;
            case ts.SyntaxKind.FunctionDeclaration:
                mod.methods.push(getMethod(node));
                break;
            case ts.SyntaxKind.ModuleDeclaration:
                mod.modules.push(visitModule(node));
                break;
            case ts.SyntaxKind.EnumDeclaration:
                mod.modules.push(getEnum(node));
                break;
        }
    });
    return mod;
}

function visitFile(node) {
    var properties = [], interfaces = [], modules = [];
    ts.forEachChild(node, function(node) {
        switch (node.kind) {
            case ts.SyntaxKind.VariableStatement:
                getVariables(node).forEach(x =>
                    properties.push(x));
                break;
            case ts.SyntaxKind.FunctionDeclaration:
                // TODO: For now, ignore global functions
                break;
            case ts.SyntaxKind.ModuleDeclaration:
                var mod = visitModule(node);
                if (mod.interfaces.length || mod.methods.length || mod.modules.length || mod.properties.length)
                    modules.push(mod);
                break;
            case ts.SyntaxKind.InterfaceDeclaration:
				interfaces.push(visitInterface(node));
                break;
            case ts.SyntaxKind.TypeAliasDeclaration:
                interfaces.push(visitInterface(node, "alias"));
                break;
            // case ts.SyntaxKind.ClassDeclaration:
			// 	interfaces.push(visitInterface(node, "class"));
            //     break;
        }
    });
    return {
        properties: properties,
        interfaces: interfaces,
        modules: modules
    };
}

var fileNames = process.argv.slice(2);
fileNames.forEach(function(fileName) {
    var code = fs.readFileSync(fileName).toString();
    var sourceFile = ts.createSourceFile(fileName, code, ts.ScriptTarget.ES6, /*setParentNodes */ true);
    var fileInfo = visitFile(sourceFile);
    var ffi = printFile(fileInfo, path.basename(fileName).replace(".d.ts",""))
    console.log(ffi);
});
process.exit(0);
