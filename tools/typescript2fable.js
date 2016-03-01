/* global process */
var ts = require("typescript");
var fs = require("fs");
var path = require("path");

var templates = {
file:
`namespace Fable.Import
open System
open Fable.Core
open Fable.Import.JS

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
    "measure",
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

var typeCache = {};

function escapeKeyword(x) {
    return !/^'|<.+?>/.test(x) && (keywords.indexOf(x) >= 0 || reserved.indexOf(x) >= 0 || /[^\w.]/.test(x))
        ? "``" + x + "``"
        : x;
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

function printMethod(prefix) {
    return function (x) {
        return prefix +
            (x.emit ? '[<Emit("' + x.emit +'")>] ' : "") +
            templates.method
            .replace("[NAME]", escapeKeyword(x.name))
            .replace("[TYPE]", escapeKeyword(x.type))
            .replace("[PARAMETERS]", printParameters(x.parameters, " * ", "unit"));
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

function printParents(prefix, node) {
    if (node.name.indexOf("Component") == 0) {
        debugger;
    }
    
    if (!node.parents || node.parents.length == 0) {
        return "";
    }
    
    var lines = [];
    var baseClasses = {};
    var interfaces = {};
    
    for (var i = 0; i < node.parents.length; i++) {
        var parentName = node.parents[i];
        var parent = typeCache[parentName.replace(/<.*?>/,"")];
        if (parent == null) {
            // TODO: Consider non cached types interfaces by default
            interfaces[parentName] = null;
        }
        else {
            if (parent.kind == "class") {
                baseClasses[parentName] = parent;
            }
            else if (parent.kind == "interface") {
                interfaces[parentName] = parent;
            }
        }
    }
    
    if (node.kind == "class") {
        Object.keys(baseClasses).forEach((x, i) => {
            if (i == 0) {
                // TODO: Check base class constructor arguments?
                lines.push(prefix + "inherit " + x + "()");
            }
            else {
                lines.push(prefix + "// inherit " + x + " // TODO: Multiple inheritance, unexpected");
            }
        });
        Object.keys(interfaces).forEach(x => {
            lines.push(prefix + "// inherit " + x + " // TODO: Interface implementation");
        });
    }
    else if (node.kind == "interface") {
        Object.keys(baseClasses).forEach(x => {
            lines.push(prefix + "// inherit " + x + " // TODO: Interfaces cannot extend classes in F#");
        });
        Object.keys(interfaces).forEach(x => {
            lines.push(prefix + "inherit " + x);
        });
    }
    
    return lines.join("\n");
}

function printParent(prefix, kind) {
    return function(name) {
        var parent = typeCache[name.replace(/<.*?>/,"")];
        switch (kind) {
            case "class":
                if (parent && parent.kind == "class")
                    return prefix + "inherit " + name + "()";
                break;
            case "interface":
                if (!parent || parent.kind == "interface")
                    return prefix + "inherit " + name;
                break;
        }
        return "";
    }
}

function printArray(arr, mapper) {
    return arr && arr.length > 0
        ? arr.map(mapper).filter(x => x.length > 0).join("\n")
        : "";
}

function printMembers(ent, prefix) {
    return [
        printParents(prefix, ent),
        // printArray(ent.parents, printParent(prefix, ent.kind)),
        printArray(ent.properties, printProperty(prefix)),
        printArray(ent.methods, printMethod(prefix))
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
        printParents(prefix, ent),
        // printArray(ent.parents, printParent(prefix, ent.kind)),
        printArray(ent.properties, printClassProperty(prefix)),
        printArray(ent.methods, printClassMethod(prefix)),
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

        switch (ifc.kind) {
            case "alias":
                return template += prefix + "    " + ifc.parents[0];
            case "enum":
                return template + ifc.properties.map(function(currentValue) {
                    var cv = templates.enumCase
                                .replace("[NAME]", currentValue.name)
                                .replace("[ID]", currentValue.value)
                    return prefix + cv;
                }).join("\n");
            case "class":
                var classMembers = printClassMembers(ifc, prefix + "    ");
                return template += (classMembers.length == 0
                    ? prefix + "    class end"
                    : classMembers);
            // case "interface":
            default:
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

function printModule(prefix) {
    return function(mod) {
        var template = prefix + templates.module
            .replace("[NAME]", escapeKeyword(mod.name));

        template = append(template, mod.interfaces.map(
            printInterface(prefix + "    ", mod.name)).join("\n\n"));

        var members = printMembers(mod, prefix + "        ");
        if (members.length > 0) {
            template +=
                prefix + "    " + templates.moduleProxyType +
                members + "\n\n" +
                prefix + "    " + templates.moduleProxyDeclaration.replace("[NAME]",
                    (mod.parent ? mod.parent + "?get=" : "") + mod.name);
        }

        template += mod.modules.map(printModule(prefix + "    ")).join("\n\n");

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

 function findTypeParameters(node, acc) {
    acc = acc || [];
    if (!node) {
        return acc;
    }
    if (Array.isArray(node.typeParameters)) {
        node.typeParameters.forEach(x => acc.push(x.name.text));
    }
    return findTypeParameters(node.parent, acc);
 }

function getType(type) {
    var typeParameters = findTypeParameters(type);
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
        case ts.SyntaxKind.TupleType:
            return type.elementTypes.map(getType).join(" * ");
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
            return (typeParameters.indexOf(result) > -1 ? "'" : "") + result;
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
        kind: "enum",
        name: getName(node),
        properties: node.members.map(function (n, i) {
            return {
                name : getName(n),
                value : n.initializer ? n.initializer.text : i
            }
        }),
        parents: [],
        methods: []
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
function getMethod(node, name) {
    var meth = {
        name: name || getName(node),
        type: getType(node.type),
        static: node.name ? hasFlag(node.name.parserContextFlags, ts.NodeFlags.Static) : false,
        parameters: node.parameters.map(getParameter)
    };
    var firstParam = node.parameters[0], secondParam = node.parameters[1];
    if (secondParam && secondParam.type.kind == ts.SyntaxKind.StringLiteral) {
        // The only case I've seen following this pattern is
        // createElementNS(namespaceURI: "http://www.w3.org/2000/svg", qualifiedName: "a"): SVGAElement
        meth.parameters = meth.parameters.slice(2);
        meth.emit = `$0.${meth.name}('${firstParam.type.text}', '${secondParam.type.text}'${meth.parameters.length?',$1...':''})`;
        meth.name += '_' + secondParam.type.text;
    }
    else if (firstParam && firstParam.type.kind == ts.SyntaxKind.StringLiteral) {
        meth.parameters = meth.parameters.slice(1);
        meth.emit = `$0.${meth.name}('${firstParam.type.text}'${meth.parameters.length?',$1...':''})`;
        meth.name += '_' + firstParam.type.text;
    }
    return meth;
}

function getInterface(node, kind) {
    function printTypeParameters(typeParams) {
        typeParams = typeParams || [];
        return typeParams.length == 0 ? "" : "<" + typeParams.map(function (x) {
            return "'" + x.name.text
        }).join(", ") + ">";
    }
    var ifc = {
      name: getName(node) + printTypeParameters(node.typeParameters),
      kind: kind || "interface",
      parents: kind == "alias" ? [getType(node.type)] : getParents(node),
      properties: [],
      methods: []
    };
    // TODO: Respect namespace?
    typeCache[ifc.name.replace(/<.*?>/,"")] = ifc;
    return ifc;
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
                var meth = getMethod(node, "createNew");
                meth.emit = "new $0($1...)";
                ifc.methods.push(meth);
                break;
            case ts.SyntaxKind.Constructor:
                ifc.constructorParameters = node.parameters.map(getParameter);
                break;
        }
    });
    return ifc;
}

function visitModule(node, parent) {
    var mod = {
      name: getName(node),
      parent: parent,
      interfaces: [],
      properties: [],
      methods: [],
      modules: []
    };
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
                // TODO: Support modules with depth > 1?
                if (!mod.parent)
                    mod.modules.push(visitModule(node, mod.name));
                break;
            case ts.SyntaxKind.EnumDeclaration:
                mod.interfaces.push(getEnum(node));
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
                var isEmpty = Object.keys(mod).reduce(function(acc, k) {
                    return acc && !(Array.isArray(mod[k]) && mod[k].length > 0); 
                }, true);
                if (!isEmpty)
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

try {
    var fileName = process.argv[2];
    var code = fs.readFileSync(fileName).toString();
    var sourceFile = ts.createSourceFile(fileName, code, ts.ScriptTarget.ES6, /*setParentNodes */ true);
    var fileInfo = visitFile(sourceFile);
    var ffi = printFile(fileInfo, path.basename(fileName).replace(".d.ts",""))
    console.log(ffi);
    process.exit(0);
}
catch (err) {
    console.log(err);
    process.exit(1);
}
