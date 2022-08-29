module internal SR.Resources

let resources =
    dict [
      ( "SeeAlso",
        ". See also {0}."
      );
      ( "ConstraintSolverTupleDiffLengths",
        "The tuples have differing lengths of {0} and {1}"
      );
      ( "ConstraintSolverInfiniteTypes",
        "The types '{0}' and '{1}' cannot be unified."
      );
      ( "ConstraintSolverMissingConstraint",
        "A type parameter is missing a constraint '{0}'"
      );
      ( "ConstraintSolverTypesNotInEqualityRelation1",
        "The unit of measure '{0}' does not match the unit of measure '{1}'"
      );
      ( "ConstraintSolverTypesNotInEqualityRelation2",
        "The type '{0}' does not match the type '{1}'"
      );
      ( "ConstraintSolverTypesNotInSubsumptionRelation",
        "The type '{0}' is not compatible with the type '{1}'{2}"
      );
      ( "ErrorFromAddingTypeEquation1",
        "This expression was expected to have type\n    '{1}'    \nbut here has type\n    '{0}'    {2}"
      );
      ( "ErrorFromAddingTypeEquation2",
        "Type mismatch. Expecting a\n    '{0}'    \nbut given a\n    '{1}'    {2}\n"
      );
      ( "ErrorFromApplyingDefault1",
        "Type constraint mismatch when applying the default type '{0}' for a type inference variable. "
      );
      ( "ErrorFromApplyingDefault2",
        " Consider adding further type constraints"
      );
      ( "ErrorsFromAddingSubsumptionConstraint",
        "Type constraint mismatch. The type \n    '{0}'    \nis not compatible with type\n    '{1}'    {2}\n"
      );
      ( "UpperCaseIdentifierInPattern",
        "Uppercase variable identifiers should not generally be used in patterns, and may indicate a missing open declaration or a misspelt pattern name."
      );
      ( "NotUpperCaseConstructor",
        "Discriminated union cases and exception labels must be uppercase identifiers"
      );
      ( "FunctionExpected",
        "This function takes too many arguments, or is used in a context where a function is not expected"
      );
      ( "BakedInMemberConstraintName",
        "Member constraints with the name '{0}' are given special status by the F# compiler as certain .NET types are implicitly augmented with this member. This may result in runtime failures if you attempt to invoke the member constraint from your own code."
      );
      ( "BadEventTransformation",
        "A definition to be compiled as a .NET event does not have the expected form. Only property members can be compiled as .NET events."
      );
      ( "ParameterlessStructCtor",
        "Implicit object constructors for structs must take at least one argument"
      );
      ( "InterfaceNotRevealed",
        "The type implements the interface '{0}' but this is not revealed by the signature. You should list the interface in the signature, as the interface will be discoverable via dynamic type casts and/or reflection."
      );
      ( "TyconBadArgs",
        "The type '{0}' expects {1} type argument(s) but is given {2}"
      );
      ( "IndeterminateType",
        "Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved."
      );
      ( "NameClash1",
        "Duplicate definition of {0} '{1}'"
      );
      ( "NameClash2",
        "The {0} '{1}' can not be defined because the name '{2}' clashes with the {3} '{4}' in this type or module"
      );
      ( "Duplicate1",
        "Two members called '{0}' have the same signature"
      );
      ( "Duplicate2",
        "Duplicate definition of {0} '{1}'"
      );
      ( "UndefinedName2",
        " A construct with this name was found in FSharp.PowerPack.dll, which contains some modules and types that were implicitly referenced in some previous versions of F#. You may need to add an explicit reference to this DLL in order to compile this code."
      );
      ( "FieldNotMutable",
        "This field is not mutable"
      );
      ( "FieldsFromDifferentTypes",
        "The fields '{0}' and '{1}' are from different types"
      );
      ( "VarBoundTwice",
        "'{0}' is bound twice in this pattern"
      );
      ( "Recursion",
        "A use of the function '{0}' does not match a type inferred elsewhere. The inferred type of the function is\n    {1}.    \nThe type of the function required at this point of use is\n    {2}    {3}\nThis error may be due to limitations associated with generic recursion within a 'let rec' collection or within a group of classes. Consider giving a full type signature for the targets of recursive calls including type annotations for both argument and return types."
      );
      ( "InvalidRuntimeCoercion",
        "Invalid runtime coercion or type test from type {0} to {1}\n{2}"
      );
      ( "IndeterminateRuntimeCoercion",
        "This runtime coercion or type test from type\n    {0}    \n to \n    {1}    \ninvolves an indeterminate type based on information prior to this program point. Runtime type tests are not allowed on some types. Further type annotations are needed."
      );
      ( "IndeterminateStaticCoercion",
        "The static coercion from type\n    {0}    \nto \n    {1}    \n involves an indeterminate type based on information prior to this program point. Static coercions are not allowed on some types. Further type annotations are needed."
      );
      ( "StaticCoercionShouldUseBox",
        "A coercion from the value type \n    {0}    \nto the type \n    {1}    \nwill involve boxing. Consider using 'box' instead"
      );
      ( "TypeIsImplicitlyAbstract",
        "This type is 'abstract' since some abstract members have not been given an implementation. If this is intentional then add the '[<AbstractClass>]' attribute to your type."
      );
      ( "NonRigidTypar1",
        "This construct causes code to be less generic than indicated by its type annotations. The type variable implied by the use of a '#', '_' or other type annotation at or near '{0}' has been constrained to be type '{1}'."
      );
      ( "NonRigidTypar2",
        "This construct causes code to be less generic than indicated by the type annotations. The unit-of-measure variable '{0} has been constrained to be measure '{1}'."
      );
      ( "NonRigidTypar3",
        "This construct causes code to be less generic than indicated by the type annotations. The type variable '{0} has been constrained to be type '{1}'."
      );
      ( "Parser.TOKEN.IDENT",
        "identifier"
      );
      ( "Parser.TOKEN.INT",
        "integer literal"
      );
      ( "Parser.TOKEN.FLOAT",
        "floating point literal"
      );
      ( "Parser.TOKEN.DECIMAL",
        "decimal literal"
      );
      ( "Parser.TOKEN.CHAR",
        "character literal"
      );
      ( "Parser.TOKEN.BASE",
        "keyword 'base'"
      );
      ( "Parser.TOKEN.LPAREN.STAR.RPAREN",
        "symbol '(*)'"
      );
      ( "Parser.TOKEN.DOLLAR",
        "symbol '$'"
      );
      ( "Parser.TOKEN.INFIX.STAR.STAR.OP",
        "infix operator"
      );
      ( "Parser.TOKEN.INFIX.COMPARE.OP",
        "infix operator"
      );
      ( "Parser.TOKEN.COLON.GREATER",
        "symbol ':>'"
      );
      ( "Parser.TOKEN.COLON.COLON",
        "symbol '::'"
      );
      ( "Parser.TOKEN.PERCENT.OP",
        "symbol '{0}"
      );
      ( "Parser.TOKEN.INFIX.AT.HAT.OP",
        "infix operator"
      );
      ( "Parser.TOKEN.INFIX.BAR.OP",
        "infix operator"
      );
      ( "Parser.TOKEN.PLUS.MINUS.OP",
        "infix operator"
      );
      ( "Parser.TOKEN.PREFIX.OP",
        "prefix operator"
      );
      ( "Parser.TOKEN.COLON.QMARK.GREATER",
        "symbol ':?>'"
      );
      ( "Parser.TOKEN.INFIX.STAR.DIV.MOD.OP",
        "infix operator"
      );
      ( "Parser.TOKEN.INFIX.AMP.OP",
        "infix operator"
      );
      ( "Parser.TOKEN.AMP",
        "symbol '&'"
      );
      ( "Parser.TOKEN.AMP.AMP",
        "symbol '&&'"
      );
      ( "Parser.TOKEN.BAR.BAR",
        "symbol '||'"
      );
      ( "Parser.TOKEN.LESS",
        "symbol '<'"
      );
      ( "Parser.TOKEN.GREATER",
        "symbol '>'"
      );
      ( "Parser.TOKEN.QMARK",
        "symbol '?'"
      );
      ( "Parser.TOKEN.QMARK.QMARK",
        "symbol '??'"
      );
      ( "Parser.TOKEN.COLON.QMARK",
        "symbol ':?'"
      );
      ( "Parser.TOKEN.INT32.DOT.DOT",
        "integer.."
      );
      ( "Parser.TOKEN.DOT.DOT",
        "symbol '..'"
      );
      ( "Parser.TOKEN.DOT.DOT.HAT",
        "symbol '..^'"
      );
      ( "Parser.TOKEN.QUOTE",
        "quote symbol"
      );
      ( "Parser.TOKEN.STAR",
        "symbol '*'"
      );
      ( "Parser.TOKEN.HIGH.PRECEDENCE.TYAPP",
        "type application "
      );
      ( "Parser.TOKEN.COLON",
        "symbol ':'"
      );
      ( "Parser.TOKEN.COLON.EQUALS",
        "symbol ':='"
      );
      ( "Parser.TOKEN.LARROW",
        "symbol '<-'"
      );
      ( "Parser.TOKEN.EQUALS",
        "symbol '='"
      );
      ( "Parser.TOKEN.GREATER.BAR.RBRACK",
        "symbol '>|]'"
      );
      ( "Parser.TOKEN.MINUS",
        "symbol '-'"
      );
      ( "Parser.TOKEN.ADJACENT.PREFIX.OP",
        "prefix operator"
      );
      ( "Parser.TOKEN.FUNKY.OPERATOR.NAME",
        "operator name"
      );
      ( "Parser.TOKEN.COMMA",
        "symbol ','"
      );
      ( "Parser.TOKEN.DOT",
        "symbol '.'"
      );
      ( "Parser.TOKEN.BAR",
        "symbol '|'"
      );
      ( "Parser.TOKEN.HASH",
        "symbol #"
      );
      ( "Parser.TOKEN.UNDERSCORE",
        "symbol '_'"
      );
      ( "Parser.TOKEN.SEMICOLON",
        "symbol ';'"
      );
      ( "Parser.TOKEN.SEMICOLON.SEMICOLON",
        "symbol ';;'"
      );
      ( "Parser.TOKEN.LPAREN",
        "symbol '('"
      );
      ( "Parser.TOKEN.RPAREN",
        "symbol ')'"
      );
      ( "Parser.TOKEN.SPLICE.SYMBOL",
        "symbol 'splice'"
      );
      ( "Parser.TOKEN.LQUOTE",
        "start of quotation"
      );
      ( "Parser.TOKEN.LBRACK",
        "symbol '['"
      );
      ( "Parser.TOKEN.LBRACE.BAR",
        "symbol '{|'"
      );
      ( "Parser.TOKEN.LBRACK.BAR",
        "symbol '[|'"
      );
      ( "Parser.TOKEN.LBRACK.LESS",
        "symbol '[<'"
      );
      ( "Parser.TOKEN.LBRACE",
        "symbol '{'"
      );
      ( "Parser.TOKEN.LBRACE.LESS",
        "symbol '{<'"
      );
      ( "Parser.TOKEN.BAR.RBRACK",
        "symbol '|]'"
      );
      ( "Parser.TOKEN.BAR.RBRACE",
        "symbol '|}'"
      );
      ( "Parser.TOKEN.GREATER.RBRACE",
        "symbol '>}'"
      );
      ( "Parser.TOKEN.GREATER.RBRACK",
        "symbol '>]'"
      );
      ( "Parser.TOKEN.RQUOTE",
        "end of quotation"
      );
      ( "Parser.TOKEN.RBRACK",
        "symbol ']'"
      );
      ( "Parser.TOKEN.RBRACE",
        "symbol '}'"
      );
      ( "Parser.TOKEN.PUBLIC",
        "keyword 'public'"
      );
      ( "Parser.TOKEN.PRIVATE",
        "keyword 'private'"
      );
      ( "Parser.TOKEN.INTERNAL",
        "keyword 'internal'"
      );
      ( "Parser.TOKEN.FIXED",
        "keyword 'fixed'"
      );
      ( "Parser.TOKEN.INTERP.STRING.BEGIN.END",
        "interpolated string"
      );
      ( "Parser.TOKEN.INTERP.STRING.BEGIN.PART",
        "interpolated string (first part)"
      );
      ( "Parser.TOKEN.INTERP.STRING.PART",
        "interpolated string (part)"
      );
      ( "Parser.TOKEN.INTERP.STRING.END",
        "interpolated string (final part)"
      );
      ( "Parser.TOKEN.CONSTRAINT",
        "keyword 'constraint'"
      );
      ( "Parser.TOKEN.INSTANCE",
        "keyword 'instance'"
      );
      ( "Parser.TOKEN.DELEGATE",
        "keyword 'delegate'"
      );
      ( "Parser.TOKEN.INHERIT",
        "keyword 'inherit'"
      );
      ( "Parser.TOKEN.CONSTRUCTOR",
        "keyword 'constructor'"
      );
      ( "Parser.TOKEN.DEFAULT",
        "keyword 'default'"
      );
      ( "Parser.TOKEN.OVERRIDE",
        "keyword 'override'"
      );
      ( "Parser.TOKEN.ABSTRACT",
        "keyword 'abstract'"
      );
      ( "Parser.TOKEN.CLASS",
        "keyword 'class'"
      );
      ( "Parser.TOKEN.MEMBER",
        "keyword 'member'"
      );
      ( "Parser.TOKEN.STATIC",
        "keyword 'static'"
      );
      ( "Parser.TOKEN.NAMESPACE",
        "keyword 'namespace'"
      );
      ( "Parser.TOKEN.OBLOCKBEGIN",
        "start of structured construct"
      );
      ( "Parser.TOKEN.OBLOCKEND",
        "incomplete structured construct at or before this point"
      );
      ( "BlockEndSentence",
        "Incomplete structured construct at or before this point"
      );
      ( "Parser.TOKEN.OTHEN",
        "keyword 'then'"
      );
      ( "Parser.TOKEN.OELSE",
        "keyword 'else'"
      );
      ( "Parser.TOKEN.OLET",
        "keyword 'let' or 'use'"
      );
      ( "Parser.TOKEN.BINDER",
        "binder keyword"
      );
      ( "Parser.TOKEN.ODO",
        "keyword 'do'"
      );
      ( "Parser.TOKEN.CONST",
        "keyword 'const'"
      );
      ( "Parser.TOKEN.OWITH",
        "keyword 'with'"
      );
      ( "Parser.TOKEN.OFUNCTION",
        "keyword 'function'"
      );
      ( "Parser.TOKEN.OFUN",
        "keyword 'fun'"
      );
      ( "Parser.TOKEN.ORESET",
        "end of input"
      );
      ( "Parser.TOKEN.ODUMMY",
        "internal dummy token"
      );
      ( "Parser.TOKEN.ODO.BANG",
        "keyword 'do!'"
      );
      ( "Parser.TOKEN.YIELD",
        "yield"
      );
      ( "Parser.TOKEN.YIELD.BANG",
        "yield!"
      );
      ( "Parser.TOKEN.OINTERFACE.MEMBER",
        "keyword 'interface'"
      );
      ( "Parser.TOKEN.ELIF",
        "keyword 'elif'"
      );
      ( "Parser.TOKEN.RARROW",
        "symbol '->'"
      );
      ( "Parser.TOKEN.SIG",
        "keyword 'sig'"
      );
      ( "Parser.TOKEN.STRUCT",
        "keyword 'struct'"
      );
      ( "Parser.TOKEN.UPCAST",
        "keyword 'upcast'"
      );
      ( "Parser.TOKEN.DOWNCAST",
        "keyword 'downcast'"
      );
      ( "Parser.TOKEN.NULL",
        "keyword 'null'"
      );
      ( "Parser.TOKEN.RESERVED",
        "reserved keyword"
      );
      ( "Parser.TOKEN.MODULE",
        "keyword 'module'"
      );
      ( "Parser.TOKEN.AND",
        "keyword 'and'"
      );
      ( "Parser.TOKEN.AND.BANG",
        "keyword 'and!'"
      );
      ( "Parser.TOKEN.AS",
        "keyword 'as'"
      );
      ( "Parser.TOKEN.ASSERT",
        "keyword 'assert'"
      );
      ( "Parser.TOKEN.ASR",
        "keyword 'asr'"
      );
      ( "Parser.TOKEN.DOWNTO",
        "keyword 'downto'"
      );
      ( "Parser.TOKEN.EXCEPTION",
        "keyword 'exception'"
      );
      ( "Parser.TOKEN.FALSE",
        "keyword 'false'"
      );
      ( "Parser.TOKEN.FOR",
        "keyword 'for'"
      );
      ( "Parser.TOKEN.FUN",
        "keyword 'fun'"
      );
      ( "Parser.TOKEN.FUNCTION",
        "keyword 'function'"
      );
      ( "Parser.TOKEN.FINALLY",
        "keyword 'finally'"
      );
      ( "Parser.TOKEN.LAZY",
        "keyword 'lazy'"
      );
      ( "Parser.TOKEN.MATCH",
        "keyword 'match'"
      );
      ( "Parser.TOKEN.MATCH.BANG",
        "keyword 'match!'"
      );
      ( "Parser.TOKEN.MUTABLE",
        "keyword 'mutable'"
      );
      ( "Parser.TOKEN.NEW",
        "keyword 'new'"
      );
      ( "Parser.TOKEN.OF",
        "keyword 'of'"
      );
      ( "Parser.TOKEN.OPEN",
        "keyword 'open'"
      );
      ( "Parser.TOKEN.OR",
        "keyword 'or'"
      );
      ( "Parser.TOKEN.VOID",
        "keyword 'void'"
      );
      ( "Parser.TOKEN.EXTERN",
        "keyword 'extern'"
      );
      ( "Parser.TOKEN.INTERFACE",
        "keyword 'interface'"
      );
      ( "Parser.TOKEN.REC",
        "keyword 'rec'"
      );
      ( "Parser.TOKEN.TO",
        "keyword 'to'"
      );
      ( "Parser.TOKEN.TRUE",
        "keyword 'true'"
      );
      ( "Parser.TOKEN.TRY",
        "keyword 'try'"
      );
      ( "Parser.TOKEN.TYPE",
        "keyword 'type'"
      );
      ( "Parser.TOKEN.VAL",
        "keyword 'val'"
      );
      ( "Parser.TOKEN.INLINE",
        "keyword 'inline'"
      );
      ( "Parser.TOKEN.WHEN",
        "keyword 'when'"
      );
      ( "Parser.TOKEN.WHILE",
        "keyword 'while'"
      );
      ( "Parser.TOKEN.WITH",
        "keyword 'with'"
      );
      ( "Parser.TOKEN.IF",
        "keyword 'if'"
      );
      ( "Parser.TOKEN.DO",
        "keyword 'do'"
      );
      ( "Parser.TOKEN.GLOBAL",
        "keyword 'global'"
      );
      ( "Parser.TOKEN.DONE",
        "keyword 'done'"
      );
      ( "Parser.TOKEN.IN",
        "keyword 'in'"
      );
      ( "Parser.TOKEN.HIGH.PRECEDENCE.PAREN.APP",
        "symbol '('"
      );
      ( "Parser.TOKEN.HIGH.PRECEDENCE.BRACK.APP",
        "symbol'['"
      );
      ( "Parser.TOKEN.BEGIN",
        "keyword 'begin'"
      );
      ( "Parser.TOKEN.END",
        "keyword 'end'"
      );
      ( "Parser.TOKEN.HASH.ENDIF",
        "directive"
      );
      ( "Parser.TOKEN.INACTIVECODE",
        "inactive code"
      );
      ( "Parser.TOKEN.LEX.FAILURE",
        "lex failure"
      );
      ( "Parser.TOKEN.WHITESPACE",
        "whitespace"
      );
      ( "Parser.TOKEN.COMMENT",
        "comment"
      );
      ( "Parser.TOKEN.LINE.COMMENT",
        "line comment"
      );
      ( "Parser.TOKEN.STRING.TEXT",
        "string text"
      );
      ( "Parser.TOKEN.KEYWORD_STRING",
        "compiler generated literal"
      );
      ( "Parser.TOKEN.BYTEARRAY",
        "byte array literal"
      );
      ( "Parser.TOKEN.STRING",
        "string literal"
      );
      ( "Parser.TOKEN.EOF",
        "end of input"
      );
      ( "UnexpectedEndOfInput",
        "Unexpected end of input"
      );
      ( "Unexpected",
        "Unexpected {0}"
      );
      ( "NONTERM.interaction",
        " in interaction"
      );
      ( "NONTERM.hashDirective",
        " in directive"
      );
      ( "NONTERM.fieldDecl",
        " in field declaration"
      );
      ( "NONTERM.unionCaseRepr",
        " in discriminated union case declaration"
      );
      ( "NONTERM.localBinding",
        " in binding"
      );
      ( "NONTERM.hardwhiteLetBindings",
        " in binding"
      );
      ( "NONTERM.classDefnMember",
        " in member definition"
      );
      ( "NONTERM.defnBindings",
        " in definitions"
      );
      ( "NONTERM.classMemberSpfn",
        " in member signature"
      );
      ( "NONTERM.valSpfn",
        " in value signature"
      );
      ( "NONTERM.tyconSpfn",
        " in type signature"
      );
      ( "NONTERM.anonLambdaExpr",
        " in lambda expression"
      );
      ( "NONTERM.attrUnionCaseDecl",
        " in union case"
      );
      ( "NONTERM.cPrototype",
        " in extern declaration"
      );
      ( "NONTERM.objectImplementationMembers",
        " in object expression"
      );
      ( "NONTERM.ifExprCases",
        " in if/then/else expression"
      );
      ( "NONTERM.openDecl",
        " in open declaration"
      );
      ( "NONTERM.fileModuleSpec",
        " in module or namespace signature"
      );
      ( "NONTERM.patternClauses",
        " in pattern matching"
      );
      ( "NONTERM.beginEndExpr",
        " in begin/end expression"
      );
      ( "NONTERM.recdExpr",
        " in record expression"
      );
      ( "NONTERM.tyconDefn",
        " in type definition"
      );
      ( "NONTERM.exconCore",
        " in exception definition"
      );
      ( "NONTERM.typeNameInfo",
        " in type name"
      );
      ( "NONTERM.attributeList",
        " in attribute list"
      );
      ( "NONTERM.quoteExpr",
        " in quotation literal"
      );
      ( "NONTERM.typeConstraint",
        " in type constraint"
      );
      ( "NONTERM.Category.ImplementationFile",
        " in implementation file"
      );
      ( "NONTERM.Category.Definition",
        " in definition"
      );
      ( "NONTERM.Category.SignatureFile",
        " in signature file"
      );
      ( "NONTERM.Category.Pattern",
        " in pattern"
      );
      ( "NONTERM.Category.Expr",
        " in expression"
      );
      ( "NONTERM.Category.Type",
        " in type"
      );
      ( "NONTERM.typeArgsActual",
        " in type arguments"
      );
      ( "FixKeyword",
        "keyword "
      );
      ( "FixSymbol",
        "symbol "
      );
      ( "FixReplace",
        " (due to indentation-aware syntax)"
      );
      ( "TokenName1",
        ". Expected {0} or other token."
      );
      ( "TokenName1TokenName2",
        ". Expected {0}, {1} or other token."
      );
      ( "TokenName1TokenName2TokenName3",
        ". Expected {0}, {1}, {2} or other token."
      );
      ( "RuntimeCoercionSourceSealed1",
        "The type '{0}' cannot be used as the source of a type test or runtime coercion"
      );
      ( "RuntimeCoercionSourceSealed2",
        "The type '{0}' does not have any proper subtypes and cannot be used as the source of a type test or runtime coercion."
      );
      ( "CoercionTargetSealed",
        "The type '{0}' does not have any proper subtypes and need not be used as the target of a static coercion"
      );
      ( "UpcastUnnecessary",
        "This upcast is unnecessary - the types are identical"
      );
      ( "TypeTestUnnecessary",
        "This type test or downcast will always hold"
      );
      ( "OverrideDoesntOverride1",
        "The member '{0}' does not have the correct type to override any given virtual method"
      );
      ( "OverrideDoesntOverride2",
        "The member '{0}' does not have the correct type to override the corresponding abstract method."
      );
      ( "OverrideDoesntOverride3",
        " The required signature is '{0}'."
      );
      ( "OverrideDoesntOverride4",
        "The member '{0}' is specialized with 'unit' but 'unit' can't be used as return type of an abstract method parameterized on return type."
      );
      ( "UnionCaseWrongArguments",
        "This constructor is applied to {0} argument(s) but expects {1}"
      );
      ( "UnionPatternsBindDifferentNames",
        "The two sides of this 'or' pattern bind different sets of variables"
      );
      ( "ValueNotContained",
        "Module '{0}' contains\n    {1}    \nbut its signature specifies\n    {2}    \n{3}."
      );
      ( "RequiredButNotSpecified",
        "Module '{0}' requires a {1} '{2}'"
      );
      ( "UseOfAddressOfOperator",
        "The use of native pointers may result in unverifiable .NET IL code"
      );
      ( "DefensiveCopyWarning",
        "{0}"
      );
      ( "DeprecatedThreadStaticBindingWarning",
        "Thread static and context static 'let' bindings are deprecated. Instead use a declaration of the form 'static val mutable <ident> : <type>' in a class. Add the 'DefaultValue' attribute to this declaration to indicate that the value is initialized to the default value on each new thread."
      );
      ( "FunctionValueUnexpected",
        "This expression is a function value, i.e. is missing arguments. Its type is {0}."
      );
      ( "UnitTypeExpected",
        "The result of this expression has type '{0}' and is implicitly ignored. Consider using 'ignore' to discard this value explicitly, e.g. 'expr |> ignore', or 'let' to bind the result to a name, e.g. 'let result = expr'."
      );
      ( "UnitTypeExpectedWithEquality",
        "The result of this equality expression has type '{0}' and is implicitly discarded. Consider using 'let' to bind the result to a name, e.g. 'let result = expression'."
      );
      ( "UnitTypeExpectedWithPossiblePropertySetter",
        "The result of this equality expression has type '{0}' and is implicitly discarded. Consider using 'let' to bind the result to a name, e.g. 'let result = expression'. If you intended to set a value to a property, then use the '<-' operator e.g. '{1}.{2} <- expression'."
      );
      ( "UnitTypeExpectedWithPossibleAssignment",
        "The result of this equality expression has type '{0}' and is implicitly discarded. Consider using 'let' to bind the result to a name, e.g. 'let result = expression'. If you intended to mutate a value, then mark the value 'mutable' and use the '<-' operator e.g. '{1} <- expression'."
      );
      ( "UnitTypeExpectedWithPossibleAssignmentToMutable",
        "The result of this equality expression has type '{0}' and is implicitly discarded. Consider using 'let' to bind the result to a name, e.g. 'let result = expression'. If you intended to mutate a value, then use the '<-' operator e.g. '{1} <- expression'."
      );
      ( "RecursiveUseCheckedAtRuntime",
        "This recursive use will be checked for initialization-soundness at runtime. This warning is usually harmless, and may be suppressed by using '#nowarn \"21\"' or '--nowarn:21'."
      );
      ( "LetRecUnsound1",
        "The value '{0}' will be evaluated as part of its own definition"
      );
      ( "LetRecUnsound2",
        "This value will be eventually evaluated as part of its own definition. You may need to make the value lazy or a function. Value '{0}'{1}."
      );
      ( "LetRecUnsoundInner",
        " will evaluate '{0}'"
      );
      ( "LetRecEvaluatedOutOfOrder",
        "Bindings may be executed out-of-order because of this forward reference."
      );
      ( "LetRecCheckedAtRuntime",
        "This and other recursive references to the object(s) being defined will be checked for initialization-soundness at runtime through the use of a delayed reference. This is because you are defining one or more recursive objects, rather than recursive functions. This warning may be suppressed by using '#nowarn \"40\"' or '--nowarn:40'."
      );
      ( "SelfRefObjCtor1",
        "Recursive references to the object being defined will be checked for initialization soundness at runtime through the use of a delayed reference. Consider placing self-references in members or within a trailing expression of the form '<ctor-expr> then <expr>'."
      );
      ( "SelfRefObjCtor2",
        "Recursive references to the object being defined will be checked for initialization soundness at runtime through the use of a delayed reference. Consider placing self-references within 'do' statements after the last 'let' binding in the construction sequence."
      );
      ( "VirtualAugmentationOnNullValuedType",
        "The containing type can use 'null' as a representation value for its nullary union case. Invoking an abstract or virtual member or an interface implementation on a null value will lead to an exception. If necessary add a dummy data value to the nullary constructor to avoid 'null' being used as a representation for this type."
      );
      ( "NonVirtualAugmentationOnNullValuedType",
        "The containing type can use 'null' as a representation value for its nullary union case. This member will be compiled as a static member."
      );
      ( "NonUniqueInferredAbstractSlot1",
        "The member '{0}' doesn't correspond to a unique abstract slot based on name and argument count alone"
      );
      ( "NonUniqueInferredAbstractSlot2",
        ". Multiple implemented interfaces have a member with this name and argument count"
      );
      ( "NonUniqueInferredAbstractSlot3",
        ". Consider implementing interfaces '{0}' and '{1}' explicitly."
      );
      ( "NonUniqueInferredAbstractSlot4",
        ". Additional type annotations may be required to indicate the relevant override. This warning can be disabled using '#nowarn \"70\"' or '--nowarn:70'."
      );
      ( "Failure1",
        "parse error"
      );
      ( "Failure2",
        "parse error: unexpected end of file"
      );
      ( "Failure3",
        "{0}"
      );
      ( "Failure4",
        "internal error: {0}"
      );
      ( "FullAbstraction",
        "{0}"
      );
      ( "MatchIncomplete1",
        "Incomplete pattern matches on this expression."
      );
      ( "MatchIncomplete2",
        " For example, the value '{0}' may indicate a case not covered by the pattern(s)."
      );
      ( "MatchIncomplete3",
        " For example, the value '{0}' may indicate a case not covered by the pattern(s). However, a pattern rule with a 'when' clause might successfully match this value."
      );
      ( "MatchIncomplete4",
        " Unmatched elements will be ignored."
      );
      ( "EnumMatchIncomplete1",
        "Enums may take values outside known cases."
      );
      ( "RuleNeverMatched",
        "This rule will never be matched"
      );
      ( "ValNotMutable",
        "This value is not mutable. Consider using the mutable keyword, e.g. 'let mutable {0} = expression'."
      );
      ( "ValNotLocal",
        "This value is not local"
      );
      ( "Obsolete1",
        "This construct is deprecated"
      );
      ( "Obsolete2",
        ". {0}"
      );
      ( "Experimental",
        "{0}. This warning can be disabled using '--nowarn:57' or '#nowarn \"57\"'."
      );
      ( "PossibleUnverifiableCode",
        "Uses of this construct may result in the generation of unverifiable .NET IL code. This warning can be disabled using '--nowarn:9' or '#nowarn \"9\"'."
      );
      ( "Deprecated",
        "This construct is deprecated: {0}"
      );
      ( "LibraryUseOnly",
        "This construct is deprecated: it is only for use in the F# library"
      );
      ( "MissingFields",
        "The following fields require values: {0}"
      );
      ( "ValueRestriction1",
        "Value restriction. The value '{0}' has generic type\n    {1}    \nEither make the arguments to '{2}' explicit or, if you do not intend for it to be generic, add a type annotation."
      );
      ( "ValueRestriction2",
        "Value restriction. The value '{0}' has generic type\n    {1}    \nEither make '{2}' into a function with explicit arguments or, if you do not intend for it to be generic, add a type annotation."
      );
      ( "ValueRestriction3",
        "Value restriction. This member has been inferred to have generic type\n    {0}    \nConstructors and property getters/setters cannot be more generic than the enclosing type.  Add a type annotation to indicate the exact types involved."
      );
      ( "ValueRestriction4",
        "Value restriction. The value '{0}' has been inferred to have generic type\n    {1}    \nEither make the arguments to '{2}' explicit or, if you do not intend for it to be generic, add a type annotation."
      );
      ( "ValueRestriction5",
        "Value restriction. The value '{0}' has been inferred to have generic type\n    {1}    \nEither define '{2}' as a simple data term, make it a function with explicit arguments or, if you do not intend for it to be generic, add a type annotation."
      );
      ( "RecoverableParseError",
        "syntax error"
      );
      ( "ReservedKeyword",
        "{0}"
      );
      ( "IndentationProblem",
        "{0}"
      );
      ( "OverrideInIntrinsicAugmentation",
        "Override implementations in augmentations are now deprecated. Override implementations should be given as part of the initial declaration of a type."
      );
      ( "OverrideInExtrinsicAugmentation",
        "Override implementations should be given as part of the initial declaration of a type."
      );
      ( "IntfImplInIntrinsicAugmentation",
        "Interface implementations in augmentations are now deprecated. Interface implementations should be given on the initial declaration of a type."
      );
      ( "IntfImplInExtrinsicAugmentation",
        "Interface implementations should be given on the initial declaration of a type."
      );
      ( "UnresolvedReferenceNoRange",
        "A required assembly reference is missing. You must add a reference to assembly '{0}'."
      );
      ( "UnresolvedPathReferenceNoRange",
        "The type referenced through '{0}' is defined in an assembly that is not referenced. You must add a reference to assembly '{1}'."
      );
      ( "HashIncludeNotAllowedInNonScript",
        "#I directives may only occur in F# script files (extensions .fsx or .fsscript). Either move this code to a script file, add a '-I' compiler option for this reference or delimit the directive with delimit it with '#if INTERACTIVE'/'#endif'."
      );
      ( "HashReferenceNotAllowedInNonScript",
        "#r directives may only occur in F# script files (extensions .fsx or .fsscript). Either move this code to a script file or replace this reference with the '-r' compiler option. If this directive is being executed as user input, you may delimit it with '#if INTERACTIVE'/'#endif'."
      );
      ( "HashDirectiveNotAllowedInNonScript",
        "This directive may only be used in F# script files (extensions .fsx or .fsscript). Either remove the directive, move this code to a script file or delimit the directive with '#if INTERACTIVE'/'#endif'."
      );
      ( "FileNameNotResolved",
        "Unable to find the file '{0}' in any of\n {1}"
      );
      ( "AssemblyNotResolved",
        "Assembly reference '{0}' was not found or is invalid"
      );
      ( "HashLoadedSourceHasIssues0",
        "One or more informational messages in loaded file.\n"
      );
      ( "HashLoadedSourceHasIssues1",
        "One or more warnings in loaded file.\n"
      );
      ( "HashLoadedSourceHasIssues2",
        "One or more errors in loaded file.\n"
      );
      ( "HashLoadedScriptConsideredSource",
        "Loaded files may only be F# source files (extension .fs). This F# script file (.fsx or .fsscript) will be treated as an F# source file"
      );
      ( "InvalidInternalsVisibleToAssemblyName1",
        "Invalid assembly name '{0}' from InternalsVisibleTo attribute in {1}"
      );
      ( "InvalidInternalsVisibleToAssemblyName2",
        "Invalid assembly name '{0}' from InternalsVisibleTo attribute (assembly filename not available)"
      );
      ( "LoadedSourceNotFoundIgnoring",
        "Could not load file '{0}' because it does not exist or is inaccessible"
      );
      ( "MSBuildReferenceResolutionError",
        "{0} (Code={1})"
      );
      ( "TargetInvocationExceptionWrapper",
        "internal error: {0}"
      );
      ( "NotUpperCaseConstructorWithoutRQA",
        "Lowercase discriminated union cases are only allowed when using RequireQualifiedAccess attribute"
      );
    ]