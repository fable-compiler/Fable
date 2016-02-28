namespace Fable.Import
open System
open Fabel.Core
open Fabel.Import.JS

type Thenable<'R> =
    abstract ``then``: ?onfulfilled: Func<'R, U2<TResult, Thenable<TResult>>> * ?onrejected: Func<obj, U2<TResult, Thenable<TResult>>> -> Thenable<TResult>
    abstract ``then``: ?onfulfilled: Func<'R, U2<TResult, Thenable<TResult>>> * ?onrejected: Func<obj, unit> -> Thenable<TResult>

and Promise<'T> =
    inherit Thenable<'T>
    abstract ``then``: ?onfulfilled: Func<'T, U2<TResult, Thenable<TResult>>> * ?onrejected: Func<obj, U2<TResult, Thenable<TResult>>> -> Promise<TResult>
    abstract ``then``: ?onfulfilled: Func<'T, U2<TResult, Thenable<TResult>>> * ?onrejected: Func<obj, unit> -> Promise<TResult>
    abstract catch: ?onrejected: Func<obj, U2<'T, Thenable<'T>>> -> Promise<'T>

and PromiseConstructor =
    abstract createNew: executor: Func<Func<U2<'T, Thenable<'T>>, unit>, Func<obj, unit>, unit> -> Promise<'T>
    abstract all: values: ResizeArray<U2<'T, Thenable<'T>>> -> Promise<ResizeArray<'T>>
    abstract race: values: ResizeArray<U2<'T, Thenable<'T>>> -> Promise<'T>
    abstract reject: reason: obj -> Promise<unit>
    abstract reject: reason: obj -> Promise<'T>
    abstract resolve: value: U2<'T, Thenable<'T>> -> Promise<'T>
    abstract resolve: unit -> Promise<unit>

module Globals =
    let [<Global>] Promise: PromiseConstructor = failwith "JS only"

module vscode =
    type Command =
        abstract title: string with get, set
        abstract command: string with get, set
        abstract arguments: ResizeArray<obj> option with get, set

    and TextLine =
        abstract lineNumber: float with get, set
        abstract text: string with get, set
        abstract range: Range with get, set
        abstract rangeIncludingLineBreak: Range with get, set
        abstract firstNonWhitespaceCharacterIndex: float with get, set
        abstract isEmptyOrWhitespace: bool with get, set

    and TextDocument =
        abstract uri: Uri with get, set
        abstract fileName: string with get, set
        abstract isUntitled: bool with get, set
        abstract languageId: string with get, set
        abstract version: float with get, set
        abstract isDirty: bool with get, set
        abstract lineCount: float with get, set
        abstract save: unit -> Thenable<bool>
        abstract lineAt: line: float -> TextLine
        abstract lineAt: position: Position -> TextLine
        abstract offsetAt: position: Position -> float
        abstract positionAt: offset: float -> Position
        abstract getText: ?range: Range -> string
        abstract getWordRangeAtPosition: position: Position -> Range
        abstract validateRange: range: Range -> Range
        abstract validatePosition: position: Position -> Position

    and [<Import("vscode?get=Position")>] Position(line: float, character: float) =
        member __.line with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.character with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.isBefore(other: Position): bool = failwith "JS only"
        member __.isBeforeOrEqual(other: Position): bool = failwith "JS only"
        member __.isAfter(other: Position): bool = failwith "JS only"
        member __.isAfterOrEqual(other: Position): bool = failwith "JS only"
        member __.isEqual(other: Position): bool = failwith "JS only"
        member __.compareTo(other: Position): float = failwith "JS only"
        member __.translate(?lineDelta: float, ?characterDelta: float): Position = failwith "JS only"
        member __.``with``(?line: float, ?character: float): Position = failwith "JS only"

    and [<Import("vscode?get=Range")>] Range(startLine: float, startCharacter: float, endLine: float, endCharacter: float) =
        member __.start with get(): Position = failwith "JS only" and set(v: Position): unit = failwith "JS only"
        member __.``end`` with get(): Position = failwith "JS only" and set(v: Position): unit = failwith "JS only"
        member __.isEmpty with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.isSingleLine with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.contains(positionOrRange: U2<Position, Range>): bool = failwith "JS only"
        member __.isEqual(other: Range): bool = failwith "JS only"
        member __.intersection(range: Range): Range = failwith "JS only"
        member __.union(other: Range): Range = failwith "JS only"
        member __.``with``(?start: Position, ?``end``: Position): Range = failwith "JS only"

    and [<Import("vscode?get=Selection")>] Selection(anchorLine: float, anchorCharacter: float, activeLine: float, activeCharacter: float) =
        inherit Range()
        member __.anchor with get(): Position = failwith "JS only" and set(v: Position): unit = failwith "JS only"
        member __.active with get(): Position = failwith "JS only" and set(v: Position): unit = failwith "JS only"
        member __.isReversed with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"

    and TextEditorSelectionChangeEvent =
        abstract textEditor: TextEditor with get, set
        abstract selections: ResizeArray<Selection> with get, set

    and TextEditorOptionsChangeEvent =
        abstract textEditor: TextEditor with get, set
        abstract options: TextEditorOptions with get, set

    and TextEditorOptions =
        abstract tabSize: float with get, set
        abstract insertSpaces: bool with get, set

    and TextEditorDecorationType =
        abstract key: string with get, set
        abstract dispose: unit -> unit

    and ThemableDecorationRenderOptions =
        abstract backgroundColor: string option with get, set
        abstract outlineColor: string option with get, set
        abstract outlineStyle: string option with get, set
        abstract outlineWidth: string option with get, set
        abstract borderColor: string option with get, set
        abstract borderRadius: string option with get, set
        abstract borderSpacing: string option with get, set
        abstract borderStyle: string option with get, set
        abstract borderWidth: string option with get, set
        abstract textDecoration: string option with get, set
        abstract cursor: string option with get, set
        abstract color: string option with get, set
        abstract gutterIconPath: string option with get, set
        abstract overviewRulerColor: string option with get, set

    and DecorationRenderOptions =
        inherit ThemableDecorationRenderOptions
        abstract isWholeLine: bool option with get, set
        abstract overviewRulerLane: OverviewRulerLane option with get, set
        abstract light: ThemableDecorationRenderOptions option with get, set
        abstract dark: ThemableDecorationRenderOptions option with get, set

    and DecorationOptions =
        abstract range: Range with get, set
        abstract hoverMessage: U2<MarkedString, ResizeArray<MarkedString>> with get, set

    and TextEditor =
        abstract document: TextDocument with get, set
        abstract selection: Selection with get, set
        abstract selections: ResizeArray<Selection> with get, set
        abstract options: TextEditorOptions with get, set
        abstract edit: callback: Func<TextEditorEdit, unit> -> Thenable<bool>
        abstract setDecorations: decorationType: TextEditorDecorationType * rangesOrOptions: U2<ResizeArray<Range>, ResizeArray<DecorationOptions>> -> unit
        abstract revealRange: range: Range * ?revealType: TextEditorRevealType -> unit
        abstract show: ?column: ViewColumn -> unit
        abstract hide: unit -> unit

    and TextEditorEdit =
        abstract replace: location: U3<Position, Range, Selection> * value: string -> unit
        abstract insert: location: Position * value: string -> unit
        abstract delete: location: U2<Range, Selection> -> unit

    and [<Import("vscode?get=Uri")>] Uri() =
        member __.scheme with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.authority with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.path with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.query with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.fragment with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.fsPath with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.file(path: string): Uri = failwith "JS only"
        member __.parse(value: string): Uri = failwith "JS only"
        member __.toString(): string = failwith "JS only"
        member __.toJSON(): obj = failwith "JS only"

    and CancellationToken =
        abstract isCancellationRequested: bool with get, set
        abstract onCancellationRequested: Event<obj> with get, set

    and [<Import("vscode?get=CancellationTokenSource")>] CancellationTokenSource() =
        member __.token with get(): CancellationToken = failwith "JS only" and set(v: CancellationToken): unit = failwith "JS only"
        member __.cancel(): unit = failwith "JS only"
        member __.dispose(): unit = failwith "JS only"

    and [<Import("vscode?get=Disposable")>] Disposable(callOnDispose: (obj->obj)) =
        member __.from([<ParamArray>] disposableLikes: obj[]): Disposable = failwith "JS only"
        member __.dispose(): obj = failwith "JS only"

    and Event<'T> =
        interface end

    and FileSystemWatcher =
        // inherit Disposable // TODO
        abstract ignoreCreateEvents: bool with get, set
        abstract ignoreChangeEvents: bool with get, set
        abstract ignoreDeleteEvents: bool with get, set
        abstract onDidCreate: Event<Uri> with get, set
        abstract onDidChange: Event<Uri> with get, set
        abstract onDidDelete: Event<Uri> with get, set

    and QuickPickItem =
        abstract label: string with get, set
        abstract description: string with get, set

    and QuickPickOptions =
        abstract matchOnDescription: bool option with get, set
        abstract placeHolder: string option with get, set

    and MessageItem =
        abstract title: string with get, set

    and InputBoxOptions =
        abstract value: string option with get, set
        abstract prompt: string option with get, set
        abstract placeHolder: string option with get, set
        abstract password: bool option with get, set

    and DocumentFilter =
        abstract language: string option with get, set
        abstract scheme: string option with get, set
        abstract pattern: string option with get, set

    and DocumentSelector =
        U3<string, DocumentFilter, ResizeArray<U2<string, DocumentFilter>>>

    and CodeActionContext =
        abstract diagnostics: ResizeArray<Diagnostic> with get, set

    and CodeActionProvider =
        abstract provideCodeActions: document: TextDocument * range: Range * context: CodeActionContext * token: CancellationToken -> U2<ResizeArray<Command>, Thenable<ResizeArray<Command>>>

    and [<Import("vscode?get=CodeLens")>] CodeLens(range: Range, ?command: Command) =
        member __.range with get(): Range = failwith "JS only" and set(v: Range): unit = failwith "JS only"
        member __.command with get(): Command = failwith "JS only" and set(v: Command): unit = failwith "JS only"
        member __.isResolved with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"

    and CodeLensProvider =
        abstract provideCodeLenses: document: TextDocument * token: CancellationToken -> U2<ResizeArray<CodeLens>, Thenable<ResizeArray<CodeLens>>>
        abstract resolveCodeLens: codeLens: CodeLens * token: CancellationToken -> U2<CodeLens, Thenable<CodeLens>>

    and Definition =
        U2<Location, ResizeArray<Location>>

    and DefinitionProvider =
        abstract provideDefinition: document: TextDocument * position: Position * token: CancellationToken -> U2<Definition, Thenable<Definition>>

    and MarkedString =
        U2<string, obj>

    and [<Import("vscode?get=Hover")>] Hover(contents: U2<MarkedString, ResizeArray<MarkedString>>, ?range: Range) =
        member __.contents with get(): ResizeArray<MarkedString> = failwith "JS only" and set(v: ResizeArray<MarkedString>): unit = failwith "JS only"
        member __.range with get(): Range = failwith "JS only" and set(v: Range): unit = failwith "JS only"

    and HoverProvider =
        abstract provideHover: document: TextDocument * position: Position * token: CancellationToken -> U2<Hover, Thenable<Hover>>

    and [<Import("vscode?get=DocumentHighlight")>] DocumentHighlight(range: Range, ?kind: DocumentHighlightKind) =
        member __.range with get(): Range = failwith "JS only" and set(v: Range): unit = failwith "JS only"
        member __.kind with get(): DocumentHighlightKind = failwith "JS only" and set(v: DocumentHighlightKind): unit = failwith "JS only"

    and DocumentHighlightProvider =
        abstract provideDocumentHighlights: document: TextDocument * position: Position * token: CancellationToken -> U2<ResizeArray<DocumentHighlight>, Thenable<ResizeArray<DocumentHighlight>>>

    and [<Import("vscode?get=SymbolInformation")>] SymbolInformation(name: string, kind: SymbolKind, range: Range, ?uri: Uri, ?containerName: string) =
        member __.name with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.containerName with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.kind with get(): SymbolKind = failwith "JS only" and set(v: SymbolKind): unit = failwith "JS only"
        member __.location with get(): Location = failwith "JS only" and set(v: Location): unit = failwith "JS only"

    and DocumentSymbolProvider =
        abstract provideDocumentSymbols: document: TextDocument * token: CancellationToken -> U2<ResizeArray<SymbolInformation>, Thenable<ResizeArray<SymbolInformation>>>

    and WorkspaceSymbolProvider =
        abstract provideWorkspaceSymbols: query: string * token: CancellationToken -> U2<ResizeArray<SymbolInformation>, Thenable<ResizeArray<SymbolInformation>>>

    and ReferenceContext =
        abstract includeDeclaration: bool with get, set

    and ReferenceProvider =
        abstract provideReferences: document: TextDocument * position: Position * context: ReferenceContext * token: CancellationToken -> U2<ResizeArray<Location>, Thenable<ResizeArray<Location>>>

    and [<Import("vscode?get=TextEdit")>] TextEdit(range: Range, newText: string) =
        member __.range with get(): Range = failwith "JS only" and set(v: Range): unit = failwith "JS only"
        member __.newText with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.replace(range: Range, newText: string): TextEdit = failwith "JS only"
        member __.insert(position: Position, newText: string): TextEdit = failwith "JS only"
        member __.delete(range: Range): TextEdit = failwith "JS only"

    and [<Import("vscode?get=WorkspaceEdit")>] WorkspaceEdit() =
        member __.size with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.replace(uri: Uri, range: Range, newText: string): unit = failwith "JS only"
        member __.insert(uri: Uri, position: Position, newText: string): unit = failwith "JS only"
        member __.delete(uri: Uri, range: Range): unit = failwith "JS only"
        member __.has(uri: Uri): bool = failwith "JS only"
        member __.set(uri: Uri, edits: ResizeArray<TextEdit>): unit = failwith "JS only"
        member __.get(uri: Uri): ResizeArray<TextEdit> = failwith "JS only"
        member __.entries(): ResizeArray<obj> = failwith "JS only"

    and RenameProvider =
        abstract provideRenameEdits: document: TextDocument * position: Position * newName: string * token: CancellationToken -> U2<WorkspaceEdit, Thenable<WorkspaceEdit>>

    and FormattingOptions =
        abstract tabSize: float with get, set
        abstract insertSpaces: bool with get, set

    and DocumentFormattingEditProvider =
        abstract provideDocumentFormattingEdits: document: TextDocument * options: FormattingOptions * token: CancellationToken -> U2<ResizeArray<TextEdit>, Thenable<ResizeArray<TextEdit>>>

    and DocumentRangeFormattingEditProvider =
        abstract provideDocumentRangeFormattingEdits: document: TextDocument * range: Range * options: FormattingOptions * token: CancellationToken -> U2<ResizeArray<TextEdit>, Thenable<ResizeArray<TextEdit>>>

    and OnTypeFormattingEditProvider =
        abstract provideOnTypeFormattingEdits: document: TextDocument * position: Position * ch: string * options: FormattingOptions * token: CancellationToken -> U2<ResizeArray<TextEdit>, Thenable<ResizeArray<TextEdit>>>

    and [<Import("vscode?get=ParameterInformation")>] ParameterInformation(label: string, ?documentation: string) =
        member __.label with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.documentation with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"

    and [<Import("vscode?get=SignatureInformation")>] SignatureInformation(label: string, ?documentation: string) =
        member __.label with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.documentation with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.parameters with get(): ResizeArray<ParameterInformation> = failwith "JS only" and set(v: ResizeArray<ParameterInformation>): unit = failwith "JS only"

    and [<Import("vscode?get=SignatureHelp")>] SignatureHelp() =
        member __.signatures with get(): ResizeArray<SignatureInformation> = failwith "JS only" and set(v: ResizeArray<SignatureInformation>): unit = failwith "JS only"
        member __.activeSignature with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.activeParameter with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

    and SignatureHelpProvider =
        abstract provideSignatureHelp: document: TextDocument * position: Position * token: CancellationToken -> U2<SignatureHelp, Thenable<SignatureHelp>>

    and [<Import("vscode?get=CompletionItem")>] CompletionItem(label: string) =
        member __.label with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.kind with get(): CompletionItemKind = failwith "JS only" and set(v: CompletionItemKind): unit = failwith "JS only"
        member __.detail with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.documentation with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.sortText with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.filterText with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.insertText with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.textEdit with get(): TextEdit = failwith "JS only" and set(v: TextEdit): unit = failwith "JS only"

    and CompletionItemProvider =
        abstract provideCompletionItems: document: TextDocument * position: Position * token: CancellationToken -> U2<ResizeArray<CompletionItem>, Thenable<ResizeArray<CompletionItem>>>
        abstract resolveCompletionItem: item: CompletionItem * token: CancellationToken -> U2<CompletionItem, Thenable<CompletionItem>>

    and CharacterPair =
        obj

    and CommentRule =
        abstract lineComment: string option with get, set
        abstract blockComment: CharacterPair option with get, set

    and IndentationRule =
        abstract decreaseIndentPattern: RegExp with get, set
        abstract increaseIndentPattern: RegExp with get, set
        abstract indentNextLinePattern: RegExp option with get, set
        abstract unIndentedLinePattern: RegExp option with get, set

    and EnterAction =
        abstract indentAction: IndentAction with get, set
        abstract appendText: string option with get, set
        abstract removeText: float option with get, set

    and OnEnterRule =
        abstract beforeText: RegExp with get, set
        abstract afterText: RegExp option with get, set
        abstract action: EnterAction with get, set

    and LanguageConfiguration =
        abstract comments: CommentRule option with get, set
        abstract brackets: ResizeArray<CharacterPair> option with get, set
        abstract wordPattern: RegExp option with get, set
        abstract indentationRules: IndentationRule option with get, set
        abstract onEnterRules: ResizeArray<OnEnterRule> option with get, set
        abstract ___electricCharacterSupport: obj option with get, set
        abstract ___characterPairSupport: obj option with get, set

    and WorkspaceConfiguration =
        abstract get: section: string * ?defaultValue: 'T -> 'T
        abstract has: section: string -> bool

    and [<Import("vscode?get=Location")>] Location(uri: Uri, rangeOrPosition: U2<Range, Position>) =
        member __.uri with get(): Uri = failwith "JS only" and set(v: Uri): unit = failwith "JS only"
        member __.range with get(): Range = failwith "JS only" and set(v: Range): unit = failwith "JS only"

    and [<Import("vscode?get=Diagnostic")>] Diagnostic(range: Range, message: string, ?severity: DiagnosticSeverity) =
        member __.range with get(): Range = failwith "JS only" and set(v: Range): unit = failwith "JS only"
        member __.message with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.source with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.severity with get(): DiagnosticSeverity = failwith "JS only" and set(v: DiagnosticSeverity): unit = failwith "JS only"
        member __.code with get(): U2<string, float> = failwith "JS only" and set(v: U2<string, float>): unit = failwith "JS only"

    and DiagnosticCollection =
        abstract name: string with get, set
        abstract set: uri: Uri * diagnostics: ResizeArray<Diagnostic> -> unit
        abstract delete: uri: Uri -> unit
        abstract set: entries: ResizeArray<obj> -> unit
        abstract clear: unit -> unit
        abstract dispose: unit -> unit

    and OutputChannel =
        abstract name: string with get, set
        abstract append: value: string -> unit
        abstract appendLine: value: string -> unit
        abstract clear: unit -> unit
        abstract show: ?column: ViewColumn -> unit
        abstract hide: unit -> unit
        abstract dispose: unit -> unit

    and StatusBarItem =
        abstract alignment: StatusBarAlignment with get, set
        abstract priority: float with get, set
        abstract text: string with get, set
        abstract tooltip: string with get, set
        abstract color: string with get, set
        abstract command: string with get, set
        abstract show: unit -> unit
        abstract hide: unit -> unit
        abstract dispose: unit -> unit

    and Extension<'T> =
        abstract id: string with get, set
        abstract extensionPath: string with get, set
        abstract isActive: bool with get, set
        abstract packageJSON: obj with get, set
        abstract exports: 'T with get, set
        abstract activate: unit -> Thenable<'T>

    and ExtensionContext =
        abstract subscriptions: ResizeArray<obj> with get, set
        abstract workspaceState: Memento with get, set
        abstract globalState: Memento with get, set
        abstract extensionPath: string with get, set
        abstract asAbsolutePath: relativePath: string -> string

    and Memento =
        abstract get: key: string * ?defaultValue: 'T -> 'T
        abstract update: key: string * value: obj -> Thenable<unit>

    and TextDocumentContentChangeEvent =
        abstract range: Range with get, set
        abstract rangeLength: float with get, set
        abstract text: string with get, set

    and TextDocumentChangeEvent =
        abstract document: TextDocument with get, set
        abstract contentChanges: ResizeArray<TextDocumentContentChangeEvent> with get, set

    type Globals =
        abstract version: string with get, set
        type TextEditorRevealType = 
            | Default = 0
            | InCenter = 1
            | InCenterIfOutsideViewport = 2

        type OverviewRulerLane = 
            | Left = 1
            | Center = 2
            | Right = 4
            | Full = 7

        type DocumentHighlightKind = 
            | Text = 0
            | Read = 1
            | Write = 2

        type SymbolKind = 
            | File = 0
            | Module = 1
            | Namespace = 2
            | Package = 3
            | Class = 4
            | Method = 5
            | Property = 6
            | Field = 7
            | Constructor = 8
            | Enum = 9
            | Interface = 10
            | Function = 11
            | Variable = 12
            | Constant = 13
            | String = 14
            | Number = 15
            | Boolean = 16
            | Array = 17

        type CompletionItemKind = 
            | Text = 0
            | Method = 1
            | Function = 2
            | Constructor = 3
            | Field = 4
            | Variable = 5
            | Class = 6
            | Interface = 7
            | Module = 8
            | Property = 9
            | Unit = 10
            | Value = 11
            | Enum = 12
            | Keyword = 13
            | Snippet = 14
            | Color = 15
            | File = 16
            | Reference = 17

        type IndentAction = 
            | None = 0
            | Indent = 1
            | IndentOutdent = 2
            | Outdent = 3

        type DiagnosticSeverity = 
            | Error = 0
            | Warning = 1
            | Information = 2
            | Hint = 3

        type ViewColumn = 
            | One = 1
            | Two = 2
            | Three = 3

        type StatusBarAlignment = 
            | Left = 0
            | Right = 1

    let [<Import("vscode")>] Globals: Globals = failwith "JS only"


    module commands =
        type Globals =
            abstract registerCommand: command: string * callback: Func<obj, obj> * ?thisArg: obj -> Disposable
            abstract registerTextEditorCommand: command: string * callback: Func<TextEditor, TextEditorEdit, unit> * ?thisArg: obj -> Disposable
            abstract executeCommand: command: string * [<ParamArray>] rest: obj[] -> Thenable<'T>
            abstract getCommands: ?filterInternal: bool -> Thenable<ResizeArray<string>>

        let [<Import("vscode?get=commands")>] Globals: Globals = failwith "JS only"


    module window =
        type Globals =
            abstract activeTextEditor: TextEditor with get, set
            abstract visibleTextEditors: ResizeArray<TextEditor> with get, set
            abstract onDidChangeActiveTextEditor: Event<TextEditor> with get, set
            abstract onDidChangeTextEditorSelection: Event<TextEditorSelectionChangeEvent> with get, set
            abstract onDidChangeTextEditorOptions: Event<TextEditorOptionsChangeEvent> with get, set
            abstract showTextDocument: document: TextDocument * ?column: ViewColumn -> Thenable<TextEditor>
            abstract createTextEditorDecorationType: options: DecorationRenderOptions -> TextEditorDecorationType
            abstract showInformationMessage: message: string * [<ParamArray>] items: string[] -> Thenable<string>
            abstract showInformationMessage: message: string * [<ParamArray>] items: 'T[] -> Thenable<'T>
            abstract showWarningMessage: message: string * [<ParamArray>] items: string[] -> Thenable<string>
            abstract showWarningMessage: message: string * [<ParamArray>] items: 'T[] -> Thenable<'T>
            abstract showErrorMessage: message: string * [<ParamArray>] items: string[] -> Thenable<string>
            abstract showErrorMessage: message: string * [<ParamArray>] items: 'T[] -> Thenable<'T>
            abstract showQuickPick: items: U2<ResizeArray<string>, Thenable<ResizeArray<string>>> * ?options: QuickPickOptions -> Thenable<string>
            abstract showQuickPick: items: U2<ResizeArray<'T>, Thenable<ResizeArray<'T>>> * ?options: QuickPickOptions -> Thenable<'T>
            abstract showInputBox: ?options: InputBoxOptions -> Thenable<string>
            abstract createOutputChannel: name: string -> OutputChannel
            abstract setStatusBarMessage: text: string -> Disposable
            abstract setStatusBarMessage: text: string * hideAfterTimeout: float -> Disposable
            abstract setStatusBarMessage: text: string * hideWhenDone: Thenable<obj> -> Disposable
            abstract createStatusBarItem: ?alignment: StatusBarAlignment * ?priority: float -> StatusBarItem

        let [<Import("vscode?get=window")>] Globals: Globals = failwith "JS only"


    module workspace =
        type Globals =
            abstract rootPath: string with get, set
            abstract textDocuments: ResizeArray<TextDocument> with get, set
            abstract onDidOpenTextDocument: Event<TextDocument> with get, set
            abstract onDidCloseTextDocument: Event<TextDocument> with get, set
            abstract onDidChangeTextDocument: Event<TextDocumentChangeEvent> with get, set
            abstract onDidSaveTextDocument: Event<TextDocument> with get, set
            abstract onDidChangeConfiguration: Event<unit> with get, set
            abstract createFileSystemWatcher: globPattern: string * ?ignoreCreateEvents: bool * ?ignoreChangeEvents: bool * ?ignoreDeleteEvents: bool -> FileSystemWatcher
            abstract asRelativePath: pathOrUri: U2<string, Uri> -> string
            abstract findFiles: ``include``: string * exclude: string * ?maxResults: float -> Thenable<ResizeArray<Uri>>
            abstract saveAll: ?includeUntitled: bool -> Thenable<bool>
            abstract applyEdit: edit: WorkspaceEdit -> Thenable<bool>
            abstract openTextDocument: uri: Uri -> Thenable<TextDocument>
            abstract openTextDocument: fileName: string -> Thenable<TextDocument>
            abstract getConfiguration: ?section: string -> WorkspaceConfiguration

        let [<Import("vscode?get=workspace")>] Globals: Globals = failwith "JS only"


    module languages =
        type Globals =
            abstract getLanguages: unit -> Thenable<ResizeArray<string>>
            abstract ``match``: selector: DocumentSelector * document: TextDocument -> float
            abstract createDiagnosticCollection: ?name: string -> DiagnosticCollection
            abstract registerCompletionItemProvider: selector: DocumentSelector * provider: CompletionItemProvider * [<ParamArray>] triggerCharacters: string[] -> Disposable
            abstract registerCodeActionsProvider: selector: DocumentSelector * provider: CodeActionProvider -> Disposable
            abstract registerCodeLensProvider: selector: DocumentSelector * provider: CodeLensProvider -> Disposable
            abstract registerDefinitionProvider: selector: DocumentSelector * provider: DefinitionProvider -> Disposable
            abstract registerHoverProvider: selector: DocumentSelector * provider: HoverProvider -> Disposable
            abstract registerDocumentHighlightProvider: selector: DocumentSelector * provider: DocumentHighlightProvider -> Disposable
            abstract registerDocumentSymbolProvider: selector: DocumentSelector * provider: DocumentSymbolProvider -> Disposable
            abstract registerWorkspaceSymbolProvider: provider: WorkspaceSymbolProvider -> Disposable
            abstract registerReferenceProvider: selector: DocumentSelector * provider: ReferenceProvider -> Disposable
            abstract registerRenameProvider: selector: DocumentSelector * provider: RenameProvider -> Disposable
            abstract registerDocumentFormattingEditProvider: selector: DocumentSelector * provider: DocumentFormattingEditProvider -> Disposable
            abstract registerDocumentRangeFormattingEditProvider: selector: DocumentSelector * provider: DocumentRangeFormattingEditProvider -> Disposable
            abstract registerOnTypeFormattingEditProvider: selector: DocumentSelector * provider: OnTypeFormattingEditProvider * firstTriggerCharacter: string * [<ParamArray>] moreTriggerCharacter: string[] -> Disposable
            abstract registerSignatureHelpProvider: selector: DocumentSelector * provider: SignatureHelpProvider * [<ParamArray>] triggerCharacters: string[] -> Disposable
            abstract setLanguageConfiguration: language: string * configuration: LanguageConfiguration -> Disposable

        let [<Import("vscode?get=languages")>] Globals: Globals = failwith "JS only"


    module extensions =
        type Globals =
            abstract all: ResizeArray<Extension<obj>> with get, set
            abstract getExtension: extensionId: string -> Extension<obj>
            abstract getExtension: extensionId: string -> Extension<'T>

        let [<Import("vscode?get=extensions")>] Globals: Globals = failwith "JS only"

