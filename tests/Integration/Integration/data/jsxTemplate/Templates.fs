module JsxTemplate.Templates

open Fable.Core

let inline html (template: string) : JSX.Element = JSX.html template

let inline htmlNested (template: string) : JSX.Element = html template

[<JSX.Component>]
let Direct (title: string) (child: JSX.Element) =
    JSX.html $"""<div title={title}>{child}</div>"""

[<JSX.Component>]
let InlineHelper (title: string) (child: JSX.Element) =
    html $"""<div title={title}>{child}</div>"""

[<JSX.Component>]
let NestedInlineHelper (title: string) (child: JSX.Element) =
    htmlNested $"""<div title={title}>{child}</div>"""

[<JSX.Component>]
let LetBound (title: string) (child: JSX.Element) =
    let template = $"""<div title={title}>{child}</div>"""
    JSX.html template

[<JSX.Component>]
let LetBoundInlineHelper (title: string) (child: JSX.Element) =
    let template = $"""<div title={title}>{child}</div>"""
    html template

[<JSX.Component>]
let Piped (title: string) (child: JSX.Element) =
    $"""<div title={title}>{child}</div>""" |> html

[<JSX.Component>]
let NestedTemplate (title: string) (count: int) =
    html $"""<div title={title}>{html $"<span>{count}</span>"}</div>"""

[<JSX.Component>]
let StringHole (title: string) (count: int) =
    let label = $"{title}: {count}"
    html $"""<div title={label}>{count}</div>"""

[<JSX.Component>]
let ConvertedHole (title: string) (count: int) (value: obj) (items: int list) (id: System.Guid) =
    html $"""<div title={string count} id={id}>{value}{items}{$"{value}"}{string value}</div>"""

[<JSX.Component>]
let FunctionHole (onClick: int -> int -> unit) (child: JSX.Element) =
    html $"""<div onClick={fun _ -> onClick 1 2} onKeyDown={onClick}>{child}</div>"""

let notJsx (title: string) (value: obj) = $"""<div title={title}>{value}</div>"""
