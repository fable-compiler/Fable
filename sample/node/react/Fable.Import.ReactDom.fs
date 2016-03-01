namespace Fable.Import
open System
open Fable.Core
open Fable.Import.JS
open Fable.Import.React

module ReactDom =
    module DOM =
        type Globals =
            abstract version: string with get, set
            abstract findDOMNode: instance: ReactInstance -> 'E
            abstract findDOMNode: instance: ReactInstance -> Element
            abstract render: element: DOMElement<'P> * container: Element * ?callback: Func<Element, obj> -> Element
            abstract render: element: ClassicElement<'P> * container: Element * ?callback: Func<ClassicComponent<'P, 'S>, obj> -> ClassicComponent<'P, 'S>
            abstract render: element: ReactElement<'P> * container: Element * ?callback: Func<Component<'P, 'S>, obj> -> Component<'P, 'S>
            abstract unmountComponentAtNode: container: Element -> bool
            abstract unstable_batchedUpdates: callback: Func<'A, 'B, obj> * a: 'A * b: 'B -> unit
            abstract unstable_batchedUpdates: callback: Func<'A, obj> * a: 'A -> unit
            abstract unstable_batchedUpdates: callback: Func<obj> -> unit
            abstract unstable_renderSubtreeIntoContainer: parentComponent: Component<obj, obj> * nextElement: DOMElement<'P> * container: Element * ?callback: Func<Element, obj> -> Element
            abstract unstable_renderSubtreeIntoContainer: parentComponent: Component<obj, obj> * nextElement: ClassicElement<'P> * container: Element * ?callback: Func<ClassicComponent<'P, 'S>, obj> -> ClassicComponent<'P, 'S>
            abstract unstable_renderSubtreeIntoContainer: parentComponent: Component<obj, obj> * nextElement: ReactElement<'P> * container: Element * ?callback: Func<Component<'P, 'S>, obj> -> Component<'P, 'S>

        let [<Import("react-dom")>] Globals: Globals = failwith "JS only"


    module DOMServer =
        type Globals =
            abstract version: string with get, set
            abstract renderToString: element: ReactElement<obj> -> string
            abstract renderToStaticMarkup: element: ReactElement<obj> -> string

        let [<Import("react-dom/server")>] Globals: Globals = failwith "JS only"

