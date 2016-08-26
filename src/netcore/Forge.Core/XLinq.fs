[<AutoOpen>]
module Forge.XLinq
open System.Runtime.CompilerServices
open System.Xml.Linq



let xattr (name:string) value    = XAttribute (XName.Get name, value)


let inline localName x = (^a:(member Name:XName) x).LocalName

let inline private matchName (name:string) x = name = (localName x)

/// Helper function to filter a seq of XElements by matching their local name against the provided string
let inline private nameFilter name sqs = sqs |> Seq.filter ^ matchName name

let inline private hasNamed name sqs = sqs |> Seq.exists ^ matchName name

let inline private getNamed name sqs = sqs |> Seq.find ^ matchName name

let  inline private tryGetNamed name sqs = 
    (None, sqs) ||> Seq.fold (fun acc elm ->
        match acc with
        | Some _ -> acc
        | None   -> if matchName name elm then Some elm else None
    )


[<RequireQualifiedAccess>]
module XDoc =

    let elements (xdoc:#XDocument) = xdoc.Elements()

    let hasElement name (xdoc:#XDocument) =
        elements xdoc |> hasNamed name

    let getElement name (xdoc:#XDocument)  =
        elements xdoc |> getNamed name

    let tryGetElement name (xdoc:#XDocument)  =
        elements xdoc |> tryGetNamed name

    let getElements name (xdoc:#XDocument)  =
        elements xdoc |> nameFilter name


[<RequireQualifiedAccess>]
/// Functions for operating on XNodes
module XNode =

    /// Returns a seq of XElements that precede the XNode 
    let elementsBefore  (node:#XNode) = node.ElementsBeforeSelf()

    /// Returns a seq of XElements that follow the XNode
    let elementsAfter   (node:#XNode) = node.ElementsAfterSelf()
  
    /// Returns a seq of XElements that follow the XNode with a local name matching `name`
    let elementsAfterNamed (node:#XNode) name =
        elementsAfter node |> nameFilter name

    /// Returns a seq of XElements that precede the XNode with a local name matching `name`
    let elementsBeforeNamed (node:#XNode) name =
        elementsBefore node |> nameFilter name

    /// Returns seq of the XNode's ancestor XElements
    let ancestors (node:#XNode) = node.Ancestors()

    /// Returns the first ancestor of the XNode with a local name matching `name`
    let ancestorNamed (name:string) (node:#XNode) =
        ancestors node |> tryGetNamed name

    /// Returns all ancestors of the XNode with a local name matching `name`
    let ancestorsNamed (name:string) (node:#XNode) =
        ancestors node |> nameFilter name 

    /// Insert a sibling XNode before `node`
    let addBefore (insert:#XNode) (node:#XNode) =
        node.AddBeforeSelf insert
        node
    
    /// Insert a sibling XNode after `node`
    let addAfter (insert:#XNode) (node:#XNode) =
        node.AddAfterSelf insert
        node

        
    let next (node:#XNode) = node.NextNode        
    let previous (node:#XNode) = node.PreviousNode
    let parent (node:#XNode) = node.Parent

    let isBefore (target:#XNode) (node:#XNode) = node.IsBefore target
    let isAfter (target:#XNode)  (node:#XNode) = node.IsAfter target


[<RequireQualifiedAccess>]
/// Functions for operating on XContainers
module XCont =

    let descendants (cont:#XContainer) = cont.Descendants()
    
    let descendantNamed name (cont:#XContainer) =
        descendants cont |> tryGetNamed name

    let descendantsNamed name (cont:#XContainer) = 
        descendants cont |> nameFilter name
    
    let elements (cont:#XContainer) = cont.Elements()

    let hasElement name (cont:#XContainer) =
        elements cont |> hasNamed name

    let getElement name (cont:#XContainer) =
        elements cont |> getNamed name

    let tryGetElement name (cont:#XContainer) =
        elements cont |> tryGetNamed name

    let getElements name (cont:#XContainer) =
        elements cont |> nameFilter name

    let nodes (cont:#XContainer) = cont.Nodes()


[<RequireQualifiedAccess>]
module XAttr =
    let value (xattr:XAttribute) = xattr.Value
    let parent (xattr:XAttribute) = xattr.Parent
    let previous (xattr:XAttribute) = xattr.PreviousAttribute
    let next (xattr:XAttribute) = xattr.NextAttribute

[<RequireQualifiedAccess>]
/// Functions for operating on XElements
module XElem =

    let inline isNamed name (xelem:#XElement) = 
        matchName name xelem

    let inline notNamed name (xelem:#XElement) = 
        not ^ matchName name xelem

    let create (name:string) (content:seq<'a>) = 
        XElement (XName.Get name, Seq.toArray content)

    let value (xelem:#XElement) = xelem.Value

    let nodes (xelem:#XElement) = xelem.Nodes()
    
    let descendants (xelem:#XElement) = xelem.Descendants()
    
    let descendantNamed name (xelem:#XElement) =
        descendants xelem |> tryGetNamed name

    let descendantsNamed name (xelem:#XElement) = 
        descendants xelem |> nameFilter name
    
    let elements (xelem:#XElement) = xelem.Elements()

    let hasElement name (xelem:#XElement) =
        elements xelem |> hasNamed name

    let getElement name (xelem:#XElement) =
        elements xelem |> getNamed name

    let getElementValue name (xelem:#XElement) =
        elements xelem |> getNamed name |> value

    let tryGetElement name (xelem:#XElement) =
        elements xelem |> tryGetNamed name

    let tryGetElementValue name (xelem:#XElement) =
        elements xelem |> tryGetNamed name |> Option.map value

    let getElements name (xelem:#XElement) =
        elements xelem |> nameFilter name

    let attributes (xelem:#XElement) =
        xelem.Attributes()
    
    let hasAttribute name (xelem:#XElement) =
        attributes xelem |> hasNamed name

    let getAttribute name (xelem:#XElement) =
        xelem.Attribute ^ XName.Get name

    let getAttributeValue name (xelem:#XElement) =
        xelem.Attribute ^ XName.Get name |> XAttr.value

    let tryGetAttribute name (xelem:#XElement) =
        attributes xelem |> tryGetNamed name

    let tryGetAttributeValue name (xelem:#XElement) =
        tryGetAttribute name xelem |> Option.map XAttr.value

    let setAttribute name value (xelem:#XElement) =
        xelem.SetAttributeValue(XName.Get name, value)
        xelem
    
    let addAttribute (xattr:#XAttribute) (xelem:#XElement) =
        xelem.Add xattr
        xelem

    let setElement name value (xelem:#XElement) =
        xelem.SetElementValue(XName.Get name, value)
        xelem

    let addElement (child:XElement) (parent:XElement) =
        parent.Add child
        parent

    let addElements (children:#seq<XElement>) (parent:XElement) =
        parent.Add children
        parent

    /// Creates a new XElement and adds it as a child
    let inline addElem elmName value xelem =
        addElement (create elmName [value]) xelem



    
[<Extension>]
type XLinqSeqExtensions =
    [<Extension>] 
    static member Ancestors (source:seq<#XNode>) name = source.Ancestors ^ XName.Get name

    [<Extension>] 
    static member AncestorsAndSelf (source:seq<XElement>)  name =  source.AncestorsAndSelf ^ XName.Get name

    [<Extension>] 
    static member Attributes (source:seq<XElement>)  name =  source.Attributes ^ XName.Get name

    [<Extension>] 
    static member Descendants (source:seq<#XContainer>) name = source.Descendants ^ XName.Get name

    [<Extension>] 
    static member DescendantsAndSelf (source:seq<XElement>) name =  source.DescendantsAndSelf ^ XName.Get name

    [<Extension>] 
    static member Elements (source:seq<#XContainer>) name =  source.Elements ^ XName.Get name




