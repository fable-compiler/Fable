(**
 - title: Super Fable Mario
 - tagline: Mario clone using HTML5 canvas
 - app-style: height:384px; width:512px; margin:20px auto 20px auto; position:relative;
 - intro: Mario clone, based on functional reactive [sample written in
   Elm](http://debug.elm-lang.org/edit/Mario.elm). The Fable version is using HTML 5
   canvas to render the background and an `img` tag showing the Mario (using animated GIFs).
   You can find the [full source code on GitHub](https://github.com/fsprojects/Fable/blob/master/samples/browser/mario/mario.fsx).

   To see how it works, use the left, right and up buttons to play. Our Mario respects
   no boundaries!
*)
(*** hide ***)
#r "node_modules/fable-core/Fable.Core.dll"
open Fable.Core
open Fable.Import.Browser

type Style =
  { border : string }

let [<Import("default","virtual-dom/h")>] h(arg1: string, arg2: obj, arg3: obj[]): obj = failwith "JS only"
let [<Import("default","virtual-dom/diff")>] diff:obj = failwith "JS only"
let [<Import("default","virtual-dom/patch")>] patch:obj = failwith "JS only"
let [<Import("default","virtual-dom/create-element")>] createElement:obj -> Node = failwith "JS only"

let hello (count) =
  h("div", createObj [ "style" ==> { border = "1px solid red" } ], [| string count |])

let tree = hello 42
let rootNode= createElement tree
document.body.appendChild(rootNode)


(*
// 1: Create a function that declares what the DOM should look like
function render(count)  {
    return h('div', {
        style: {
            textAlign: 'center',
            lineHeight: (100 + count) + 'px',
            border: '1px solid red',
            width: (100 + count) + 'px',
            height: (100 + count) + 'px'
        }
    }, [String(count)]);
}

// 2: Initialise the document
var count = 0;      // We need some app data. Here we just store a count.

var tree = render(count);               // We need an initial tree
var rootNode = createElement(tree);     // Create an initial root DOM node ...
document.body.appendChild(rootNode);    // ... and it should be in the document

// 3: Wire up the update logic
setInterval(function () {
      count++;

      var newTree = render(count);
      var patches = diff(tree, newTree);
      rootNode = patch(rootNode, patches);
      tree = newTree;
}, 1000);
*)
