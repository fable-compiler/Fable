/// Same file name as Naming/First/Types.fs — see the comment there.
module Fable.Tests.Naming.Second.Types

type Colour =
    | Red
    | Green

let name =
    function
    | Red -> "red"
    | Green -> "green"
