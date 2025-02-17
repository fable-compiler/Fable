module RemoveUnitIdents

type Model = unit

let update (model: Model) =
    model, ()

update () |> ignore