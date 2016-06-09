namespace Fable.Import
open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Import.JS

module ReactRedux =
    type [<Import("ElementClass","react-redux")>] ElementClass() =
        interface Component<obj, obj>


    and ClassDecorator =
        [<Emit("$0($1...)")>] abstract Invoke: ``component``: 'T -> 'T

    and MapStateToProps =
        [<Emit("$0($1...)")>] abstract Invoke: state: obj * ?ownProps: obj -> obj

    and MapDispatchToPropsFunction =
        [<Emit("$0($1...)")>] abstract Invoke: dispatch: Dispatch * ?ownProps: obj -> obj

    and MapDispatchToPropsObject =
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: name: string -> ActionCreator with get, set

    and MergeProps =
        [<Emit("$0($1...)")>] abstract Invoke: stateProps: obj * dispatchProps: obj * ownProps: obj -> obj

    and Options =
        abstract ``pure``: bool with get, set

    and Property =
        abstract store: Store option with get, set
        abstract children: Function option with get, set

    and [<Import("Provider","react-redux")>] Provider() =
        interface Component<Property, obj>


    type [<Import("*","react-redux")>] Globals =
        static member connect(?mapStateToProps: MapStateToProps, ?mapDispatchToProps: U2<MapDispatchToPropsFunction, MapDispatchToPropsObject>, ?mergeProps: MergeProps, ?options: Options): ClassDecorator = failwith "JS only"
