namespace Fable.Import
open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Import.JS
open Fable.Import.React
open Fable.Import.Redux

// Type definitions for react-redux v. 4.4.0, generated from Typescript
module ReactRedux =
    type [<Import("ElementClass","react-redux")>] ElementClass<'P, 'S when 'P :> Property<'P>>(?props: 'P, ?context: obj) =
        inherit Component<'P, 'S>(?props = props, ?context = context)


    and ClassDecorator =
        [<Emit("$0($1...)")>] abstract Invoke: ``component``: 'T -> 'T

    and MapStateToProps =
        [<Emit("$0($1...)")>] abstract Invoke: state: obj * ?ownProps: obj -> obj

    and MapDispatchToPropsFunction =
        [<Emit("$0($1...)")>] abstract Invoke: dispatch: Dispatch * ?ownProps: obj -> obj

    and MapDispatchToPropsObject =
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: name: string -> ActionCreator with get

    and MergeProps =
        [<Emit("$0($1...)")>] abstract Invoke: stateProps: obj * dispatchProps: obj * ownProps: obj -> obj

    and Options =
        abstract ``pure``: bool with get

    and Property<'P when 'P :> Property<'P>> =
        abstract store: Store option with get
        abstract children: React.ReactElement<'P> option with get

    and [<Import("Provider","react-redux")>] Provider<'StateType, 'PropType when 'PropType :> Property<'PropType>>(?props: Property<'PropType>, ?context: obj) =
        inherit Component<Property<'PropType>, 'StateType>(?props = props, ?context = context)


    type [<Import("*","react-redux")>] Globals =
        static member connect(?mapStateToProps: MapStateToProps, ?mapDispatchToProps: U2<MapDispatchToPropsFunction, MapDispatchToPropsObject>, ?mergeProps: MergeProps, ?options: Options): ClassDecorator = failwith "JS only"
