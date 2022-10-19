module AsyncBuilder

open System
open System.Collections.Generic
open Fable.Core
open Timer

type Continuation<'T> = 'T -> unit

type OperationCanceledError () =
    inherit Exception ("The operation was canceled")

type Continuations<'T> = Continuation<'T> * Continuation<exn> * Continuation<OperationCanceledError>


type CancellationToken (cancelled: bool) =
    let mutable idx = 0
    let mutable cancelled = cancelled
    let listeners = Dictionary<int, unit -> unit>()

    new () = CancellationToken(false)

    member this.IsCancelled = cancelled

    member this.Cancel() =
        if not cancelled then cancelled <- true

        for KeyValue (_, listener) in listeners do
            listener ()

    member this.AddListener(f: unit -> unit) =
        let id = idx
        idx <- idx + 1
        listeners.Add(idx, f)
        id

    member this.RemoveListener(id: int) = listeners.Remove(id)

    member this.Register(f: unit -> unit) : IDisposable =
        let id = this.AddListener(f)

        { new IDisposable with
            member x.Dispose() = this.RemoveListener(id) |> ignore }

    member this.Register(f: obj -> unit, state: obj) : IDisposable =
        let id = this.AddListener(fun () -> f (state))

        { new IDisposable with
            member x.Dispose() = this.RemoveListener(id) |> ignore }

type Trampoline () =
    let mutable callCount = 0

    static member MaxTrampolineCallCount = 2000

    member this.IncrementAndCheck() =
        callCount <- callCount + 1
        callCount > Trampoline.MaxTrampolineCallCount

    member this.Hijack(f: unit -> unit) =
        callCount <- 0
        let timer = Timer.Create(0., f)
        timer.daemon <- true
        timer.start ()

type IAsyncContext<'T> =
    abstract member onSuccess : Continuation<'T>
    abstract member onError : Continuation<exn>
    abstract member onCancel : Continuation<OperationCanceledError>

    abstract member cancelToken : CancellationToken
    abstract member trampoline : Trampoline

type IAsync<'T> = IAsyncContext<'T> -> unit

let protectedCont<'T> (f: IAsync<'T>) =
    fun (ctx: IAsyncContext<'T>) ->
        if ctx.cancelToken.IsCancelled then
            ctx.onCancel (new OperationCanceledError())
        else if (ctx.trampoline.IncrementAndCheck()) then
            ctx.trampoline.Hijack
                (fun () ->
                    try
                        f ctx
                    with err -> ctx.onError (err))
        else
            try
                f ctx
            with err -> ctx.onError (err)

let protectedBind<'T, 'U> (computation: IAsync<'T>, binder: 'T -> IAsync<'U>) =
    protectedCont
        (fun (ctx: IAsyncContext<'U>) ->
            computation (
                { new IAsyncContext<'T> with
                    member this.onSuccess =
                        fun (x: 'T) ->
                            try
                                binder (x) (ctx)
                            with ex -> ctx.onError (ex)

                    member this.onError = ctx.onError
                    member this.onCancel = ctx.onCancel
                    member this.cancelToken = ctx.cancelToken
                    member this.trampoline = ctx.trampoline }
            ))

let protectedReturn<'T> (value: 'T) = protectedCont (fun (ctx: IAsyncContext<'T>) -> ctx.onSuccess (value))

type IAsyncBuilder =
    abstract member Bind<'T, 'U> : IAsync<'T> * ('T -> IAsync<'U>) -> IAsync<'U>

    abstract member Combine<'T> : IAsync<unit> * IAsync<'T> -> IAsync<'T>

    abstract member Delay<'T> : (unit -> IAsync<'T>) -> IAsync<'T>

    //abstract member Return<'T> : [<ParamArray>] values: 'T [] -> IAsync<'T>
    abstract member Return<'T> : value: 'T -> IAsync<'T>

    abstract member While : (unit -> bool) * IAsync<unit> -> IAsync<unit>
    abstract member Zero : unit -> IAsync<unit>


type AsyncBuilder () =
    interface IAsyncBuilder with

        member this.Bind<'T, 'U>(computation: IAsync<'T>, binder: 'T -> IAsync<'U>) = protectedBind (computation, binder)

        member this.Combine<'T>(computation1: IAsync<unit>, computation2: IAsync<'T>) =
            let self = this :> IAsyncBuilder
            self.Bind(computation1, (fun () -> computation2))

        member x.Delay<'T>(generator: unit -> IAsync<'T>) = protectedCont (fun (ctx: IAsyncContext<'T>) -> generator () (ctx))


        //   public For<T>(sequence: Iterable<T>, body: (x: T) => IAsync<void>) {
        //     const iter = sequence[Symbol.iterator]();
        //     let cur = iter.next();
        //     return this.While(() => !cur.done, this.Delay(() => {
        //       const res = body(cur.value);
        //       cur = iter.next();
        //       return res;
        //     }));
        //   }

        member this.Return<'T>(value: 'T) : IAsync<'T> = protectedReturn (unbox value)
        // member this.Return<'T>([<ParamArray>] value: 'T []) : IAsync<'T> =
        //     match value with
        //     | [||] -> protectedReturn (unbox null)
        //     | [| value |] -> protectedReturn value
        //     | _ -> failwith "Return takes zero or one argument."


        //   public ReturnFrom<T>(computation: IAsync<T>) {
//     return computation;
//   }

        //   public TryFinally<T>(computation: IAsync<T>, compensation: () => void) {
//     return protectedCont((ctx: IAsyncContext<T>) => {
//       computation({
//         onSuccess: (x: T) => {
//           compensation();
//           ctx.onSuccess(x);
//         },
//         onError: (x: any) => {
//           compensation();
//           ctx.onError(x);
//         },
//         onCancel: (x: any) => {
//           compensation();
//           ctx.onCancel(x);
//         },
//         cancelToken: ctx.cancelToken,
//         trampoline: ctx.trampoline,
//       });
//     });
//   }

        //   public TryWith<T>(computation: IAsync<T>, catchHandler: (e: any) => IAsync<T>) {
//     return protectedCont((ctx: IAsyncContext<T>) => {
//       computation({
//         onSuccess: ctx.onSuccess,
//         onCancel: ctx.onCancel,
//         cancelToken: ctx.cancelToken,
//         trampoline: ctx.trampoline,
//         onError: (ex: any) => {
//           try {
//             catchHandler(ex)(ctx);
//           } catch (ex2) {
//             ctx.onError(ex2);
//           }
//         },
//       });
//     });
//   }

        //   public Using<T extends IDisposable, U>(resource: T, binder: (x: T) => IAsync<U>) {
//     return this.TryFinally(binder(resource), () => resource.Dispose());
//   }

        member this.While(guard: unit -> bool, computation: IAsync<unit>) : IAsync<unit> =
            let self = this :> IAsyncBuilder

            if guard () then
                self.Bind(computation, (fun () -> self.While(guard, computation)))
            else
                self.Return()

        //    member this.Bind<'T, 'U>(computation: IAsync<'T>, binder: 'T -> IAsync<'U>) = (this :> IAsyncBuilder).Bind(computation, binder)
        member this.Zero() : IAsync<unit> = protectedCont (fun (ctx: IAsyncContext<unit>) -> ctx.onSuccess (()))

// }

let singleton : IAsyncBuilder = AsyncBuilder() :> _
