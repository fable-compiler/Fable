// Source: http://fssnip.net/dK/title/Monadic-Trampoline
module MonadicTrampoline

type TrampolineValue<'T> =
    | DelayValue of Delay<'T>
    | ReturnValue of Return<'T>
    | BindValue of IBind<'T>

and ITrampoline<'T> = 
    abstract member Value : TrampolineValue<'T>
    abstract member Run : unit -> 'T

and Delay<'T>(f : unit -> ITrampoline<'T>) =
    member self.Func = f 
    interface ITrampoline<'T> with
        member self.Value = DelayValue self
        member self.Run () = (f ()).Run()

and Return<'T>(x :'T) = 
    member self.Value = x
    interface ITrampoline<'T> with
        member self.Value = ReturnValue self
        member self.Run () = x

and IBind<'T> = 
    abstract Bind<'R> : ('T -> ITrampoline<'R>) -> ITrampoline<'R>

and Bind<'T, 'R>(trampoline : ITrampoline<'T>, f : ('T -> ITrampoline<'R>)) = 
    interface IBind<'R> with
        member self.Bind<'K>(f' : 'R -> ITrampoline<'K>) : ITrampoline<'K> =
            new Bind<'T, 'K>(trampoline, fun t -> new Bind<'R, 'K>(f t, (fun r -> f' r)) :> _) :> _
    interface ITrampoline<'R> with
        member self.Value = BindValue self
        member self.Run () = 
            match trampoline.Value with
            | BindValue b -> b.Bind(f).Run() 
            | ReturnValue r -> (f r.Value).Run()
            | DelayValue d -> (new Bind<'T, 'R>(d.Func (), f) :> ITrampoline<'R>).Run() 

type TrampolineBuilder() = 
    member self.Bind(tr, f) = new Bind<'T, 'R>(tr, f) :> ITrampoline<'R>
    member self.Delay f = new Delay<_>(f) :> ITrampoline<_>
    member self.Return a = new Return<_>(a) :> ITrampoline<_>
    member self.ReturnFrom a = a

let trampoline = new TrampolineBuilder()

let run (tr : ITrampoline<'T>) = tr.Run()

let rec private trampolineListMapAcc acc f xs =
    trampoline {
        match xs with
        | [] -> return List.rev acc
        | h::t ->
            let! x = f h
            return! trampolineListMapAcc (x::acc) f t
    }
let trampolineListMap (f: 'a -> ITrampoline<'b>) xs =
    trampolineListMapAcc [] f xs

let trampolineOptionMap (f: 'a -> ITrampoline<'b>) opt =
    trampoline {
        match opt with
        | Some e ->
            let! x = f e
            return Some x
        | None -> return None
    }
