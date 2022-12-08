module Timer

open Fable.Core

/// This class represents an action that should be run only after a
/// certain amount of time has passed — a timer. Timer is a subclass of
/// Thread and as such also functions as an example of creating custom
/// threads.
type ITimer =
    abstract daemon : bool with get, set

    /// Start the thread’s activity.
    abstract start : unit -> unit
    /// Stop the timer, and cancel the execution of the timer’s action.
    /// This will only work if the timer is still in its waiting stage.
    abstract cancel : unit -> unit

    /// Create a timer that will run function with arguments args and
    /// keyword arguments kwargs, after interval seconds have passed. If
    /// args is None (the default) then an empty list will be used. If
    /// kwargs is None (the default) then an empty dict will be used.
    [<Emit("$0($1, $2)")>]
    abstract Create : float * (unit -> unit) -> ITimer

[<Import("Timer", "threading")>]
let Timer : ITimer = nativeOnly
