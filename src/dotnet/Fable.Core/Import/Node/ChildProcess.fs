module rec Fable.Import.Node.ChildProcess

open Fable.Core
open Fable.Import.Node.Events
open Fable.Import.Node.Stream
open Fable.Import.Node.Buffer
open Fable.Import.Node.Base

type ExecError = 
    inherit NodeJS.Error
    abstract code: int with get, set
    abstract signal: string option with get, set

type [<AllowNullLiteral>] ChildProcess =
    inherit event_types.EventEmitter
    abstract stdin: stream_types.Writable<string> with get, set
    abstract stdout: stream_types.Readable<string> with get, set
    abstract stderr: stream_types.Readable<string> with get, set
    abstract connected: bool with get, set
    abstract stdio: U2<stream_types.Readable<string>,stream_types.Writable<string>>[] with get, set
    abstract pid: float with get, set
    abstract kill: ?signal: string -> unit
    abstract send: message: obj * ?sendHandle: obj -> unit
    abstract disconnect: unit -> unit
    abstract unref: unit -> unit

type [<AllowNullLiteral>] ChildProcessStatic =
    [<Emit("new $0()")>] abstract Create: unit -> ChildProcess

type ExecOptions = {
    encoding : string option;
}

type IExports =
    abstract ChildProcess: ChildProcessStatic with get, set
    abstract spawn: command: string * ?args: ResizeArray<string> * ?options: obj -> ChildProcess
    abstract exec: command: string * ?options: ExecOptions * ?callback:(ExecError option -> U2<string, buffer_types.Buffer> -> U2<string, buffer_types.Buffer> -> unit) -> ChildProcessStatic

    abstract execFile: file: string * ?callback: (ExecError option -> buffer_types.Buffer -> buffer_types.Buffer -> unit) -> ChildProcess
    
    abstract execFile: file: string * ?args: ResizeArray<string> * ?callback: (ExecError option -> buffer_types.Buffer -> buffer_types.Buffer -> unit) -> ChildProcess
    
    abstract execFile :file: string * ?args: ResizeArray<string> * ?options: ExecOptions * ?callback: (ExecError option -> buffer_types.Buffer -> buffer_types.Buffer -> unit) -> ChildProcess
    
    abstract fork: modulePath: string * ?args: ResizeArray<string> * ?options: obj -> ChildProcess
    
    abstract execSync: command: string * ?options: obj -> U2<string, buffer_types.Buffer>
    
    abstract execFileSync: command: string * ?args: ResizeArray<string> * ?options: obj -> U2<string, buffer_types.Buffer>
    
    abstract spawnSync: command: string * ?args: ResizeArray<string> * ?options: obj -> obj