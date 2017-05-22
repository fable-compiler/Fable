module rec Fable.Import.Node.Path

open System
open Fable.Core

type PathObjectProps = 
    abstract dir : string with get, set
    abstract root : string with get, set
    [<Emit("$0.base{{=$1}}")>] // using this because back ticks are way too ugly to read in code
    abstract baseName : string with get, set
    abstract name : string with get, set
    abstract ext : string with get, set

type IExports = 
    /// Returns the last portion of 'path' argument. Similar to the "basename" command in UNIX.Trailing directory separators are ignored
    abstract basename : path:string -> string 
    /// Returns the last portion of 'path' argument. Similar to the "basename" command in UNIX.Trailing directory separators are ignored
    abstract basename : path:string * ext:string -> string
    /// Provides the platform-specific path delimiter. ";" for Windows. ":"
    abstract delimiter : string
    /// Returns the directory name of a 'path'
    abstract dirname : path:string -> string
    /// The path.extname() method returns the extension of the path, from the last occurrence of the . (period) character to end of string in the last portion of the path. If there is no . in the last portion of the path, or if the first character of the basename of path is ., then an empty string is returned.
    abstract extname : path:string -> string
    /// Returns a path string from an object, this is the oppisite of path.parse
    abstract format : PathObjectProps -> string
    /// Determines if path is an absolute path.
    abstract isAbsolute : path:string -> bool 
    /// Joins all given path segments together using the platform specific separator as a delimiter, then normalizes the resulting path. Zero-length path segments are ignored. If the joined path string is a zero-length string then '.' will be returned, representing the current working directory.  
    abstract join : [<ParamArray>] paths : string [] -> string
    /// Normalizes the given path, resolving '..' and '.' segments.
    abstract normalize : string -> string
    /// Returns an object whose properties represent significant elements of the path. Trailing directory separators are ignored.
    abstract parse : string -> PathObjectProps
    /// returns the relative path from from to to. If from and to each resolve to the same path (after calling path.resolve() on each), a zero-length string is returned.
    abstract relative : string * string -> string
    /// resolves a sequence of paths or path segments into an absolute path.
    abstract resolve : [<ParamArray>] paths : string [] -> string
    /// Provides the platform-specific path segment separator. "\" On Windows, "/" on POSIX
    abstract sep : string
    /// Provides access to Windows-specific implementations of the path methods.
    abstract win32 : obj
    /// Provides access to POSIX specific implementations of the path methods.
    abstract posix : obj