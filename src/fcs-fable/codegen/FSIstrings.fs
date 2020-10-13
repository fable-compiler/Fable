// This is a generated file; the original input is '/home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt'
namespace FSIstrings
type internal SR private() =
    /// Stopped due to error\n
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:2)
    static member stoppedDueToError() = (sprintf "Stopped due to error\n" )
    /// Usage: %s <options> [script.fsx [<arguments>]]
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:3)
    static member fsiUsage(a0 : System.String) = (sprintf "Usage: %s <options> [script.fsx [<arguments>]]" a0)
    /// - INPUT FILES -
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:4)
    static member fsiInputFiles() = (sprintf "- INPUT FILES -" )
    /// - CODE GENERATION -
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:5)
    static member fsiCodeGeneration() = (sprintf "- CODE GENERATION -" )
    /// - ERRORS AND WARNINGS -
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:6)
    static member fsiErrorsAndWarnings() = (sprintf "- ERRORS AND WARNINGS -" )
    /// - LANGUAGE -
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:7)
    static member fsiLanguage() = (sprintf "- LANGUAGE -" )
    /// - MISCELLANEOUS -
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:8)
    static member fsiMiscellaneous() = (sprintf "- MISCELLANEOUS -" )
    /// - ADVANCED -
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:9)
    static member fsiAdvanced() = (sprintf "- ADVANCED -" )
    /// Exception raised when starting remoting server.\n%s
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:10)
    static member fsiExceptionRaisedStartingServer(a0 : System.String) = (sprintf "Exception raised when starting remoting server.\n%s" a0)
    /// Use the given file on startup as initial input
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:11)
    static member fsiUse() = (sprintf "Use the given file on startup as initial input" )
    /// #load the given file on startup
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:12)
    static member fsiLoad() = (sprintf "#load the given file on startup" )
    /// Treat remaining arguments as command line arguments, accessed using fsi.CommandLineArgs
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:13)
    static member fsiRemaining() = (sprintf "Treat remaining arguments as command line arguments, accessed using fsi.CommandLineArgs" )
    /// Display this usage message (Short form: -?)
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:14)
    static member fsiHelp() = (sprintf "Display this usage message (Short form: -?)" )
    /// Exit fsi after loading the files or running the .fsx script given on the command line
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:15)
    static member fsiExec() = (sprintf "Exit fsi after loading the files or running the .fsx script given on the command line" )
    /// Execute interactions on a Windows Forms event loop (on by default)
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:16)
    static member fsiGui() = (sprintf "Execute interactions on a Windows Forms event loop (on by default)" )
    /// Suppress fsi writing to stdout
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:17)
    static member fsiQuiet() = (sprintf "Suppress fsi writing to stdout" )
    /// Support TAB completion in console (on by default)
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:18)
    static member fsiReadline() = (sprintf "Support TAB completion in console (on by default)" )
    /// Emit debug information in quotations
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:19)
    static member fsiEmitDebugInfoInQuotations() = (sprintf "Emit debug information in quotations" )
    /// For help type #help;;
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:20)
    static member fsiBanner3() = (sprintf "For help type #help;;" )
    /// A problem occurred starting the F# Interactive process. This may be due to a known problem with background process console support for Unicode-enabled applications on some Windows systems. Try selecting Tools->Options->F# Interactive for Visual Studio and enter '--fsi-server-no-unicode'.
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:21)
    static member fsiConsoleProblem() = (sprintf "A problem occurred starting the F# Interactive process. This may be due to a known problem with background process console support for Unicode-enabled applications on some Windows systems. Try selecting Tools->Options->F# Interactive for Visual Studio and enter '--fsi-server-no-unicode'." )
    /// '%s' is not a valid assembly name
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:22)
    static member fsiInvalidAssembly(a0 : System.String) = (2301, sprintf "'%s' is not a valid assembly name" a0)
    /// Directory '%s' doesn't exist
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:23)
    static member fsiDirectoryDoesNotExist(a0 : System.String) = (2302, sprintf "Directory '%s' doesn't exist" a0)
    /// Warning: line too long, ignoring some characters\n
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:24)
    static member fsiLineTooLong() = (sprintf "Warning: line too long, ignoring some characters\n" )
    /// Real: %s, CPU: %s, GC %s
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:25)
    static member fsiTimeInfoMainString(a0 : System.String, a1 : System.String, a2 : System.String) = (sprintf "Real: %s, CPU: %s, GC %s" a0 a1 a2)
    /// gen
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:26)
    static member fsiTimeInfoGCGenerationLabelSomeShorthandForTheWordGeneration() = (sprintf "gen" )
    /// \n\nException raised during pretty printing.\nPlease report this so it can be fixed.\nTrace: %s\n
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:27)
    static member fsiExceptionDuringPrettyPrinting(a0 : System.String) = (sprintf "\n\nException raised during pretty printing.\nPlease report this so it can be fixed.\nTrace: %s\n" a0)
    ///   F# Interactive directives:
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:28)
    static member fsiIntroTextHeader1directives() = (sprintf "  F# Interactive directives:" )
    /// Reference (dynamically load) the given DLL
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:29)
    static member fsiIntroTextHashrInfo() = (sprintf "Reference (dynamically load) the given DLL" )
    /// Add the given search path for referenced DLLs
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:30)
    static member fsiIntroTextHashIInfo() = (sprintf "Add the given search path for referenced DLLs" )
    /// Include package source uri when searching for packages
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:31)
    static member fsiIntroPackageSourceUriInfo() = (sprintf "Include package source uri when searching for packages" )
    /// Load the given file(s) as if compiled and referenced
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:32)
    static member fsiIntroTextHashloadInfo() = (sprintf "Load the given file(s) as if compiled and referenced" )
    /// Toggle timing on/off
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:33)
    static member fsiIntroTextHashtimeInfo() = (sprintf "Toggle timing on/off" )
    /// Display help
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:34)
    static member fsiIntroTextHashhelpInfo() = (sprintf "Display help" )
    /// Exit
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:35)
    static member fsiIntroTextHashquitInfo() = (sprintf "Exit" )
    ///   F# Interactive command line options:
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:36)
    static member fsiIntroTextHeader2commandLine() = (sprintf "  F# Interactive command line options:" )
    ///       See '%s' for options
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:37)
    static member fsiIntroTextHeader3(a0 : System.String) = (sprintf "      See '%s' for options" a0)
    /// Loading
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:38)
    static member fsiLoadingFilesPrefixText() = (sprintf "Loading" )
    /// \n- Interrupt\n
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:39)
    static member fsiInterrupt() = (sprintf "\n- Interrupt\n" )
    /// \n- Exit...\n
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:40)
    static member fsiExit() = (sprintf "\n- Exit...\n" )
    /// - Aborting main thread...
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:41)
    static member fsiAbortingMainThread() = (sprintf "- Aborting main thread..." )
    /// Failed to install ctrl-c handler - Ctrl-C handling will not be available. Error was:\n\t%s
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:42)
    static member fsiCouldNotInstallCtrlCHandler(a0 : System.String) = (sprintf "Failed to install ctrl-c handler - Ctrl-C handling will not be available. Error was:\n\t%s" a0)
    /// --> Referenced '%s'
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:43)
    static member fsiDidAHashr(a0 : System.String) = (sprintf "--> Referenced '%s'" a0)
    /// --> Referenced '%s' (file may be locked by F# Interactive process)
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:44)
    static member fsiDidAHashrWithLockWarning(a0 : System.String) = (sprintf "--> Referenced '%s' (file may be locked by F# Interactive process)" a0)
    /// --> Referenced '%s' (an assembly with a different timestamp has already been referenced from this location, reset fsi to load the updated assembly)
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:45)
    static member fsiDidAHashrWithStaleWarning(a0 : System.String) = (sprintf "--> Referenced '%s' (an assembly with a different timestamp has already been referenced from this location, reset fsi to load the updated assembly)" a0)
    /// --> Added '%s' to library include path
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:46)
    static member fsiDidAHashI(a0 : System.String) = (sprintf "--> Added '%s' to library include path" a0)
    /// --> Timing now on
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:47)
    static member fsiTurnedTimingOn() = (sprintf "--> Timing now on" )
    /// --> Timing now off
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:48)
    static member fsiTurnedTimingOff() = (sprintf "--> Timing now off" )
    /// - Unexpected ThreadAbortException (Ctrl-C) during event handling: Trying to restart...
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:49)
    static member fsiUnexpectedThreadAbortException() = (sprintf "- Unexpected ThreadAbortException (Ctrl-C) during event handling: Trying to restart..." )
    /// Failed to resolve assembly '%s'
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:50)
    static member fsiFailedToResolveAssembly(a0 : System.String) = (sprintf "Failed to resolve assembly '%s'" a0)
    /// Binding session to '%s'...
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:51)
    static member fsiBindingSessionTo(a0 : System.String) = (sprintf "Binding session to '%s'..." a0)
    /// Microsoft (R) F# Interactive version %s
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:52)
    static member fsiProductName(a0 : System.String) = (sprintf "Microsoft (R) F# Interactive version %s" a0)
    /// F# Interactive for F# %s
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:53)
    static member fsiProductNameCommunity(a0 : System.String) = (sprintf "F# Interactive for F# %s" a0)
    /// Prevents references from being locked by the F# Interactive process
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:54)
    static member shadowCopyReferences() = (sprintf "Prevents references from being locked by the F# Interactive process" )
    /// Operation could not be completed due to earlier error
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:55)
    static member fsiOperationCouldNotBeCompleted() = (sprintf "Operation could not be completed due to earlier error" )
    /// Operation failed. The error text has been printed in the error stream. To return the corresponding FSharpErrorInfo use the EvalInteractionNonThrowing, EvalScriptNonThrowing or EvalExpressionNonThrowing
    /// (Originally from /home/dev/Projects/fsharp/src/fsharp/fsi/FSIstrings.txt:56)
    static member fsiOperationFailed() = (sprintf "Operation failed. The error text has been printed in the error stream. To return the corresponding FSharpErrorInfo use the EvalInteractionNonThrowing, EvalScriptNonThrowing or EvalExpressionNonThrowing" )
