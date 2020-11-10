using System;
using System.ComponentModel;

public class FileWatcherErrorEventArgs : HandledEventArgs
{
    public readonly Exception Error;
    public FileWatcherErrorEventArgs(Exception exception)
    {
        this.Error = exception;
    }
}

