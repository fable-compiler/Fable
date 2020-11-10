using LeanWork.IO.FileSystem.Watcher.LeanWork.IO.FileSystem;
using System;
using System.ComponentModel;
using System.IO;
using System.Threading;

namespace LeanWork.IO.FileSystem
{
    public class RecoveringFileSystemWatcher : BufferingFileSystemWatcher
    {
        public TimeSpan DirectoryMonitorInterval = TimeSpan.FromMinutes(5);
        public TimeSpan DirectoryRetryInterval = TimeSpan.FromSeconds(5);

        private System.Threading.Timer _monitorTimer = null;


        public RecoveringFileSystemWatcher()
            : base()
        { }

        public RecoveringFileSystemWatcher(string path)
            : base(path, "*.*")
        { }

        public RecoveringFileSystemWatcher(string path, string filter)
            : base(path, filter)
        { }

        //To allow consumer to cancel default error handling
        private EventHandler<FileWatcherErrorEventArgs> _onErrorHandler = null;
        public new event EventHandler<FileWatcherErrorEventArgs> Error
        {
            add
            {
                _onErrorHandler += value;
            }
            remove
            {
                _onErrorHandler -= value;
            }
        }

        public new bool EnableRaisingEvents
        {
            get { return base.EnableRaisingEvents; }
            set
            {
                //Static _firstCall = True
                //If _firstCall And value Then
                //    _firstCall = False
                //    File.Create(System.IO.Path.Combine(Path, "~~monitored by RecoveringFileSystemWatcher~~"), 1024, FileOptions.DeleteOnClose)
                //End If

                if (value == EnableRaisingEvents)
                    return;

                base.EnableRaisingEvents = value;
                if (EnableRaisingEvents)
                {
                    base.Error += BufferingFileSystemWatcher_Error;
                    Start();
                }
                else
                {
                    base.Error -= BufferingFileSystemWatcher_Error;
                }
            }
        }

        private void Start()
        {

            try
            {
                _monitorTimer = new System.Threading.Timer(_monitorTimer_Elapsed);

                Disposed += (_, __) =>
                {
                    _monitorTimer.Dispose();
                };

                ReStartIfNeccessary(TimeSpan.Zero);
            }
            catch (Exception ex)
            {
                throw ex;
            }
        }

        private void _monitorTimer_Elapsed(object state)
        {

            try
            {
                if (!Directory.Exists(Path))
                {
                    throw new DirectoryNotFoundException($"Directory not found {Path}");
                }
                else
                {
                    if (!EnableRaisingEvents)
                    {
                        EnableRaisingEvents = true;
                    }

                    ReStartIfNeccessary(DirectoryMonitorInterval);
                }
            }
            catch (Exception ex) when (ex is FileNotFoundException || ex is DirectoryNotFoundException)
            {
                //Handles race condition too: Path loses accessiblity between .Exists() and .EnableRaisingEvents
                if (ExceptionWasHandledByCaller(ex))
                    return;

                EnableRaisingEvents = false;
                ReStartIfNeccessary(DirectoryRetryInterval);
            }
            catch (Exception ex)
            {
                throw ex;
            }
        }

        private void ReStartIfNeccessary(TimeSpan delay)
        {
            try
            {
                _monitorTimer.Change(delay, Timeout.InfiniteTimeSpan);
            }
            catch (ObjectDisposedException)
            { } //ignore timer disposed
        }

       private void BufferingFileSystemWatcher_Error(object sender, ErrorEventArgs e)
        {
            //These exceptions have the same HResult
            var NetworkNameNoLongerAvailable = -2147467259; //occurs on network outage
            var AccessIsDenied = -2147467259; //occurs after directory was deleted


            var ex = e.GetException();
            if (ExceptionWasHandledByCaller(e.GetException()))
                return;

            //The base FSW does set .EnableRaisingEvents=False AFTER raising OnError()
            EnableRaisingEvents = false;

            if (ex is InternalBufferOverflowException || ex is EventQueueOverflowException)
            {
                ReStartIfNeccessary(DirectoryRetryInterval);
            }
            else if (ex is Win32Exception && (ex.HResult == NetworkNameNoLongerAvailable | ex.HResult == AccessIsDenied))
            {
                ReStartIfNeccessary(DirectoryRetryInterval);
            }
            else
            {
                throw ex;
            }
        }

        //Function GetMaxInternalBuffersize() As Integer
        //    'NOTE: Only increase FSW InternalBuffersize after evaluation other options:
        //    '  http://msdn.microsoft.com/en-us/library/ded0dc5s.aspx
        //    '  http://msdn.microsoft.com/en-us/library/aa366778(VS.85).aspx
        //    Dim maxInternalBufferSize64BitOS = ByteSize.ByteSize.FromKiloBytes(16 * 4)
        //    Dim maxInternalBufferSize32BitOS = ByteSize.ByteSize.FromKiloBytes(2 * 4)
        //    If Environment.Is64BitOperatingSystem Then
        //        Return maxInternalBufferSize64BitOS.Bytes
        //    Else
        //        Return maxInternalBufferSize32BitOS.Bytes
        //    End If
        //End Function

        private bool ExceptionWasHandledByCaller(Exception ex)
        {
            //Allow consumer to handle error
            if (_onErrorHandler != null)
            {
                FileWatcherErrorEventArgs e = new FileWatcherErrorEventArgs(ex);
                InvokeHandler(_onErrorHandler, e);
                return e.Handled;
            }
            else
            {
                return false;
            }
        }

        private void InvokeHandler(EventHandler<FileWatcherErrorEventArgs> eventHandler, FileWatcherErrorEventArgs e)
        {
            if (eventHandler != null)
            {
                if (SynchronizingObject != null && this.SynchronizingObject.InvokeRequired)
                    SynchronizingObject.BeginInvoke(eventHandler, new object[] { this, e });
                else
                    eventHandler(this, e);
            }
        }
    }
}
