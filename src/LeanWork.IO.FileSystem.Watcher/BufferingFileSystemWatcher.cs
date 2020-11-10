using LeanWork.IO.FileSystem.Watcher.LeanWork.IO.FileSystem;
using System;
using System.Collections.ObjectModel;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace LeanWork.IO.FileSystem
{
    /// <devdoc>
    /// Features:
    /// - Buffers FileSystemWatcher events in a BlockinCollection to prevent InternalBufferOverflowExceptions.
    /// - Does not break the original FileSystemWatcher API.
    /// - Supports reporting existing files via a new Existed event.
    /// - Supports sorting events by oldest (existing) file first.
    /// - Supports an new event Any reporting any FSW change.
    /// - Offers the Error event in Win Forms designer (via [Browsable[true)]
    /// - Does not prevent duplicate files occuring.
    /// Notes:
    ///   We contain FilSystemWatcher to follow the prinicple composition over inheritance
    ///   and because System.IO.FileSystemWatcher is not designed to be inherited from:
    ///   Event handlers and Dispose(disposing) are not virtual.
    /// </devdoc>
    public class BufferingFileSystemWatcher : Component
    {
        private FileSystemWatcher _containedFSW = null;

        private FileSystemEventHandler _onExistedHandler = null;
        private FileSystemEventHandler _onAllChangesHandler = null;

        private FileSystemEventHandler _onCreatedHandler = null;
        private FileSystemEventHandler _onChangedHandler = null;
        private FileSystemEventHandler _onDeletedHandler = null;
        private RenamedEventHandler _onRenamedHandler = null;

        private ErrorEventHandler _onErrorHandler = null;

        //We use a single buffer for all change types. Alternatively we could use one buffer per event type, costing additional enumerate tasks.
        private BlockingCollection<FileSystemEventArgs> _fileSystemEventBuffer = null;
        private CancellationTokenSource _cancellationTokenSource = null;

        #region Contained FileSystemWatcher
        public BufferingFileSystemWatcher()
        {
            _containedFSW = new FileSystemWatcher();
        }

        public BufferingFileSystemWatcher(string path)
        {
            _containedFSW = new FileSystemWatcher(path, "*.*");
        }

        public BufferingFileSystemWatcher(string path, string filter)
        {
            _containedFSW = new FileSystemWatcher(path, filter);
        }

        public bool EnableRaisingEvents
        {
            get
            {
                return _containedFSW.EnableRaisingEvents;
            }
            set
            {
                if (_containedFSW.EnableRaisingEvents == value) return;

                StopRaisingBufferedEvents();
                _cancellationTokenSource = new CancellationTokenSource();

                //We EnableRaisingEvents, before NotifyExistingFiles
                //  to prevent missing any events
                //  accepting more duplicates (which may occure anyway).
                _containedFSW.EnableRaisingEvents = value;
                if (value)
                    RaiseBufferedEventsUntilCancelled();
            }
        }

        public string Filter
        {
            get { return _containedFSW.Filter; }
            set { _containedFSW.Filter = value; }
        }

        public Collection<string> Filters
        {
            get { return _containedFSW.Filters; }
        }

        public bool IncludeSubdirectories
        {
            get { return _containedFSW.IncludeSubdirectories; }
            set { _containedFSW.IncludeSubdirectories = value; }
        }

        public int InternalBufferSize
        {
            get { return _containedFSW.InternalBufferSize; }
            set { _containedFSW.InternalBufferSize = value; }
        }

        public NotifyFilters NotifyFilter
        {
            get { return _containedFSW.NotifyFilter; }
            set { _containedFSW.NotifyFilter = value; }
        }

        public string Path
        {
            get { return _containedFSW.Path; }
            set { _containedFSW.Path = value; }
        }

        public ISynchronizeInvoke SynchronizingObject
        {
            get { return _containedFSW.SynchronizingObject; }
            set { _containedFSW.SynchronizingObject = value; }
        }

        public override ISite Site
        {
            get { return _containedFSW.Site; }
            set { _containedFSW.Site = value; }
        }

        #endregion

        [DefaultValue(false)]
        public bool OrderByOldestFirst { get; set; } = false;

        private int _eventQueueSize = int.MaxValue;
        public int EventQueueCapacity
        {
            get { return _eventQueueSize; }
            set { _eventQueueSize = value; }
        }

        #region New BufferingFileSystemWatcher specific events
        public event FileSystemEventHandler Existed
        {
            add
            {
                _onExistedHandler += value;
            }
            remove
            {
                _onExistedHandler -= value;
            }
        }

        public event FileSystemEventHandler All
        {
            add
            {
                if (_onAllChangesHandler == null)
                {
                    _containedFSW.Created += BufferEvent;
                    _containedFSW.Changed += BufferEvent;
                    _containedFSW.Renamed += BufferEvent;
                    _containedFSW.Deleted += BufferEvent;
                }
                _onAllChangesHandler += value;
            }
            remove
            {
                _containedFSW.Created -= BufferEvent;
                _containedFSW.Changed -= BufferEvent;
                _containedFSW.Renamed -= BufferEvent;
                _containedFSW.Deleted -= BufferEvent;
                _onAllChangesHandler -= value;
            }
        }

        #endregion

        #region Standard FSW events
        //- The _fsw events add to the buffer.
        //- The public events raise from the buffer to the consumer.
        public event FileSystemEventHandler Created
        {
            add
            {
                if (_onCreatedHandler == null)
                    _containedFSW.Created += BufferEvent;
                _onCreatedHandler += value;
            }
            remove
            {
                _containedFSW.Created -= BufferEvent;
                _onCreatedHandler -= value;
            }
        }

        public event FileSystemEventHandler Changed
        {
            add
            {
                if (_onChangedHandler == null)
                    _containedFSW.Changed += BufferEvent;
                _onChangedHandler += value;
            }
            remove
            {
                _containedFSW.Changed -= BufferEvent;
                _onChangedHandler -= value;
            }
        }

        public event FileSystemEventHandler Deleted
        {
            add
            {
                if (_onDeletedHandler == null)
                    _containedFSW.Deleted += BufferEvent;
                _onDeletedHandler += value;
            }
            remove
            {
                _containedFSW.Deleted -= BufferEvent;
                _onDeletedHandler -= value;
            }
        }

        public event RenamedEventHandler Renamed
        {
            add
            {
                if (_onRenamedHandler == null)
                    _containedFSW.Renamed += BufferEvent;
                _onRenamedHandler += value;
            }
            remove
            {
                _containedFSW.Renamed -= BufferEvent;
                _onRenamedHandler -= value;
            }
        }

        private void BufferEvent(object _, FileSystemEventArgs e)
        {
            if (!_fileSystemEventBuffer.TryAdd(e))
            {
                var ex = new EventQueueOverflowException($"Event queue size {_fileSystemEventBuffer.BoundedCapacity} events exceeded.");
                InvokeHandler(_onErrorHandler, new ErrorEventArgs(ex));
            }
        }

        private void StopRaisingBufferedEvents(object _ = null, EventArgs __ = null)
        {
            _cancellationTokenSource?.Cancel();
            _fileSystemEventBuffer = new BlockingCollection<FileSystemEventArgs>(_eventQueueSize);
        }

        public event ErrorEventHandler Error
        {
            add
            {
                if (_onErrorHandler == null)
                    _containedFSW.Error += BufferingFileSystemWatcher_Error;
                _onErrorHandler += value;
            }
            remove
            {
                if (_onErrorHandler == null)
                    _containedFSW.Error -= BufferingFileSystemWatcher_Error;
                _onErrorHandler -= value;
            }
        }

        private void BufferingFileSystemWatcher_Error(object sender, ErrorEventArgs e)
        {
            InvokeHandler(_onErrorHandler, e);
        }
        #endregion

        private void RaiseBufferedEventsUntilCancelled()
        {
            Task.Run(() =>
            {
                try
                {
                    if (_onExistedHandler != null || _onAllChangesHandler != null)
                        NotifyExistingFiles();

                    foreach (FileSystemEventArgs e in _fileSystemEventBuffer.GetConsumingEnumerable(_cancellationTokenSource.Token))
                    {
                        if (_onAllChangesHandler != null)
                            InvokeHandler(_onAllChangesHandler, e);
                        else
                        {
                            switch (e.ChangeType)
                            {
                                case WatcherChangeTypes.Created:
                                    InvokeHandler(_onCreatedHandler, e);
                                    break;
                                case WatcherChangeTypes.Changed:
                                    InvokeHandler(_onChangedHandler, e);
                                    break;
                                case WatcherChangeTypes.Deleted:
                                    InvokeHandler(_onDeletedHandler, e);
                                    break;
                                case WatcherChangeTypes.Renamed:
                                    InvokeHandler(_onRenamedHandler, e as RenamedEventArgs);
                                    break;
                            }
                        }
                    }
                }
                catch (OperationCanceledException)
                { } //ignore
                catch (Exception ex)
                {
                    BufferingFileSystemWatcher_Error(this, new ErrorEventArgs(ex));
                }
            });
        }

        private void NotifyExistingFiles()
        {
            //BufferingFileSystemWatcher_Error(this, new ErrorEventArgs(new Exception("test exception")));

            var searchSubDirectoriesOption = (IncludeSubdirectories) ? SearchOption.AllDirectories : SearchOption.TopDirectoryOnly;
            if (OrderByOldestFirst)
            {
                var sortedFileInfos = from fi in new DirectoryInfo(Path).GetFiles(Filter, searchSubDirectoriesOption)
                                      orderby fi.LastWriteTime ascending
                                      select fi;
                foreach (var fi in sortedFileInfos)
                {
                    InvokeHandler(_onExistedHandler, new FileSystemEventArgs(WatcherChangeTypes.All, fi.DirectoryName, fi.Name));
                    InvokeHandler(_onAllChangesHandler, new FileSystemEventArgs(WatcherChangeTypes.All,fi.DirectoryName, fi.Name));
                }
            }
            else
            {
                foreach (var fsi in new DirectoryInfo(Path).EnumerateFileSystemInfos(Filter, searchSubDirectoriesOption))
                {
                    InvokeHandler(_onExistedHandler, new FileSystemEventArgs(WatcherChangeTypes.All, System.IO.Path.GetDirectoryName(fsi.FullName), fsi.Name ));
                    InvokeHandler(_onAllChangesHandler, new FileSystemEventArgs(WatcherChangeTypes.All, System.IO.Path.GetDirectoryName(fsi.FullName), fsi.Name));
                }
            }
        }

        #region InvokeHandlers
        //Automatically raise event in calling thread when _fsw.SynchronizingObject is set. Ex: When used as a component in Win Forms.
        //TODO: remove redundancy. I don't understand how to cast the specific *EventHandler to a generic Delegate, EventHandler, Action or whatever.
        private void InvokeHandler(FileSystemEventHandler eventHandler, FileSystemEventArgs e)
        {
            if (eventHandler != null)
            {
                if (_containedFSW.SynchronizingObject != null && this._containedFSW.SynchronizingObject.InvokeRequired)
                    _containedFSW.SynchronizingObject.BeginInvoke(eventHandler, new object[] { this, e });
                else
                    eventHandler(this, e);
            }
        }
        private void InvokeHandler(RenamedEventHandler eventHandler, RenamedEventArgs e)
        {
            if (eventHandler != null)
            {
                if (_containedFSW.SynchronizingObject != null && this._containedFSW.SynchronizingObject.InvokeRequired)
                    _containedFSW.SynchronizingObject.BeginInvoke(eventHandler, new object[] { this, e });
                else
                    eventHandler(this, e);
            }
        }
        private void InvokeHandler(ErrorEventHandler eventHandler, ErrorEventArgs e)
        {
            if (eventHandler != null)
            {
                if (_containedFSW.SynchronizingObject != null && this._containedFSW.SynchronizingObject.InvokeRequired)
                    _containedFSW.SynchronizingObject.BeginInvoke(eventHandler, new object[] { this, e });
                else
                    eventHandler(this, e);
            }
        }
        #endregion

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                _cancellationTokenSource?.Cancel();
                _containedFSW?.Dispose();

                //_onExistedHandler = null;
                //_onAllChangesHandler = null;
                //_onCreatedHandler = null;
                //_onChangedHandler = null;
                //_onDeletedHandler = null;
                //_onRenamedHandler = null;
                //_onErrorHandler = null;
            }
            base.Dispose(disposing);
        }
    }
}
