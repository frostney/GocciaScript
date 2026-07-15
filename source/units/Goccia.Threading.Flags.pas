{ Thread-identity flags — the dependency-free sliver of
  Goccia.Threading.

  Value-layer units (Goccia.Values.Primitives, SymbolValue,
  BigIntValue) only ever ask "am I on a worker thread?" before
  touching their shared class-var singletons; importing the full
  thread pool dragged Classes/SyncObjs/CLI reporting into every
  value unit's closure. This unit carries EXACTLY the flag, so those
  units compile on runtimes without TThread machinery (the Lakon
  WASM lane is single-threaded: the flag stays False there, which is
  also the correct answer). }

unit Goccia.Threading.Flags;

{$I Goccia.inc}

interface

threadvar
  { True on worker threads spawned by TGocciaThreadPool
    (Goccia.Threading sets it in each worker's setup). Code that can
    run on either the main thread or a worker checks this flag
    before calling WriteLn, because FPC's standard I/O is not
    thread-safe — concurrent WriteLn calls corrupt the shared Output
    TextRec buffer and cause access violations. }
  GIsWorkerThread: Boolean;

implementation

end.
