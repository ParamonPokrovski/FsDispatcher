namespace FsDispatcher.Helpers

open FsDispatcher.Prelude
open FsDispatcher.Deliver
open FsDispatcher.Dispatcher
open FsDispatcher.Publisher

module Register =
    let async<'a> = register<'a> (Mode.Basic BasicMode.Async)
    let syncHead<'a> = register<'a> (Mode.Basic (BasicMode.Sync SyncMode.Head))
    let syncTail<'a> = register<'a> (Mode.Basic (BasicMode.Sync SyncMode.Tail))
    let queue<'a> = register<'a> (Mode.Queue QueueMode.Sync)
    let dedicatedQueue<'a> id = register<'a> (Mode.DedicatedQueue (id,QueueMode.Sync))

module Send =                
    let sync<'a> = start<'a>
            
    let async<'a> dispatcher message =
        async { sync<'a> dispatcher message }