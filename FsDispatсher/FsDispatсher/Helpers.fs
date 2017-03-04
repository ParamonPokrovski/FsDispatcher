namespace FsDispatcher.Helpers

open FsDispatcher.Prelude
open FsDispatcher.Deliver
open FsDispatcher.Dispatcher

module Register =
    let async<'a> = register<'a> (BasicMode.Async |> Mode.Basic)
    let sync<'a> = register<'a> (SyncMode.Tail |> BasicMode.Sync |> Mode.Basic )
    let queue<'a> = register<'a> (BasicMode.Async |> QueueMode.Each|> Mode.Queue)
    let dedicatedQueue<'a> id = register<'a> (Mode.DedicatedQueue (id, BasicMode.Async |> QueueMode.Each ))

module Send =                
    let sync<'a> = Deliver.start<'a>
            
    let async<'a> dispatcher message =
        async { sync<'a> dispatcher message }