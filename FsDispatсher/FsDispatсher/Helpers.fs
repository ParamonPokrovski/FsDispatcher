namespace FsDispatcher.Helpers

open FsDispatcher.Prelude
open FsDispatcher.Deliver
open FsDispatcher.Dispatcher
open FsDispatcher.Parallel

module Register =
    let async<'a> = register<'a> (BasicMode.Async |> Mode.Basic)
    let sync<'a> = register<'a> (SyncMode.Tail |> BasicMode.Sync |> Mode.Basic )

    module Queue =
        let each<'a> = register<'a> (BasicMode.Async |> QueueMode.Each|> Mode.Queue)
        let last<'a> = register<'a> (BasicMode.Async |> QueueMode.Last|> Mode.Queue)

        module Dedicated =
            let each<'a,'b> (id : 'b) = register<'a> (Mode.DedicatedQueue (id, BasicMode.Async |> QueueMode.Each ))
            let eachParallel<'a,'b> (id : 'b) parallelism func = each<'a,'b> id (func |> Func.mapParallel parallelism)
            let last<'a,'b> (id : 'b) = register<'a> (Mode.DedicatedQueue (id, BasicMode.Async |> QueueMode.Last ))

        module Exclusive =
            let each<'a> = Dedicated.each Guid.create
            let eachParallel<'a> = Dedicated.eachParallel Guid.create

module Send =                
    let sync<'a> = Deliver.start<'a>
            
    let async<'a> dispatcher message =
        async { sync<'a> dispatcher message }