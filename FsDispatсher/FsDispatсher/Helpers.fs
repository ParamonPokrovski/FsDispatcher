namespace FsDispatcher.Helpers

open FsDispatcher.Prelude
open FsDispatcher.Deliver
open FsDispatcher.Dispatcher
open FsDispatcher.Publisher

module Register =
    let async<'a> = register<'a> Mode.Async
    let syncHead<'a> = register<'a> Mode.SyncHead
    let syncTail<'a> = register<'a> Mode.SyncTail
    let queue<'a> = register<'a> Mode.Queue

module Send =                
    let sync<'a> = start<'a>
            
    let async<'a> dispatcher message =
        async { sync<'a> dispatcher message }