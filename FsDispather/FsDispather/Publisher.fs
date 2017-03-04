module FsDispatcher.Publisher
        
open FsDispatcher.Prelude
open FsDispatcher.Deliver
open FsDispatcher.Queue
open FsDispatcher.Dispatcher

let private sync<'a> message = broadcast<'a> (fun f -> f message)
let private async<'a> message = broadcast<'a> (fun f -> async{ f message} |> Async.Start)

let start<'a> dispatcher (message : 'a) = 
    let om = box message
    dispatcher
    |> sync om Deliver.Mode.SyncHead
    |> async om Deliver.Mode.Async
    |> enqueue om
    |> sync om Deliver.Mode.SyncTail
    |> ignore