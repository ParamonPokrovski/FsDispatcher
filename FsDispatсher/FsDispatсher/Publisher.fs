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
    |> sync om (key (BasicMode.Sync SyncMode.Head))
    |> async om (key BasicMode.Async)
    |> enqueue om
    |> sync om (key (BasicMode.Sync SyncMode.Tail))
    |> ignore