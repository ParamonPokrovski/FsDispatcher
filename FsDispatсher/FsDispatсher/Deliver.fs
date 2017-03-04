module FsDispatcher.Deliver
        
open FsDispatcher.Prelude

type Mode =
    | SyncHead = 0         
    | SyncTail = 1
    | Async = 2
    | Queue = 3

type Container<'a> = Map<Mode, ('a -> unit) list>

let private noQueueModes = [Mode.SyncHead;Mode.SyncTail;Mode.Async]
let private queueModes = [Mode.Queue]

let isQueueMode m = queueModes |> List.exists ((=) m)
let isNoQueueMode m = not(isQueueMode m)

module Container =
    let private create<'a> seq : Container<'a> = 
        seq
        |> Seq.map (fun x -> x, []) 
        |> Map.ofSeq

    let createNoQueue<'a> = 
        noQueueModes
        |> create<'a>

    let createQueue<'a> = 
        queueModes
        |> create<'a>

    let register<'a> deliver (func: 'a -> unit) (container : Container<'a>) =    
        let current = if container.ContainsKey(deliver) 
                        then container.[deliver]
                        else []
        container
        |> Map.change deliver (func :: current)            

