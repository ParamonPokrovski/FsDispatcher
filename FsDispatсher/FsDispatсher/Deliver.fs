module FsDispatcher.Deliver
        
open FsDispatcher.Prelude

type SyncMode =
    | Head
    | Tail

type BasicMode =
    | Sync of SyncMode  
    | Async

type QueueMode =
    | Each of BasicMode 
    
type Mode =
    | Basic of BasicMode
    | Queue of QueueMode
    | DedicatedQueue of obj*QueueMode

type ModeKey = int
let key (mode : _) : ModeKey= 
    mode.GetHashCode()

type Container<'a> = Map<ModeKey, ('a -> unit) list>

module Container =
    module Create = 
        let private create<'a> seq : Container<'a> = 
            seq
            |> Seq.map (fun x -> x, []) 
            |> Map.ofSeq

        let basic<'a> =
            [BasicMode.Async; BasicMode.Sync SyncMode.Head; BasicMode.Sync SyncMode.Tail]
            |> Seq.map key
            |> create<'a>

        let queue<'a> =        
            [QueueMode.Each BasicMode.Async; QueueMode.Each (BasicMode.Sync SyncMode.Head); QueueMode.Each (BasicMode.Sync SyncMode.Tail)]
            |> Seq.map key
            |> create<'a>

    let register<'a> mode (func: 'a -> unit) (container : Container<'a>) =    
        let current = if container.ContainsKey(mode) 
                        then container.[mode]
                        else []
        container
        |> Map.change mode (func :: current)    
        
module Publish =
    let broadcast<'a> (func:('a -> unit)-> unit) mode (container : Container<'a>) =
        container.[mode]
        |> List.iter func
        container

    let sync<'a> message = broadcast<'a> (fun f -> f message)
    let async<'a> message = broadcast<'a> (fun f -> async{ f message} |> Async.Start)

