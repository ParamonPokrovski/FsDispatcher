module FsDispatcher.Deliver
        
open FsDispatcher.Prelude

type SyncMode =
    | Head
    | Tail

type BasicMode =
    | Sync of SyncMode  
    | Async

type QueueMode =
    | Sync
    
type Mode =
    | Basic of BasicMode
    | Queue of QueueMode
    | DedicatedQueue of obj*QueueMode

type ModeKey = int
let key (mode : _) : ModeKey= 
    mode.GetHashCode()

type Container<'a> = Map<ModeKey, ('a -> unit) list>

module Container =
    let private create<'a> seq : Container<'a> = 
        seq
        |> Seq.map (fun x -> x, []) 
        |> Map.ofSeq

    let basic<'a> =
        [BasicMode.Async; BasicMode.Sync SyncMode.Head; BasicMode.Sync SyncMode.Tail]
        |> Seq.map key
        |> create<'a>

    let queue<'a> =        
        [QueueMode.Sync]
        |> Seq.map key
        |> create<'a>

    let register<'a> mode (func: 'a -> unit) (container : Container<'a>) =    
        let current = if container.ContainsKey(mode) 
                        then container.[mode]
                        else []
        container
        |> Map.change mode (func :: current)            

