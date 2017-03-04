module FsDispatcher.Dispatcher
        
open FsDispatcher.Prelude
open FsDispatcher

type Dispatcher = 
    private { funcs :  Deliver.Container<obj>
              queues : Queue.Container }
    
let create = { funcs = Deliver.Container.Create.basic<obj>
               queues = Queue.Container.empty}

let register<'a> mode (func: 'a -> unit) (dispatcher : Dispatcher) =   
    match mode with
    | Deliver.Mode.Basic m ->
        { dispatcher with
            funcs = dispatcher.funcs
                    |> Deliver.Container.register<obj> (Deliver.key m) (Func.doIf<'a> func)
        }
    | Deliver.Mode.Queue m ->
        { dispatcher with
            queues = dispatcher.queues
                     |> Queue.Container.register<'a> (Deliver.key m) func
        }
    | Deliver.Mode.DedicatedQueue (id, m) ->
        { dispatcher with
            queues = dispatcher.queues
                     |> Queue.Container.registerWithId<'a> (Deliver.key m) (Some id) func
        }

let init dispatcher = 
    Queue.Container.init dispatcher.queues
    dispatcher

module Deliver =
    open FsDispatcher.Deliver

    let enqueue message dispatcher =
        dispatcher.queues 
        |> Queue.Container.publish message
        dispatcher

    let basic func dispatcher =
        dispatcher.funcs
        |> func |> ignore
        dispatcher

    let start<'a> dispatcher (message : 'a) = 
        let om = box message
        dispatcher
        |> basic (Publish.sync om (key (BasicMode.Sync SyncMode.Head)))
        |> basic (Publish.sync om (key BasicMode.Async))
        |> enqueue om
        |> basic (Publish.sync om (key (BasicMode.Sync SyncMode.Tail)))
        |> ignore