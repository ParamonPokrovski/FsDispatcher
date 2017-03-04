module FsDispatcher.Dispatcher
        
open FsDispatcher.Prelude
open FsDispatcher

type Dispatcher = 
    private { funcs :  Deliver.Container<obj>
              queues : Queue.Container }
    
let create = { funcs = Deliver.Container.basic<obj>
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
        

let broadcast<'a> (func:(obj -> unit)-> unit) mode dispatcher =
    dispatcher.funcs.[mode]
    |> List.iter func
    dispatcher

let enqueue<'a> message dispatcher =
    dispatcher.queues 
    |> Queue.Container.publish message
    dispatcher

let init dispatcher = 
    Queue.Container.init dispatcher.queues
    dispatcher