module FsDispatcher.Dispatcher
        
open FsDispatcher.Prelude
open FsDispatcher

type Dispatcher = 
    private { funcs :  Deliver.Container<obj>
              queues : Queue.Container }
    
let create = { funcs = Deliver.Container.createNoQueue<obj>
               queues = Queue.Container.empty}

let register<'a> deliver (func: 'a -> unit) (dispatcher : Dispatcher) =   
    if Deliver.isNoQueueMode deliver
    then { dispatcher with
            funcs = dispatcher.funcs
                    |> Deliver.Container.register<obj> deliver (Func.doIf<'a> func)
         }
    else { dispatcher with
            queues = dispatcher.queues
                     |> Queue.Container.register<'a> deliver func
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