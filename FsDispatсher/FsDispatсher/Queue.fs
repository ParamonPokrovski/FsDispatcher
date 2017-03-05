module FsDispatcher.Queue
        
open FsDispatcher.Prelude
open FsDispatcher.Deliver

[<AbstractClass>]
type Processor() =
   abstract member Add: obj -> unit
   abstract member Start: unit -> unit

type Processor<'a>(mailbox : MailboxProcessor<'a>, funcs : Deliver.Container<'a>) =
    inherit Processor()
    member this.Mailbox = mailbox
    member this.Funcs = funcs
    override this.Add message = Func.doIf<'a> mailbox.Post message
    override this.Start() = mailbox.Start()

module Mailbox =
    let empty<'a> = new MailboxProcessor<'a>(fun x -> async{()})

    let create<'a> container = 
        new MailboxProcessor<'a>(fun inbox ->                                     
                                    let rec messageLoop() = async{
                                        
                                        let! msg = inbox.Receive()
        
                                        container
                                        |> Publish.Broadcast.sync msg (key (SyncMode.Head |> BasicMode.Sync |> QueueMode.Each))
                                        |> Publish.Parralel.run msg (key (BasicMode.Async |> QueueMode.Each))
                                        |> Publish.Broadcast.sync msg (key (SyncMode.Tail |> BasicMode.Sync |> QueueMode.Each))
                                        |> ignore
                                        
                                        return! messageLoop()}                                    
                                    messageLoop() )

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Processor =
    let create<'a> id = new Processor<'a>(Mailbox.empty<'a>, Deliver.Container.Create.queue<'a>)
    
    let register<'a> mode (f : 'a -> unit) (proc : Processor<'a>) =
        let newFuncs = proc.Funcs
                       |> Deliver.Container.register<'a> mode f 
        new Processor<'a>(Mailbox.create<'a> newFuncs, newFuncs)
        
type Container = 
    private { processors : Map<string, Processor> }

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Container =
    let private key<'a> id =  
        sprintf "%A | %A" typeof<'a> id

    let empty = { processors = Map.empty }     

    let private create<'a> key (container : Container) =        
        if not(container.processors.ContainsKey(key))
        then
            let newProcessor = Processor.create<'a> id
            let newProcessors = container.processors.Add(key, newProcessor)
            { container with
                processors = newProcessors }
        else container

    let private add<'a> mode key (f : 'a -> unit) (container : Container) =
        let p = container.processors.[key] :?> Processor<'a>
                |> Processor.register<'a> mode f
        { container with
           processors = container.processors
                        |> Map.change key (p :> Processor) }

    let registerWithId<'a> mode id (f : 'a -> unit) =
        let k = key<'a> id
        create<'a> k
        >> add<'a> mode k f

    let register<'a> mode (f : 'a -> unit) =
        registerWithId<'a> mode None (f : 'a -> unit)

    let publish message container = 
        container.processors 
        |> Map.iter (fun _ p -> p.Add message)

    let init container = 
        container.processors 
        |> Map.iter (fun _ p -> p.Start())

