namespace FsDispatcher

open NUnit.Framework
open FsUnit

open FsDispatcher.Dispatcher
open FsDispatcher.Helpers
open Tests.Evaluator

[<TestFixture>]
module SimpleTests =

    type State =
        private 
            { checkCount : Collector.Collector<obj, int>
              dispatcher : Dispatcher
            }

    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module State =
        let create() = 
            { checkCount = Tests.Evaluator.Collector.increment<obj>
              dispatcher = create()
            }

        let send msg state = 
            Send.sync state.dispatcher msg
            System.Threading.Thread.Sleep (2000)
            state

        let checkCount value state =
            state.checkCount
            |> Collector.shouldCompleteWith value
            state

    module Has = 
        let one v state msg =
            consume v msg
            state.checkCount.Post (Collector.E.Value msg)

        let oneOf v state msg =
            consumeAny v msg
            state.checkCount.Post (Collector.E.Value msg)

    [<Test>]
    let ``Messages receiving by delivery modes``() =            
        let regMode state mode = 
            Dispatcher.register<string> mode (Has.one "m" state)
            >> Dispatcher.register<int> mode (Has.one 12 state)
            >> Dispatcher.register<obj> mode (Has.oneOf [12; "m"] state)
            
        let regAllBasic state dispatcher mode = 
            [Deliver.SyncMode.Tail |> Deliver.BasicMode.Sync
             Deliver.SyncMode.Head |> Deliver.BasicMode.Sync
             Deliver.BasicMode.Async ]
            |> Seq.map mode
            |> Seq.fold (fun d m -> regMode state m d) dispatcher
                     
        let register state = 
            {state with
                dispatcher = 
                    [Deliver.Mode.Basic
                     (fun x -> x |> Deliver.QueueMode.Each |> Deliver.Mode.Queue)
                     (fun x -> x |> Deliver.QueueMode.Last |> Deliver.Mode.Queue)
                     (fun x -> Deliver.Mode.DedicatedQueue (1, x |> Deliver.QueueMode.Each))
                     (fun x -> Deliver.Mode.DedicatedQueue (1, x |> Deliver.QueueMode.Last))]
                    |> Seq.fold (regAllBasic state) state.dispatcher
                    |> init}

        State.create()
        |> register
        |> State.send 12
        |> State.send "m"
        |> State.checkCount 60
        |> ignore

    [<Test>]
    let ``Messages receiving by helper functions``() =    
        let m = 1

        let register state = 
            let check = Has.one m state
            {state with
                dispatcher = 
                    state.dispatcher
                    |> Register.async check
                    |> Register.sync check
                    |> Register.Queue.each check
                    |> Register.Queue.last check
                    |> Register.Queue.Dedicated.each 1 check
                    |> Register.Queue.Dedicated.eachParallel 1 5 check
                    |> Register.Queue.Dedicated.last 1 check
                    |> Register.Queue.Exclusive.each check
                    |> Register.Queue.Exclusive.eachParallel 5 check
                    |> init
            }

        State.create()
        |> register
        |> State.send m
        |> State.checkCount 9
        |> ignore