namespace FsDispatcher

open NUnit.Framework
open FsUnit

open FsDispatcher.Dispatcher
open FsDispatcher.Helpers
open Tests.Evaluator

[<TestFixture>]
module SimpleTests =
    
    let consume assume message = 
        message |> should equal assume
    let consumeAny<'a when 'a : equality> (assumes : 'a seq) (m : 'a) = 
        (assumes |> Seq.exists ((=) m))
        |> should be True 
    
    [<Test>]
    let ``Messages receiving``() =            
        let checkCount = Tests.Evaluator.increment<obj>
        let has v msg =
            consume v msg
            checkCount.Post (E.Value msg)
        let hasOneOf v msg =
            consumeAny v msg
            checkCount.Post (E.Value msg)

        let regMode mode = 
            Dispatcher.register<string> mode (has "m")
            >> Dispatcher.register<int> mode (has 12)
            >> Dispatcher.register<obj> mode (hasOneOf [12; "m"])
            
        let regAllBasic dispatcher mode = 
            [Deliver.SyncMode.Tail |> Deliver.BasicMode.Sync
             Deliver.SyncMode.Head |> Deliver.BasicMode.Sync
             Deliver.BasicMode.Async ]
            |> Seq.map mode
            |> Seq.fold (fun d m -> regMode m d) dispatcher
             
        (*let d = create()
                |> regAllBasic Deliver.Mode.Basic
                |> regAllBasic (fun x -> x |> Deliver.QueueMode.Each |> Deliver.Mode.Queue)
                |> regAllBasic (fun x -> x |> Deliver.QueueMode.Last |> Deliver.Mode.Queue)
                |> regAllBasic (fun x -> Deliver.Mode.DedicatedQueue (1, x |> Deliver.QueueMode.Each))
                |> regAllBasic (fun x -> Deliver.Mode.DedicatedQueue (1, x |> Deliver.QueueMode.Last))
                |> init*)
             
        let d = [Deliver.Mode.Basic
                 (fun x -> x |> Deliver.QueueMode.Each |> Deliver.Mode.Queue)
                 (fun x -> x |> Deliver.QueueMode.Last |> Deliver.Mode.Queue)
                 (fun x -> Deliver.Mode.DedicatedQueue (1, x |> Deliver.QueueMode.Each))
                 (fun x -> Deliver.Mode.DedicatedQueue (1, x |> Deliver.QueueMode.Last))]
                |> Seq.fold regAllBasic (create())
                |> init

        Send.sync d 12
        System.Threading.Thread.Sleep (1000)
        Send.sync d "m"   
        System.Threading.Thread.Sleep (1000)
        
        (fun x -> E.Complete x )
        |> checkCount.PostAndReply
        |> should equal 60