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

        let testMode mode = 
            Dispatcher.register<string> mode (has "m")
            >> Dispatcher.register<int> mode (has 12)
            >> Dispatcher.register<obj> mode (hasOneOf [12; "m"])

        let d = create
                |> testMode (Deliver.SyncMode.Tail |> Deliver.BasicMode.Sync |> Deliver.Mode.Basic)
                |> testMode (Deliver.SyncMode.Head |> Deliver.BasicMode.Sync |> Deliver.Mode.Basic)
                |> testMode (Deliver.BasicMode.Async |> Deliver.Mode.Basic)
                |> testMode (Deliver.SyncMode.Tail |> Deliver.BasicMode.Sync |> Deliver.QueueMode.Each |> Deliver.Mode.Queue)
                |> testMode (Deliver.SyncMode.Head |> Deliver.BasicMode.Sync |> Deliver.QueueMode.Each |> Deliver.Mode.Queue)
                |> testMode (Deliver.BasicMode.Async |> Deliver.QueueMode.Each |> Deliver.Mode.Queue)
                |> testMode (Deliver.Mode.DedicatedQueue (1, Deliver.SyncMode.Tail |> Deliver.BasicMode.Sync |> Deliver.QueueMode.Each))
                |> testMode (Deliver.Mode.DedicatedQueue (1, Deliver.SyncMode.Head |> Deliver.BasicMode.Sync |> Deliver.QueueMode.Each))
                |> testMode (Deliver.Mode.DedicatedQueue (1, Deliver.BasicMode.Async |> Deliver.QueueMode.Each))
                |> init
    
        Send.sync d 12
        Send.sync d "m"   

        System.Threading.Thread.Sleep (3000)
        
        (fun x -> E.Complete x )
        |> checkCount.PostAndReply
        |> should equal 36