namespace FsDispatcher

open NUnit.Framework
open FsUnit

open FsDispatcher.Dispatcher
open FsDispatcher.Publisher
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

        let d = create
                //sync tail 
                |> Register.syncTail<string> (has "m")
                |> Register.syncTail<int> (has 12)
                |> Register.syncTail<obj> (hasOneOf [12; "m"])
                //sync head 
                |> Register.syncHead<string> (has "m")
                |> Register.syncHead<int> (has 12)
                |> Register.syncHead<obj> (hasOneOf [12; "m"])
                //async
                |> Register.async<string> (has "m")
                |> Register.async<int> (has 12)
                |> Register.async<obj> (hasOneOf [12; "m"])
                //queue
                |> Register.queue<string> (has "m")
                |> Register.queue<int> (has 12)
                |> Register.queue<obj> (hasOneOf [12; "m"])
                //dedicated queue
                |> Register.dedicatedQueue<string> 1 (has "m")
                |> Register.dedicatedQueue<int> 1 (has 12)
                |> Register.dedicatedQueue<obj> 1 (hasOneOf [12; "m"])
                |> init
    
        Send.sync d 12
        Send.sync d "m"   

        System.Threading.Thread.Sleep (3000)
        
        (fun x -> E.Complete x )
        |> checkCount.PostAndReply
        |> should equal 20