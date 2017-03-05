module FsDispatcher.Tests.Evaluator

open FsUnit

let consume assume message = 
        message |> should equal assume

let consumeAny<'a when 'a : equality> (assumes : 'a seq) (m : 'a) = 
    (assumes |> Seq.exists ((=) m))
    |> should be True 

module Collector =
    type E<'a, 'b> =
            | Value of 'a
            | Complete of AsyncReplyChannel<'b>

    type Collector<'a, 'b> =  MailboxProcessor<E<'a, 'b>>

    let create<'a, 'b> execute initState = 
        MailboxProcessor<E<'a, 'b>>.Start(fun inbox->             
            let rec messageLoop state = async{
                let! msg = inbox.Receive()        
                match msg with
                | Value x -> 
                    return! messageLoop (execute x state)  
                | Complete x -> 
                    x.Reply state
                }
            messageLoop initState )

    let increment<'a> =
        create<'a, int> (fun _ state -> state + 1) 0

    let shouldCompleteWith result (c : Collector<_,_>)=
        (fun x -> E.Complete x )
        |> c.PostAndReply
        |> should equal result