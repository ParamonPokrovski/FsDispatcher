module FsDispatcher.Tests.Evaluator

open FsUnit

type E<'a, 'b> =
        | Value of 'a
        | Complete of AsyncReplyChannel<'b>

let collector<'a, 'b> execute initState = 
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
    collector<'a, int> (fun _ state -> state + 1) 0