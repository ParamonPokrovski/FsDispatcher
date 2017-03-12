namespace FsDispatcher

open NUnit.Framework
open FsUnit

open FsDispatcher.Dispatcher
open FsDispatcher.Helpers
open FsDispatcher.Prelude
open FsDispatcher.Parallel
open Tests.Evaluator
open Collector

[<TestFixture>]
module ParallelTests =

    [<Test>]
    let ``Function is executed parralel``() =            
        let sum = Collector.increment

        let parallelism = 5
        let waitTime = 10000 

        fun () -> 
            sum.Post <| E.Value () 
            Threading.sleep waitTime
        |> Func.mapParallel parallelism 
        |> Func.execute (parallelism*2)

        Threading.sleep (waitTime/2)

        sum
        |> Collector.shouldCompleteWith 5
        |> ignore