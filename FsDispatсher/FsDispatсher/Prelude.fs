namespace FsDispatcher.Prelude

    module Option =
        let cast<'a> v =
            match box v with 
            | :? 'a as a -> Some a 
            | _ -> None    
            
    module Check =
        let typeof<'a> x = (Option.cast<'a> x).IsSome

    module Seq =
       let ofType<'a> = Seq.choose(Option.cast<'a>)

       let any<'a> = Seq.exists Check.typeof<'a>
       let find<'a> = Seq.find Check.typeof<'a>

       let iter<'a> func =
            ofType<'a -> unit>
            >> Seq.iter func

    module Map =
        let change key value map =
            map
            |> Map.remove key
            |> Map.add key value

    module Func =
        let doIf<'a> (func : 'a -> unit) =
            fun (x:obj) -> match Option.cast<'a> x with
                           | Some a -> func a
                           | _ -> ()

        let execute count f =
            [1..count]
            |> List.iter (fun _ -> f())

    module Threading =
        let sleep (ms:int) = 
            System.Threading.Thread.Sleep(ms)

    module Guid =
        let create = System.Guid.NewGuid()