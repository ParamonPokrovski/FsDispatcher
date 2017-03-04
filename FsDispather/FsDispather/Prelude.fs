namespace FsDispatcher.Prelude

    module Option =
        let cast<'a> v =
            match box v with 
            | :? 'a as a -> Some a 
            | _ -> None    
            
    module Seq =
       let ofType<'a> = Seq.choose(Option.cast<'a>)

       let any<'a> = Seq.exists (fun x -> (Option.cast<'a> x).IsSome)
       let find<'a> = Seq.find (fun x -> (Option.cast<'a> x).IsSome)

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