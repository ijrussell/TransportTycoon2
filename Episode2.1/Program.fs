module TransportTycoon

open System.IO

type Connection = { From:string; To:string; Distance:int }

module List = 
    // Thanks to Rich Minerich http://www.fssnip.net/4u/title/Very-Fast-Permutations
    let rec permutations = function
        | []      -> seq [List.empty]
        | x :: xs -> Seq.collect (insertions x) (permutations xs)
    and insertions x = function
        | []             -> [[x]]
        | (y :: ys) as xs -> (x::xs)::(List.map (fun x -> y::x) (insertions x ys))

let lines = 
    Path.Combine(__SOURCE_DIRECTORY__, "resources", "data.csv")
    |> File.ReadAllText
    |> fun data -> data.Split(System.Environment.NewLine)
    |> Array.skip 1

let myData = [
    for line in lines do
        match line.Split("\t") with
        | [|A; B; Km|] -> 
            { From = A.Trim(); To = B.Trim(); Distance = int Km }
            { From = B.Trim(); To = A.Trim(); Distance = int Km }
        | _ -> ()
]

let locations = 
    myData
    |> List.map (fun x -> x.From)
    |> List.distinct

let connectionMap =
    myData
    |> List.map (fun x -> (x.From, x.To), x)
    |> Map.ofList

let allPermutationsFrom start = 
    locations
    |> List.permutations 
    |> Seq.toList
    |> List.filter (fun x -> x.Head = start)

let endAt finish possible = 
    let index = List.findIndex (fun x -> x = finish) possible
    possible
    |> List.splitAt (index + 1)
    |> fst

let getCandidates start finish = 
    allPermutationsFrom start
    |> List.map (endAt finish)
    |> List.distinct

let findShortestRoute start finish =
    getCandidates start finish
    |> List.map (fun route -> 
        route 
        |> List.pairwise
        |> List.map (fun conn -> Map.tryFind conn connectionMap)
    )
    |> List.filter (fun x -> x |> List.forall (fun z -> z <> None))
    |> List.map (fun x -> 
        x 
        |> List.choose id 
        |> List.map (fun connection -> connection.To, connection.Distance)
        |> List.fold (fun acc (loc,dist) -> (fst acc + "," + loc, snd acc + dist)) (start, 0)    
    )
    |> List.minBy snd

[<EntryPoint>]
let main argv =
    findShortestRoute argv.[0] argv.[1]
    |> fun x -> printfn "%s" (fst x)
    0

