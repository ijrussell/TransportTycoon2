open System.IO

type Tree<'T> =
    | Branch of 'T * Tree<'T> seq
    | Leaf of 'T

type Waypoint = { Location:string; Route:string list; TotalDistance:int }

type Connection = { Start:string; Finish:string; Distance:int }

let loadData path =
    path
    |> File.ReadAllText
    |> fun text -> text.Split(System.Environment.NewLine)
    |> Array.skip 1
    |> fun rows -> [
        for row in rows do
            match row.Split(",") with
            | [|start;finish;distance|] -> 
                { Start = start; Finish = finish; Distance = int distance }
                { Start = finish; Finish = start; Distance = int distance }
            | _ -> failwith "Row is badly formed"
    ]
    |> List.groupBy (fun cn -> cn.Start)
    |> Map.ofList

let getUnvisited connections current =
    connections
    |> List.filter (fun cn -> current.Route |> List.exists (fun loc -> loc = cn.Finish) |> not)
    |> List.map (fun cn -> { 
        Location = cn.Finish
        Route = cn.Start :: current.Route
        TotalDistance = cn.Distance + current.TotalDistance })

let rec treeToList tree =
    match tree with 
    | Leaf x -> [x]
    | Branch (_, xs) -> List.collect treeToList (xs |> Seq.toList)

let findPossibleRoutes start finish (routeMap:Map<string, Connection list>) =
    let rec loop current =
        let nextRoutes = getUnvisited routeMap[current.Location] current
        if nextRoutes |> List.isEmpty |> not && current.Location <> finish then
            Branch (current, seq { for next in nextRoutes do loop next })
        else 
            Leaf current
    loop { Location = start; Route = []; TotalDistance = 0 }
    |> treeToList
    |> List.filter (fun wp -> wp.Location = finish)

let selectShortestRoute routes =
    routes 
    |> List.minBy (fun wp -> wp.TotalDistance)
    |> fun wp -> wp.Location :: wp.Route |> List.rev, wp.TotalDistance

[<EntryPoint>]
let main argv =
    Path.Combine(__SOURCE_DIRECTORY__, "resources", "data.csv") 
    |> loadData
    |> findPossibleRoutes argv[0] argv[1]
    |> selectShortestRoute
    |> printfn "%A"
    0
