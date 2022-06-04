open System.IO

type Tree<'T> =
    | Branch of 'T * Tree<'T> seq
    | Leaf of 'T

type Waypoint = { Location:string; Route:(string * decimal) list; Duration:decimal }

type Connection = { Start:string; Finish:string; Distance:int; Speed:int }

let loadData path =
    path
    |> File.ReadAllText
    |> fun text -> text.Split(System.Environment.NewLine)
    |> Array.skip 1
    |> fun rows -> [
        for row in rows do
            match row.Split(",") with
            | [|start;finish;distance;speed|] -> 
                { Start = start; Finish = finish; Distance = int distance; Speed = int speed }
                { Start = finish; Finish = start; Distance = int distance; Speed = int speed }
            | _ -> failwith "Row is badly formed"
    ]
    |> List.groupBy (fun cn -> cn.Start)
    |> Map.ofList

let getUnvisited connections current =
    connections
    |> List.filter (fun cn -> current.Route |> List.exists (fun loc -> fst loc = cn.Finish) |> not)
    |> List.map (fun cn -> 
        let duration = decimal cn.Distance / decimal cn.Speed
        { Location = cn.Finish; Route = (cn.Start, current.Duration) :: current.Route; Duration = duration + current.Duration })

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
    loop { Location = start; Route = []; Duration = 0M }
    |> treeToList
    |> List.filter (fun wp -> wp.Location = finish)

let selectFastestRoute routes =
    routes 
    |> List.minBy (fun wp -> wp.Duration)

// let prepareOutput start waypoint =
//     waypoint
//     |> fun wp -> (wp.Location, wp.Duration) :: wp.Route |> List.rev |> List.tail
//     |> List.fold (fun acc (loc,time) -> fst acc + "\n" + $"%.2f{time}h  ARRIVE  {loc}", 0M) ($"00.00h  DEPART  {start}", 0M)
//     |> fst    

let prepareOutput waypoint =
    waypoint
    |> fun wp -> (wp.Location, wp.Duration) :: wp.Route |> List.rev
    |> function
        | [] -> failwith "No route to output"
        | head::tail ->     
            tail |> List.fold (fun acc (loc,time) -> fst acc + "\n" + $"%.2f{time}h  ARRIVE  {loc}", 0M) ($"00.00h  DEPART  {fst head}", 0M)
    |> fst    

[<EntryPoint>]
let main argv =
    Path.Combine(__SOURCE_DIRECTORY__, "resources", "data.csv") 
    |> loadData
    |> findPossibleRoutes argv[0] argv[1]
    |> selectFastestRoute
    |> prepareOutput
    |> printfn "%A"
    0
