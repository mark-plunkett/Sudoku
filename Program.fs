open System

let isValid board =
    true

let rec solve size (board:Map<int, Map<int, int>>) row col =

    if row = size - 1 && col = size then
        Some board
    else if col = size then
        solve size board (row + 1) 0
    else if board.[row].[col] <> 0 then
        solve size board row (col + 1)
    else
        (*
            We want to change the 0 to a new value
            then try and solve with the board containing the new value
        *)

        [1..size]
        |> Seq.map (fun i -> board.Add(row, board.[row].Add(col, i)))
        |> Seq.filter isValid
        |> Seq.tryPick (fun b -> solve size b row col)

[<EntryPoint>]
let main argv =
    let size = 9
    let solver = solve size

    let inline listToMap list = list |> List.indexed |> Map.ofList
    let listToBoard = (List.map listToMap) >> listToMap
    let board = listToBoard [
        [3; 0; 6; 5; 0; 8; 4; 0; 0]
        [5; 2; 0; 0; 0; 0; 0; 0; 0]
        [0; 8; 7; 0; 0; 0; 0; 3; 1]
        [0; 0; 3; 0; 1; 0; 0; 8; 0]
        [9; 0; 0; 8; 6; 3; 0; 0; 5]
        [0; 5; 0; 0; 9; 0; 6; 0; 0]
        [1; 3; 0; 0; 0; 0; 2; 5; 0]
        [0; 0; 0; 0; 0; 0; 0; 7; 4]
        [0; 0; 5; 2; 0; 6; 3; 0; 0]
    ]

    match solve size board 0 0 with
    | Some solution -> ()
    | _ -> printfn "No solution"

    0