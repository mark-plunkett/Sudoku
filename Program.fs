open System

open FSharpPlus

type Board = Map<int, Map<int, int>>

let boardToString =
    Map.toSeq
    >> Seq.map (
        snd 
        >> Map.toSeq 
        >> Seq.map (snd >> string) 
        >> String.concat " "
    )
    >> String.concat Environment.NewLine

let isValid row col (board:Board) =

    let uniqueOrZero =
        Seq.groupBy id
        >> Seq.forall (fun (v, g) -> v = 0 || Seq.length g = 1)
        
    let rowValid (board:Board) = 
        board.[row]
        |> Map.toSeq
        |> Seq.map snd
        |> uniqueOrZero

    let colValid (board:Board) =
        board
        |> Map.toSeq
        |> Seq.map (fun (_, map) -> map.[col])
        |> uniqueOrZero

    let squareValid (board:Board) =
        let rowMod = row - (row % 3)
        let colMod = col - (col % 3)
        [ for j = rowMod to rowMod + 2 do
            for i = colMod to colMod + 2 do
                yield i, j
        ]
        |> Seq.map (fun (i, j) -> board.[j].[i])
        |> uniqueOrZero

    rowValid board
    && colValid board 
    && squareValid board

let rec solve size row col (board:Board) =
    if row = size - 1 && col = size then
        Some board
    else if col = size then
        solve size (row + 1) 0 board
    else if board.[row].[col] <> 0 then
        solve size row (col + 1) board
    else
        [1..size]
        |> Seq.map (fun i -> board.Add(row, board.[row].Add(col, i)))
        |> Seq.filter (isValid row col)
        |> Seq.tryPick (solve size row col)

[<EntryPoint>]
let main argv =
    let size = 9
    let solver = solve size 0 0

    let inline listToMap list = list |> List.indexed |> Map.ofList
    let listToBoard = (List.map listToMap) >> listToMap
    //let board = listToBoard [
        //[3; 0; 6; 5; 0; 8; 4; 0; 0]
        //[5; 2; 0; 0; 0; 0; 0; 0; 0]
        //[0; 8; 7; 0; 0; 0; 0; 3; 1]
        //[0; 0; 3; 0; 1; 0; 0; 8; 0]
        //[9; 0; 0; 8; 6; 3; 0; 0; 5]
        //[0; 5; 0; 0; 9; 0; 6; 0; 0]
        //[1; 3; 0; 0; 0; 0; 2; 5; 0]
        //[0; 0; 0; 0; 0; 0; 0; 7; 4]
        //[0; 0; 5; 2; 0; 6; 3; 0; 0]
    //]
    let board = listToBoard [
        [ 0; 0; 0; 0; 0; 0; 0; 0; 0; ]
        [ 0; 0; 0; 0; 0; 0; 0; 0; 0; ]
        [ 0; 0; 0; 0; 0; 0; 0; 0; 0; ]
        [ 0; 0; 0; 0; 0; 0; 0; 0; 0; ]
        [ 0; 0; 0; 0; 0; 0; 0; 0; 0; ]
        [ 0; 5; 0; 3; 0; 8; 0; 2; 0; ]
        [ 2; 0; 5; 0; 3; 0; 6; 0; 9; ]
        [ 0; 9; 0; 4; 0; 6; 0; 1; 0; ]
        [ 0; 0; 0; 0; 0; 0; 0; 0; 0; ]
    ]

    match solver board with
    | Some solution -> solution |> boardToString |> printfn "%s"
    | _ -> printfn "No solution"

    0