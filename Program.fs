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
        >> Seq.forall (fun (v, g) -> v <> 0 && Seq.length g < 2)

    let rowValid = 
        board.[row]
        |> Map.toSeq
        |> Seq.map snd
        |> uniqueOrZero

    let colValid =
        board
        |> Map.toSeq
        |> Seq.map (fun (_, map) -> map.[col])
        |> uniqueOrZero

    let rowMod = row % 3
    let colMod = col % 3
    let squareValid =
        [ for j = rowMod to rowMod + 2 do
            for i = colMod to colMod + 2 do
                yield i, j
        ]
        |> Seq.map (fun (i, j) -> board.[j].[i])
        |> uniqueOrZero
    
    //printfn "rowSum %i" rowSum
    //printfn "colSum %i" colSum
    //printfn "squareSum %i" squareSum

    rowValid && colValid && squareValid

let rec solve size (board:Board) row col =

    printfn "%i x %i" row col
    printfn "%s%s" (boardToString board) Environment.NewLine

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

            for each value that can go in the cell
            build a new board with the value at row,col
            check whether it's valid according to sudoku rules
            if yes then recurse to solve with the new board

        *)

        [1..size]
        |> Seq.map (fun i -> board.Add(row, board.[row].Add(col, i)))
        |> Seq.filter (isValid row col)
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
    | Some solution -> solution |> boardToString |> printfn "%s"
    | _ -> printfn "No solution"

    0