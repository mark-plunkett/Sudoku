open System

let isValid board row col =
    true

let rec solve size (board:int[,]) row col =

    if row = size - 1 && col = size then
        Some board
    if board.[row,col] <> 0 then
        solve size board row (col + 1)
    else if col = size then
        solve size board (row + 1) 0
    else
        // we have a 0 so change 0 to each value, check validity and solve the next cell
        [1..size]
        |> Seq.
        for i = 1 to size do
            board.[row,col] <- i
            if isValid board row col then
                match solve size board row (col + 1) with
                | Some b -> Some b
                | _ -> None
            
            board.[row,col] <- 0

        None

[<EntryPoint>]
let main argv =
    let size = 9
    let solver = solve size

    let grid = [
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
    let board = Array2D.init size size (fun x y -> grid.[y].[x])
    match solve size board 0 0 with
    | Some solution -> ()
    | _ -> printfn "No solution"

    0