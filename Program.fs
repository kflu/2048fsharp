﻿(*
TODOs:
- The double for loops can be replaced with sequence generators
- Use single case DU instead of type alias maybe?
- Design the Board type to a record to embed vacancy?
 *)

open System

[<AutoOpen>]
module Types = 
    type Board = int [,]
    type Direction = Up | Left | Down | Right

module Board = 
    let private swap (x1, y1) (x2, y2) (array: _[,]) = 
        let tmp = array.[x1, y1]
        array.[x1, y1] <- array.[x2, y2]
        array.[x2, y2] <- tmp
        array

    let rotateCC (board: Board) : Board = 
        let transpose (board: Board) : Board = 
            for x = 0 to board.GetLength(0) - 1 do
                for y = 0 to x do
                    swap (x, y) (y, x) board |> ignore
            board

        let flip (board: Board) : Board =
            let nRow = board.GetLength(0)
            for x = 0 to board.GetLength(0) - 1 do
                for y = 0 to board.GetLength(1) / 2 - 1 do
                    swap (x, y) (x, nRow - 1 - y) board |> ignore
            board

        board |> transpose |> flip

    let collapseRowRight (board: Board) (iRow: int): bool =
        (* Rules: 
           1. 0s don't take slots
           2. two same values collapse into 1 slot
           3. collapsed slot doesn't furter collapse

           TODO: this could also be written in a recursive way
         *)
        let N = board.GetLength(0)
        let mutable cur = N - 1
        let mutable peeker = cur
        let mutable hasUpdated = false

        let findNextNonZero () =
            peeker <- seq { peeker - 1 .. -1 .. 0 } 
                      |> Seq.tryFind (fun i -> board.[iRow, i] <> 0)
                      |> function
                         | Some(n) -> n
                         | _ -> -1
            peeker

        while findNextNonZero () >= 0 do
            if board.[iRow, cur] = 0 then
                swap (iRow, cur) (iRow, peeker) board |> ignore
                hasUpdated <- true
            elif board.[iRow, cur] = board.[iRow, peeker] then 
                board.[iRow, cur] <- 2 * board.[iRow, cur]
                board.[iRow, peeker] <- 0
                cur <- cur - 1
                hasUpdated <- true
            else // board[cur] <> board[peeker]
                swap (iRow, peeker) (iRow, cur - 1) board |> ignore
                if cur - 1 <> peeker then hasUpdated <- true
                cur <- cur - 1

        hasUpdated
    
    let getVacancy (board: Board) : (int * Board) =
        let mutable vacancy = 0
        for x = 0 to board.GetLength(0) - 1 do
            for y = 0 to board.GetLength(1) - 1 do
                if board.[x, y] = 0 then vacancy <- vacancy + 1
        (vacancy, board)

    let private sprout (vacancy: int) (board: Board) : int * Board = 
        let mutable slots = vacancy
        assert (slots > 0) // The game should be over if there's no vacancy

        let mutable pos = System.Random().Next slots
        let mutable sprouted = false
        for x = 0 to board.GetLength(0) - 1 do
            for y = 0 to board.GetLength(1) - 1 do
                if board.[x, y] = 0 then do
                    if pos = 0 then do 
                        // FIXME: I want to break after this is met. F# doesn't have break
                        // so I'm not writing it F#-esque.
                        board.[x, y] <- 2
                        sprouted <- true
                        pos <- pos - 1
                    elif pos > 0 then pos <- pos - 1

        assert (sprouted = true)
        slots - 1, board

    let private collapseRight (board: Board) : Board * bool =
        seq { 0 .. board.GetLength(0) - 1 } 
        |> Seq.filter (collapseRowRight board)
        |> Seq.length
        |> function 0 -> board, false | _ -> board, true

    let collapse (board: Board) (direction: Direction) : int * Board * bool = 

        let mutable hasUpdated = false
        let collapseRight board = 
            collapseRight board
            |> function
               | (_, true) -> hasUpdated <- true
               | _ -> ()
            board

        let (vacancy, board) = 
            match direction with 
            | Right -> board |> collapseRight
            | Down  -> board |> rotateCC |> rotateCC |> rotateCC |> collapseRight |> rotateCC
            | Left  -> board |> rotateCC |> rotateCC |> collapseRight |> rotateCC |> rotateCC
            | Up    -> board |> rotateCC |> collapseRight |> rotateCC |> rotateCC |> rotateCC
            |> getVacancy

        match hasUpdated with 
        | true -> sprout vacancy board |> function (x, y) -> x, y, hasUpdated
        | false -> vacancy, board, hasUpdated

    let create (size: int) : Board =
        Array2D.zeroCreate size size |> sprout (size * size) |> snd

module ConsoleUi =
    let private mapColor : int -> ConsoleColor = function
        | 0 -> ConsoleColor.DarkGray 
        | 2 -> ConsoleColor.Cyan 
        | 4 -> ConsoleColor.Magenta 
        | 8 -> ConsoleColor.Red 
        | 16 -> ConsoleColor.Green 
        | 32 -> ConsoleColor.Yellow 
        | 64 -> ConsoleColor.Yellow 
        | 128 -> ConsoleColor.DarkCyan 
        | 256 -> ConsoleColor.Cyan 
        | 512 -> ConsoleColor.DarkMagenta 
        | 1024 -> ConsoleColor.Magenta 
        | _ -> ConsoleColor.Red

    let private printNum num =
        Console.ForegroundColor <- mapColor num
        printf "%6d" num
        Console.ResetColor ()

    let print (board: Board) : Board =
        Console.Clear ()
        let height = board.GetLength 0
        let width = board.GetLength 1

        seq { for x in 0 .. height - 1 do
              for y in 0 .. width - 1 do
                yield (x, y) }
        |> Seq.iter (function 
            | (x, 0) -> printfn ""; printNum board.[x, 0]
            | (x, y) -> printNum board.[x, y])

        printfn ""
        board

[<EntryPoint>]
let main argv = 
    
    let board = Board.create 4

    let collapse = Board.collapse board

    let getInput _ = System.Console.ReadKey true
                     |> (fun keyInfo -> keyInfo.Key)
                     |> (function
                         | ConsoleKey.UpArrow -> Some Up
                         | ConsoleKey.RightArrow -> Some Right
                         | ConsoleKey.DownArrow -> Some Down
                         | ConsoleKey.LeftArrow -> Some Left
                         | _ -> None)

    let isGameOver: (int * Board * bool -> bool) = 
        function
        | (0, board, _) -> 
            [Left; Up; Right; Down]
            |> List.map (Board.collapse (Array2D.copy board))
            |> List.forall (fun (_, _, hasUpdated) -> not hasUpdated)
        | _ -> false

    let play _ : bool =
        ConsoleUi.print board |> ignore
        getInput ()
        |> (function 
            | None -> false
            | Some(dir) -> 
                collapse dir |> isGameOver)
    
    Seq.initInfinite (fun _ -> ())
    |> Seq.find play

    // print final board
    ConsoleUi.print board |> ignore
    printfn "Game over"

    0 // return an integer exit code