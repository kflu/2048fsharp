(*
TODOs:
- The double for loops can be replaced with sequence generators
- Use single case DU instead of type alias maybe?
- Design the Board type to a record to embed vacancy?
 *)

open System

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
