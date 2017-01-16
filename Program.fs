(*
TODOs:
- The double for loops can be replaced with sequence generators
- Use single case DU instead of type alias maybe?
- Design the Board type to a record to embed vacancy?
 *)

open System
open Game

[<EntryPoint>]
let main argv = 
    
    let board = Game.create 4

    let collapse = Game.collapse board

    let rec getInput () = 
        System.Console.ReadKey true
        |> (fun keyInfo -> keyInfo.Key)
        |> (function
            | ConsoleKey.UpArrow -> Up
            | ConsoleKey.RightArrow -> Right
            | ConsoleKey.DownArrow -> Down
            | ConsoleKey.LeftArrow -> Left
            | _ -> getInput ())

    let play _ : bool =
        ConsoleUi.print board |> ignore
        getInput () |> collapse |> isGameOver
    
    // run until game over
    Seq.initInfinite id |> Seq.find play |> ignore

    // print final board
    ConsoleUi.print board |> ignore
    printfn "Game over"

    0 // return an integer exit code
