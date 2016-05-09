module ConsoleUi
open System
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

let print (Board board) : Board =
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
    Board board