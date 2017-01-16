module Game

type Board = int [,]
type Direction = Up | Left | Down | Right
let swap (x1, y1) (x2, y2) (array: _[,]) = 
    let tmp = array.[x1, y1]
    array.[x1, y1] <- array.[x2, y2]
    array.[x2, y2] <- tmp
    array

let len1 = Array2D.length1
let len2 = Array2D.length2

let rotateCC board = 
    let transpose board = 
        for x = 0 to len2 board - 1 do
            for y = 0 to x do
                swap (x, y) (y, x) board |> ignore
        board

    let flip board =
        let nRow, nCol = len1 board, len2 board
        for x = 0 to nRow - 1 do
            for y = 0 to nCol / 2 - 1 do
                swap (x, y) (x, nRow - 1 - y) board |> ignore
        board

    board |> transpose |> flip

let collapseRowRight (board) (iRow: int) =
    (* Rules: 
        1. 0s don't take slots
        2. two same values collapse into 1 slot
        3. collapsed slot doesn't furter collapse

        TODO: this could also be written in a recursive way
        *)
    let mutable cur = len1 board - 1
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

    board
    
let getVacancy (board) =
    seq {
        for x in 0 .. len1 board - 1 do
        for y in 0 .. len2 board - 1 -> board.[x,y]
    }
    |> Seq.map (function | 0 -> 1 | _ -> 0)
    |> Seq.sum

let sprout (vacancy: int) (board) = 
    let slots = vacancy
    assert (slots > 0) // The game should be over if there's no vacancy

    let mutable pos = System.Random().Next slots
    let mutable sprouted = false
    for x = 0 to len1 board - 1 do
        for y = 0 to len2 board - 1 do
            if board.[x, y] = 0 then do
                if pos = 0 then do 
                    // FIXME: I want to break after this is met. F# doesn't have break
                    // so I'm not writing it F#-esque.
                    board.[x, y] <- 2
                    sprouted <- true
                    pos <- pos - 1
                elif pos > 0 then pos <- pos - 1

    assert sprouted
    board

let collapseRight (board) =
    seq { 0 .. len1 board - 1 } 
    |> Seq.iter (collapseRowRight board >> ignore)
    board


let equals (b1: int[,]) (b2: int[,]) : bool =
    (len1 b1 = len1 b2) &&
    (len2 b1 = len2 b2) &&
    (Seq.zip (b1 |> Seq.cast) (b2 |> Seq.cast)
    |> Seq.map (fun (x, y) -> x - y)
    |> Seq.sum) = 0

let collapse board direction = 
    let old = Array2D.copy board
    let board =
        match direction with 
        | Right -> board |> collapseRight
        | Down  -> board |> rotateCC |> rotateCC |> rotateCC |> collapseRight |> rotateCC
        | Left  -> board |> rotateCC |> rotateCC |> collapseRight |> rotateCC |> rotateCC
        | Up    -> board |> rotateCC |> collapseRight |> rotateCC |> rotateCC |> rotateCC
    
    match equals old board with
    | false -> 
        let vacancy = getVacancy board
        sprout vacancy board
    | true -> board

let create (size: int) =
    Array2D.zeroCreate size size |> sprout (size * size)

let isGameOver (board) =
    match getVacancy (board) with
    | 0 -> 
        [Left; Up; Right; Down]
        |> List.map (board |> Array2D.copy |> collapse)
        |> List.forall (equals board)
    | _ -> false