module Board 
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
