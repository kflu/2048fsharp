module Board
val getVacancy : Types.Board -> int * Types.Board
val collapse :
  board:Types.Board -> direction:Types.Direction -> int * Types.Board * bool
val create : size:int -> Types.Board
