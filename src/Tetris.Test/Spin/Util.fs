module Spin.Util

open Tetris.Core

let cell2int =
    Map [
        Empty, 0
        Guard, 1
        Mino Shape.T, 2
        Mino Shape.I, 3
        Mino Shape.O, 4
        Mino Shape.S, 5
        Mino Shape.Z, 6
        Mino Shape.J, 7
        Mino Shape.L, 8
    ]

let int2cell =
    cell2int
    |> Map.toSeq
    |> Seq.map (fun (k, v) -> (v, k))
    |> Map.ofSeq



let createBoardMatrix (initBoard: int list list) =
    let board = TetrisBoard.init ()

    initBoard
    |> List.rev
    |> List.iteri (fun y row ->
        row
        |> List.iteri (fun x v -> board[21 - y, x + 3] <- int2cell[v]))

    board

let boardToIntMatrix (board: TetrisBoard) =
    board
    |> Array2D.map (fun v ->
        match v with
        | Empty -> 0
        | Guard -> 1
        | Mino _ -> 2)
