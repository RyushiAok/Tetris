module Spin.Util

open Tetris.Core
open System.Text

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
        |> List.iteri (fun x v -> board[config.height - 4 - y, x + 3] <- int2cell[v]))

    board

let board2String (board: TetrisBoard) =
    board
    |> Array2D.map (fun v ->
        match v with
        | Empty -> " "
        | Guard -> "■"
        | Mino _ -> "×")
    |> fun res ->
        let s = StringBuilder()

        for y in 0 .. (Array2D.length1 res - 1) do
            for x in 0 .. Array2D.length2 res - 1 do
                s.Append(res[y, x]) |> ignore

            s.AppendLine() |> ignore

        s.ToString()
