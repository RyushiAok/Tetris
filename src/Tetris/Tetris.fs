module Tetris.Game

open Tetris.Core
open System


type State = {
    tetrimino: Tetrimino
    hold: Tetrimino option
    board: TetrisBoard
    lastUpdated: DateTime
    isOver: bool
    score: int
}

type Msg =
    | Empty
    | Update
    | NewGame
    | Left
    | Right
    | Down
    | RotL
    | RotR
    | Hold

let generateMinos =
    let mutable queue = []
    let random = Random()

    let blocks =
        [| Shape.I; Shape.O; Shape.S; Shape.Z; Shape.J; Shape.L; Shape.T |]
        |> Array.map (Tetrimino.create (7, 2))

    fun reset ->
        if reset then
            queue <- []

        match queue with
        | [] ->
            blocks |> Array.sortInPlaceBy (fun _ -> random.Next(0, 100))
            queue <- Array.toList blocks[1 .. blocks.Length - 1]
            blocks[0]
        | h :: t ->
            queue <- t
            h

let init () = {
    tetrimino = generateMinos true
    hold = None
    board = TetrisBoard.init ()
    lastUpdated = DateTime.Now
    isOver = false
    score = 0
}


let update msg state =
    match msg with
    | Update ->
        if
            state.isOver
            || (DateTime.Now - state.lastUpdated).TotalMilliseconds < 500.0
        then
            state
        else
            let nxt = state.tetrimino |> Tetrimino.moveDown state.board

            if state.tetrimino <> nxt then
                {
                    state with
                        tetrimino = nxt
                        lastUpdated = DateTime.Now
                }
            else
                nxt
                |> Tetrimino.isHighLimitOver
                |> function
                    | true ->
                        let existsOtherBlock =
                            nxt |> Tetrimino.existsOtherBlock state.board

                        let res = state.board |> TetrisBoard.setTetrimino nxt

                        {
                            state with
                                isOver = true
                                board =
                                    if existsOtherBlock then
                                        state.board
                                    else
                                        res.newBoard
                        }
                    | false ->
                        let newMino = generateMinos false

                        let res = state.board |> TetrisBoard.setTetrimino nxt

                        {
                            state with
                                board = res.newBoard
                                tetrimino = newMino
                                lastUpdated = DateTime.Now
                                isOver =
                                    newMino |> Tetrimino.existsOtherBlock res.newBoard
                                score = state.score + res.eraced
                        }
    | RotL -> {
        state with
            tetrimino = state.tetrimino |> Tetrimino.rotateLeft state.board
            lastUpdated =
                if
                    state.tetrimino.shape = Shape.O
                    || state.tetrimino |> Tetrimino.rotateLeft state.board = state.tetrimino
                    || state.tetrimino
                       |> Tetrimino.rotateLeft state.board
                       |> fun mino -> { mino with y = mino.y + 1 }
                       |> Tetrimino.existsOtherBlock state.board
                       |> not
                then
                    state.lastUpdated
                else
                    DateTime.Now
      }
    | RotR -> {
        state with
            tetrimino = state.tetrimino |> Tetrimino.rotateRight state.board
            lastUpdated =
                if
                    state.tetrimino.shape = Shape.O
                    || state.tetrimino |> Tetrimino.rotateRight state.board = state.tetrimino
                    || state.tetrimino
                       |> Tetrimino.rotateRight state.board
                       |> fun mino -> { mino with y = mino.y + 1 }
                       |> Tetrimino.existsOtherBlock state.board
                       |> not
                then
                    state.lastUpdated
                else
                    DateTime.Now
      }
    | Left -> {
        state with
            tetrimino = state.tetrimino |> Tetrimino.moveLeft state.board
            lastUpdated =
                if
                    state.tetrimino.shape = Shape.O
                    || state.tetrimino |> Tetrimino.moveDown state.board
                       <> state.tetrimino
                    || state.tetrimino |> Tetrimino.moveLeft state.board = state.tetrimino
                then
                    state.lastUpdated
                else
                    DateTime.Now
      }
    | Right -> {
        state with
            tetrimino = state.tetrimino |> Tetrimino.moveRight state.board
            lastUpdated =
                if
                    state.tetrimino.shape = Shape.O
                    || state.tetrimino |> Tetrimino.moveDown state.board
                       <> state.tetrimino
                    || state.tetrimino |> Tetrimino.moveRight state.board = state.tetrimino
                then
                    state.lastUpdated
                else
                    DateTime.Now
      }
    | Down -> {
        state with
            tetrimino = state.tetrimino |> Tetrimino.moveDown state.board
      }
    | Hold ->
        if state.hold.IsSome then
            {
                state with
                    tetrimino = state.hold.Value
                    hold = Some(Tetrimino.create (7, 2) state.tetrimino.shape)
            }
        else
            {
                state with
                    tetrimino = generateMinos false
                    hold = Some(Tetrimino.create (7, 2) state.tetrimino.shape)
            }
    | NewGame -> init ()
    | _ -> state
