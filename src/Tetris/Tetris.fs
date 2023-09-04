module Tetris.Game

open Tetris.Core
open System

type Board = {
    width: int
    height: int
    board: TetrisBoard
}

type State = {
    tetrimino: Tetrimino
    hold: Tetrimino option
    grid: Board
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

let init () = {
    tetrimino = Tetrimino.generate true
    hold = None
    grid = {
        width = 16
        height = 24
        board = TetrisBoard.init
    }
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
            let nxt = state.tetrimino |> Tetrimino.moveDown state.grid.board

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
                            nxt |> Tetrimino.existsOtherBlock state.grid.board

                        let res = state.grid.board |> TetrisBoard.setTetrimino nxt

                        {
                            state with
                                isOver = true
                                grid = {
                                    state.grid with
                                        board =
                                            if existsOtherBlock then
                                                state.grid.board
                                            else
                                                res.newBoard
                                }
                        }
                    | false ->
                        let newMino = Tetrimino.generate false

                        let res = state.grid.board |> TetrisBoard.setTetrimino nxt

                        {
                            state with
                                grid = { state.grid with board = res.newBoard }
                                tetrimino = newMino
                                lastUpdated = DateTime.Now
                                isOver =
                                    newMino |> Tetrimino.existsOtherBlock res.newBoard
                                score = state.score + res.eraced
                        }
    | RotL -> {
        state with
            tetrimino = state.tetrimino |> Tetrimino.rotateLeft state.grid.board
            lastUpdated =
                if
                    state.tetrimino.shape = Shape.O
                    || state.tetrimino |> Tetrimino.rotateLeft state.grid.board = state.tetrimino
                    || state.tetrimino
                       |> Tetrimino.rotateLeft state.grid.board
                       |> fun mino -> { mino with y = mino.y + 1 }
                       |> Tetrimino.existsOtherBlock state.grid.board
                       |> not
                then
                    state.lastUpdated
                else
                    DateTime.Now
      }
    | RotR -> {
        state with
            tetrimino = state.tetrimino |> Tetrimino.rotateRight state.grid.board
            lastUpdated =
                if
                    state.tetrimino.shape = Shape.O
                    || state.tetrimino |> Tetrimino.rotateRight state.grid.board = state.tetrimino
                    || state.tetrimino
                       |> Tetrimino.rotateRight state.grid.board
                       |> fun mino -> { mino with y = mino.y + 1 }
                       |> Tetrimino.existsOtherBlock state.grid.board
                       |> not
                then
                    state.lastUpdated
                else
                    DateTime.Now
      }
    | Left -> {
        state with
            tetrimino = state.tetrimino |> Tetrimino.moveLeft state.grid.board
            lastUpdated =
                if
                    state.tetrimino.shape = Shape.O
                    || state.tetrimino |> Tetrimino.moveDown state.grid.board
                       <> state.tetrimino
                    || state.tetrimino |> Tetrimino.moveLeft state.grid.board = state.tetrimino
                then
                    state.lastUpdated
                else
                    DateTime.Now
      }
    | Right -> {
        state with
            tetrimino = state.tetrimino |> Tetrimino.moveRight state.grid.board
            lastUpdated =
                if
                    state.tetrimino.shape = Shape.O
                    || state.tetrimino |> Tetrimino.moveDown state.grid.board
                       <> state.tetrimino
                    || state.tetrimino |> Tetrimino.moveRight state.grid.board = state.tetrimino
                then
                    state.lastUpdated
                else
                    DateTime.Now
      }
    | Down -> {
        state with
            tetrimino = state.tetrimino |> Tetrimino.moveDown state.grid.board
      }
    | Hold ->
        if state.hold.IsSome then
            {
                state with
                    tetrimino = state.hold.Value
                    hold = Some(Tetrimino.initMino state.tetrimino.shape)
            }
        else
            {
                state with
                    tetrimino = Tetrimino.generate false
                    hold = Some(Tetrimino.initMino state.tetrimino.shape)
            }
    | NewGame -> init ()
    | _ -> state
