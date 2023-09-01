namespace Tetris

open System
open Avalonia.FuncUI.DSL
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Controls.Primitives
open Avalonia.FuncUI.Helpers

type Board = {
    width: int
    height: int
    board: TetrisBoard
}

module Game =
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
                                        newMino
                                        |> Tetrimino.existsOtherBlock res.newBoard
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

    let shapeToColor shape =
        match shape with
        | Shape.I -> "#00ffff"
        | Shape.O -> "#ffd700"
        | Shape.J -> "#1e90ff"
        | Shape.L -> "#ffa500"
        | Shape.S -> "#00FF00"
        | Shape.Z -> "#ff0000"
        | Shape.T -> "#800080"

    let boardView state dispatch =
        let w, h = state.grid.width, state.grid.height

        let toColor state =
            let cells = state.grid.board

            let minoPos =
                state.tetrimino.pos
                |> Array.map (fun (x, y) ->
                    (x + state.tetrimino.x, y + state.tetrimino.y))

            let minoShape = state.tetrimino.shape

            [|
                for y in 3 .. (Array2D.length1 cells) - 1 - 1 do
                    for x in 2 .. (Array2D.length2 cells) - 1 - 2 do
                        if not state.isOver && minoPos |> Array.contains (x, y) then
                            yield shapeToColor minoShape
                        else
                            match cells[y, x] with
                            | Cell.Empty -> yield "#222222"
                            | Cell.Guard -> yield "#AAAAAA"
                            | Cell.Mino shape -> yield shapeToColor shape
            |]

        UniformGrid.create [
            UniformGrid.columns (w - 4)
            UniformGrid.rows (h - 4)
            UniformGrid.width 280.0
            UniformGrid.height 480.0
            UniformGrid.children (
                state
                |> toColor
                |> Array.map (fun (color: string) ->
                    Border.create [
                        Border.padding 0.8
                        Border.child (Border.create [ Border.background color ])
                    ]
                    |> generalize)
                |> Array.toList
            )
        ]

    let holdView (state: State) dispatch =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.dock Dock.Top
            StackPanel.top 0.0

            StackPanel.children [
                UniformGrid.create [
                    UniformGrid.columns 4
                    UniformGrid.rows 4
                    UniformGrid.width 70.0
                    UniformGrid.height 70.0
                    UniformGrid.left 360.0
                    UniformGrid.top 0.0
                    UniformGrid.children (
                        state.hold
                        |> Option.map (fun mino -> {
                            mino with
                                pos =
                                    mino.pos
                                    |> Array.map (fun (a, b) -> (a + 1, b + 1))
                        })
                        |> Option.map (fun mino -> [|
                            for y in 0..3 do
                                for x in 0..3 do
                                    if
                                        not state.isOver
                                        && mino.pos |> Array.contains (x, y)
                                    then
                                        yield shapeToColor mino.shape
                                    else
                                        yield "#222222"
                        |])
                        |> function
                            | None -> []
                            | Some a ->
                                a
                                |> Array.map (fun (color: string) ->
                                    Border.create [
                                        Border.padding 1.5
                                        Border.child (
                                            Border.create [ Border.background color ]
                                        )
                                    ]
                                    |> generalize)
                                |> Array.toList
                    )
                ]
            ]
        ]



    let menuView state dispatch =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.dock Dock.Top
            StackPanel.children [
                TextBlock.create [
                    TextBlock.fontSize 16.
                    TextBlock.horizontalAlignment HorizontalAlignment.Center
                    TextBlock.width 350.
                    TextBlock.text (sprintf "Score: %d " state.score)
                ]
            ]
        ]

    let howToPlayView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.dock Dock.Bottom
            StackPanel.children [
                TextBlock.create [
                    TextBlock.fontSize 12.
                    TextBlock.horizontalAlignment HorizontalAlignment.Center
                    TextBlock.width 350.
                    TextBlock.text
                        "[A] LEFT \n[D] RIGHT \n[SHIFT] ROT L \n[SPACE] ROT R \n[E] HOLD"
                ]
            ]
        ]

    let gameOverView state dispatch =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.horizontalAlignment HorizontalAlignment.Center
            StackPanel.verticalAlignment VerticalAlignment.Center
            StackPanel.children [
                TextBlock.create [
                    TextBlock.fontSize 16.
                    TextBlock.margin 4.
                    TextBlock.text "Game Over"
                ]
                Button.create [
                    Button.fontSize 16.
                    Button.margin 4.
                    Button.onClick (fun _ -> dispatch NewGame)
                    Button.content "New game"
                ]
            ]
        ]

    let view state dispatch =
        if state.isOver then
            gameOverView state dispatch |> generalize
        else
            DockPanel.create [
                DockPanel.background "#222222"
                DockPanel.lastChildFill true
                DockPanel.children [
                    menuView state dispatch
                    Border.create [
                        Control.dock Dock.Left
                        Border.borderThickness (20., 0., 0., 0.0)
                        Border.child (boardView state dispatch)
                    ]
                    Border.create [
                        Control.dock Dock.Right
                        Border.borderThickness (30.0, 0., 0., 0.0)
                        Border.child howToPlayView
                    ]

                    Border.create [
                        Control.dock Dock.Right
                        Border.borderThickness (20., 0., 0., 250.0)
                        Border.child (holdView state dispatch)
                    ]

                ]
            ]
            |> generalize
