module ``I-Spin Test``
// https://tetrisch.github.io/main/spins.html

open Xunit
open FsUnit.Xunit
open Tetris.Core

open Spin.Util


[<Fact>]
let ``I-Spin 1`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 1; 1; 1; 1; 0; 0; 0; 0; 0; 0 ]
            [ 1; 1; 1; 0; 0; 0; 0; 0; 0; 0 ] // 3
            [ 1; 1; 1; 0; 1; 1; 1; 1; 1; 1 ]
            [ 1; 1; 1; 0; 1; 1; 1; 1; 1; 1 ]
            [ 1; 1; 1; 0; 1; 1; 1; 1; 1; 1 ]
        ]

    let mino =
        let x, y = 6, 3

        Tetrimino.create (x + 3, config.height - 4 - y) Shape.I
        |> Tetrimino.moveLeft board
        |> Tetrimino.rotateRight board

    let actual =
        TetrisBoard.setTetrimino mino board
        |> fun res -> res.newBoard


    let expectedBoard =
        let i = cell2int[Mino Shape.I]

        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 1; 1; 1; 1; 0; 0; 0; 0; 0; 0 ]
            [ 1; 1; 1; i; 0; 0; 0; 0; 0; 0 ] // 3
        ]

    actual |> should equal expectedBoard


[<Fact>]
let ``I-Spin 2`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 1; 1; 1; 1 ]
            [ 0; 0; 0; 0; 0; 0; 0; 1; 1; 1 ] // 3
            [ 1; 1; 1; 1; 1; 1; 0; 1; 1; 1 ]
            [ 1; 1; 1; 1; 1; 1; 0; 1; 1; 1 ]
            [ 1; 1; 1; 1; 1; 1; 0; 1; 1; 1 ]
        ]

    let mino =
        let x, y = 4, 3

        Tetrimino.create (x + 3, config.height - 4 - y) Shape.I
        |> Tetrimino.rotateRight board
        |> Tetrimino.rotateRight board
        |> Tetrimino.rotateLeft board

    let actual =
        TetrisBoard.setTetrimino mino board
        |> fun res -> res.newBoard


    let expectedBoard =
        let i = cell2int[Mino Shape.I]

        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 1; 1; 1; 1 ]
            [ 0; 0; 0; 0; 0; 0; i; 1; 1; 1 ] // 3
        ]

    actual |> should equal expectedBoard



[<Fact>]
let ``I-Spin 3`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ] // 4
            [ 1; 1; 1; 0; 0; 0; 0; 0; 0; 0 ]
            [ 1; 1; 1; 0; 0; 0; 0; 1; 1; 1 ] // 2
            [ 1; 1; 1; 0; 1; 1; 1; 1; 1; 1 ]
            [ 1; 1; 1; 0; 0; 0; 0; 1; 1; 1 ]
        ]

    let mino =
        let x, y = 3, 4

        Tetrimino.create (x + 3, config.height - 4 - y) Shape.I
        |> Tetrimino.rotateRight board
        |> Tetrimino.moveDown board
        |> Tetrimino.moveDown board
        |> Tetrimino.moveDown board
        |> Tetrimino.rotateRight board

    let actual =
        TetrisBoard.setTetrimino mino board
        |> fun res -> res.newBoard


    let expectedBoard =

        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 1; 1; 1; 0; 0; 0; 0; 0; 0; 0 ] // 3
            [ 1; 1; 1; 0; 0; 0; 0; 1; 1; 1 ]
            [ 1; 1; 1; 0; 1; 1; 1; 1; 1; 1 ]
        ]

    actual |> should equal expectedBoard



[<Fact>]
let ``I-Spin 4`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ] // 4
            [ 0; 0; 0; 0; 0; 0; 0; 1; 1; 1 ]
            [ 1; 1; 1; 0; 0; 0; 0; 1; 1; 1 ] // 2
            [ 1; 1; 1; 1; 1; 1; 0; 1; 1; 1 ]
            [ 1; 1; 1; 0; 0; 0; 0; 1; 1; 1 ]
        ]

    let mino =
        let x, y = 6, 4

        Tetrimino.create (x + 3, config.height - 4 - y) Shape.I
        |> Tetrimino.rotateLeft board
        |> Tetrimino.moveDown board
        |> Tetrimino.moveDown board
        |> Tetrimino.moveDown board
        |> Tetrimino.rotateLeft board

    let actual =
        TetrisBoard.setTetrimino mino board
        |> fun res -> res.newBoard


    let expectedBoard =

        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ] // 4
            [ 0; 0; 0; 0; 0; 0; 0; 1; 1; 1 ]
            [ 1; 1; 1; 0; 0; 0; 0; 1; 1; 1 ] // 2
            [ 1; 1; 1; 1; 1; 1; 0; 1; 1; 1 ]
        ]

    actual |> should equal expectedBoard
