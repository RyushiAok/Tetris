module ``T-Spin Test``
// https://tetrisch.github.io/main/spins.html

open Xunit
open FsUnit.Xunit
open Tetris.Core

open Spin.Util


[<Fact>]
let ``T-Spin Mini 1`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 1; 1; 1; 1; 1; 1; 1; 1; 1 ]
        ]

    let t_mino =
        let x, y = 1, 2

        Tetrimino.create (x + 3, 21 - y) Shape.T
        |> Tetrimino.rotateRight board
        |> Tetrimino.rotateRight board
        |> Tetrimino.moveDown board
        |> Tetrimino.rotateRight board

    let actual =
        TetrisBoard.setTetrimino t_mino board
        |> fun res -> res.newBoard


    let expectedBoard =
        let t = cell2int[Mino Shape.T]

        createBoardMatrix [
            [ t; 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
            [ t; t; 0; 0; 0; 0; 0; 0; 0; 0 ]
        ]

    expectedBoard |> should equal actual


[<Fact>]
let ``T-Spin Mini 2`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 1; 1; 1; 1; 1; 1; 1; 1 ]
            [ 0; 0; 0; 1; 1; 1; 1; 1; 1; 1 ]
        ]

    let t_mino =
        let x, y = 1, 2

        Tetrimino.create (x + 3, 21 - y) Shape.T
        |> Tetrimino.rotateLeft board
        |> Tetrimino.moveLeft board
        |> Tetrimino.moveDown board
        |> Tetrimino.rotateLeft board

    let actual =
        TetrisBoard.setTetrimino t_mino board
        |> fun res -> res.newBoard


    let expectedBoard =
        let t = cell2int[Mino Shape.T]

        createBoardMatrix [
            [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
            [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
            [ 0; t; 1; 1; 1; 1; 1; 1; 1; 1 ]
        ]

    expectedBoard |> should equal actual


[<Fact>]
let ``T-Spin Double`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 1; 1; 1; 1; 0; 0; 1; 1; 1; 1 ]
            [ 1; 1; 1; 0; 0; 0; 1; 1; 1; 1 ]
            [ 1; 1; 1; 1; 0; 1; 1; 1; 1; 1 ]
        ]

    let t_mino =
        let x, y = 5, 3

        Tetrimino.create (x + 3, 21 - y) Shape.T
        |> Tetrimino.rotateRight board
        |> Tetrimino.moveDown board
        |> Tetrimino.rotateLeft board


    let actual =
        TetrisBoard.setTetrimino t_mino board
        |> fun res -> res.newBoard


    let expectedBoard =
        createBoardMatrix [
            [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
            [ 1; 1; 1; 1; 0; 0; 1; 1; 1; 1 ]
        ]

    expectedBoard |> should equal actual



[<Fact>]
let ``T-Spin NEO`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 1; 1; 1; 0; 0; 0; 0 ]
            [ 0; 0; 0; 0; 0; 1; 1; 1; 1; 1 ] // 3
            [ 0; 0; 0; 0; 1; 1; 1; 1; 1; 1 ]
            [ 1; 1; 0; 0; 1; 1; 1; 1; 1; 1 ]
            [ 1; 1; 1; 0; 1; 1; 1; 1; 1; 1 ]
        ]

    let t_mino =
        let x, y = 3, 3

        Tetrimino.create (x + 3, 21 - y) Shape.T
        |> Tetrimino.rotateRight board
        |> Tetrimino.moveDown board


    let actual =
        TetrisBoard.setTetrimino t_mino board
        |> fun res -> res.newBoard


    let expectedBoard =
        let t = cell2int[Mino Shape.T]

        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 1; 1; 1; 0; 0; 0; 0 ]
            [ 0; 0; 0; 0; 0; 1; 1; 1; 1; 1 ] // 3
            [ 0; 0; 0; t; 1; 1; 1; 1; 1; 1 ]
        ]

    expectedBoard |> should equal actual

[<Fact>]
let ``T-Spin FIN`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 1; 1; 1; 0; 0; 0; 0; 0 ]
            [ 0; 0; 0; 0; 1; 1; 1; 1; 1; 1 ] // 3
            [ 0; 0; 0; 0; 1; 1; 1; 1; 1; 1 ]
            [ 1; 1; 0; 0; 1; 1; 1; 1; 1; 1 ]
            [ 1; 1; 1; 0; 1; 1; 1; 1; 1; 1 ]
        ]

    let t_mino =
        let x, y = 2, 3

        Tetrimino.create (x + 3, 21 - y) Shape.T
        |> Tetrimino.rotateRight board
        |> Tetrimino.moveDown board


    let actual =
        TetrisBoard.setTetrimino t_mino board
        |> fun res -> res.newBoard


    let expectedBoard =
        let t = cell2int[Mino Shape.T]

        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 1; 1; 1; 0; 0; 0; 0; 0 ]
            [ 0; 0; 0; 0; 1; 1; 1; 1; 1; 1 ] // 3
            [ 0; 0; 0; t; 1; 1; 1; 1; 1; 1 ]
        ]

    let expectedBoard = boardToIntMatrix expectedBoard
    let actual = boardToIntMatrix actual
    expectedBoard |> should equal actual


[<Fact>]
let ``T-Spin ISO`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 1; 1; 1; 1; 0; 0; 0; 0 ]
            [ 0; 0; 0; 0; 0; 1; 1; 1; 1; 1 ] // 3
            [ 0; 0; 0; 0; 1; 1; 1; 1; 1; 1 ]
            [ 1; 1; 1; 0; 0; 1; 1; 1; 1; 1 ]
            [ 1; 1; 1; 0; 1; 1; 1; 1; 1; 1 ]
        ]

    let t_mino =
        let x, y = 3, 3

        Tetrimino.create (x + 3, 21 - y) Shape.T
        |> Tetrimino.rotateLeft board


    let actual =
        TetrisBoard.setTetrimino t_mino board
        |> fun res -> res.newBoard


    let expectedBoard =
        let t = cell2int[Mino Shape.T]

        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 1; 1; 1; 1; 0; 0; 0; 0 ]
            [ 0; 0; 0; 0; 0; 1; 1; 1; 1; 1 ] // 3
            [ 0; 0; 0; t; 1; 1; 1; 1; 1; 1 ]
        ]

    expectedBoard |> should equal actual
