module ``LJ-Spin Test``
// https://tetrisch.github.io/main/spins.html

open Xunit
open FsUnit.Xunit
open Tetris.Core

open Spin.Util


[<Fact>]
let ``L-Spin Double 1`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ] //2
            [ 1; 1; 1; 1; 1; 0; 1; 1; 1; 1 ]
            [ 1; 1; 1; 0; 0; 0; 1; 1; 1; 1 ]
        ]

    let mino =
        let x, y = 5, 2

        Tetrimino.create (x + 3, config.height - 4 - y) Shape.L
        |> Tetrimino.rotateLeft board
        |> Tetrimino.moveDown board
        |> Tetrimino.rotateRight board

    let actual =
        TetrisBoard.setTetrimino mino board
        |> fun res -> res.newBoard


    let expectedBoard =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ] //2
        ]

    actual |> should equal expectedBoard


[<Fact>]
let ``L-Spin Double 2`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ] //3
            [ 1; 1; 1; 1; 1; 0; 1; 1; 1; 1 ]
            [ 1; 1; 1; 0; 0; 0; 1; 1; 1; 1 ]
            [ 1; 1; 1; 0; 1; 1; 1; 1; 1; 1 ]
        ]

    let mino =
        let x, y = 5, 3

        Tetrimino.create (x + 3, config.height - 4 - y) Shape.L
        |> Tetrimino.rotateLeft board
        |> Tetrimino.moveDown board
        |> Tetrimino.rotateLeft board

    let actual =
        TetrisBoard.setTetrimino mino board
        |> fun res -> res.newBoard


    let expectedBoard =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ] //3
            [ 1; 1; 1; 1; 1; 0; 1; 1; 1; 1 ]
        ]

    actual |> should equal expectedBoard


[<Fact>]
let ``L-Spin Double 3`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 1; 1; 1; 1; 1 ] // 2
            [ 1; 1; 1; 0; 0; 0; 1; 1; 1; 1 ]
            [ 1; 1; 1; 0; 1; 1; 1; 1; 1; 1 ]
        ]

    let mino =
        let x, y = 3, 2

        Tetrimino.create (x + 3, config.height - 4 - y) Shape.L
        |> Tetrimino.rotateLeft board
        |> Tetrimino.moveDown board
        |> Tetrimino.rotateLeft board

    let actual =
        TetrisBoard.setTetrimino mino board
        |> fun res -> res.newBoard


    let expectedBoard =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 1; 1; 1; 1; 1 ] // 2
        ]

    actual |> should equal expectedBoard



[<Fact>]
let ``L-Spin Triple`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 1; 1; 1; 1; 0; 0; 0; 0; 0; 0 ]
            [ 1; 1; 1; 0; 0; 0; 0; 0; 0; 0 ] // 3
            [ 1; 1; 1; 0; 1; 1; 1; 1; 1; 1 ]
            [ 1; 1; 1; 0; 1; 1; 1; 1; 1; 1 ]
            [ 1; 1; 1; 0; 0; 1; 1; 1; 1; 1 ]
        ]

    let mino =
        let x, y = 4, 3

        Tetrimino.create (x + 3, config.height - 4 - y) Shape.L
        |> Tetrimino.rotateRight board

    let actual =
        TetrisBoard.setTetrimino mino board
        |> fun res -> res.newBoard


    let expectedBoard =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 1; 1; 1; 1; 0; 0; 0; 0; 0; 0 ]
            [ 1; 1; 1; 0; 0; 0; 0; 0; 0; 0 ] // 3
        ]

    actual |> should equal expectedBoard


[<Fact>]
let ``J-Spin Double 1`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ] //2
            [ 1; 1; 1; 1; 0; 1; 1; 1; 1; 1 ]
            [ 1; 1; 1; 1; 0; 0; 0; 1; 1; 1 ]
        ]

    let mino =
        let x, y = 4, 2

        Tetrimino.create (x + 3, config.height - 4 - y) Shape.J
        |> Tetrimino.rotateRight board
        |> Tetrimino.moveDown board
        |> Tetrimino.rotateLeft board

    let actual =
        TetrisBoard.setTetrimino mino board
        |> fun res -> res.newBoard


    let expectedBoard =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ] //2
        ]

    actual |> should equal expectedBoard


[<Fact>]
let ``J-Spin Double 2`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ] //3
            [ 1; 1; 1; 1; 0; 1; 1; 1; 1; 1 ]
            [ 1; 1; 1; 1; 0; 0; 0; 1; 1; 1 ]
            [ 1; 1; 1; 1; 1; 1; 0; 1; 1; 1 ]
        ]

    let mino =
        let x, y = 4, 3

        Tetrimino.create (x + 3, config.height - 4 - y) Shape.J
        |> Tetrimino.rotateRight board
        |> Tetrimino.moveDown board
        |> Tetrimino.rotateRight board

    let actual =
        TetrisBoard.setTetrimino mino board
        |> fun res -> res.newBoard


    let expectedBoard =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ] //3
            [ 1; 1; 1; 1; 0; 1; 1; 1; 1; 1 ]
        ]

    actual |> should equal expectedBoard


[<Fact>]
let ``J-Spin Double 3`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 1; 1; 1; 1; 1; 0; 0; 0; 0; 0 ] // 2
            [ 1; 1; 1; 1; 0; 0; 0; 1; 1; 1 ]
            [ 1; 1; 1; 1; 1; 1; 0; 1; 1; 1 ]
        ]

    let mino =
        let x, y = 6, 2

        Tetrimino.create (x + 3, config.height - 4 - y) Shape.J
        |> Tetrimino.rotateLeft board
        |> Tetrimino.moveDown board
        |> Tetrimino.rotateLeft board

    let actual =
        TetrisBoard.setTetrimino mino board
        |> fun res -> res.newBoard


    let expectedBoard =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 1; 1; 1; 1; 1; 0; 0; 0; 0; 0 ] // 2
        ]

    actual |> should equal expectedBoard



[<Fact>]
let ``J-Spin Triple`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 1; 1; 1; 1 ]
            [ 0; 0; 0; 0; 0; 0; 0; 1; 1; 1 ] // 3
            [ 1; 1; 1; 1; 1; 1; 0; 1; 1; 1 ]
            [ 1; 1; 1; 1; 1; 1; 0; 1; 1; 1 ]
            [ 1; 1; 1; 1; 1; 0; 0; 1; 1; 1 ]
        ]

    let mino =
        let x, y = 6, 3

        Tetrimino.create (x + 3, config.height - 4 - y) Shape.J
        |> Tetrimino.rotateLeft board

    let actual =
        TetrisBoard.setTetrimino mino board
        |> fun res -> res.newBoard


    let expectedBoard =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 1; 1; 1; 1 ]
            [ 0; 0; 0; 0; 0; 0; 0; 1; 1; 1 ] // 3
        ]

    actual |> should equal expectedBoard
