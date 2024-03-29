module ``SZ-Spin Test``
// https://tetrisch.github.io/main/spins.html

open Xunit
open FsUnit.Xunit
open Tetris.Core

open Spin.Util


[<Fact>]
let ``S-Spin Double 1`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ] //2
            [ 1; 1; 1; 1; 0; 0; 1; 1; 1; 1 ]
            [ 1; 1; 1; 0; 0; 1; 1; 1; 1; 1 ]
        ]

    let mino =
        let x, y = 4, 2

        Tetrimino.create (x + 3, config.height - 4 - y) Shape.S
        |> Tetrimino.rotateRight board
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
let ``Z-Spin Double 1`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ] //2
            [ 1; 1; 1; 1; 0; 0; 1; 1; 1; 1 ]
            [ 1; 1; 1; 1; 1; 0; 0; 1; 1; 1 ]
        ]

    let mino =
        let x, y = 5, 2

        Tetrimino.create (x + 3, config.height - 4 - y) Shape.Z
        |> Tetrimino.rotateLeft board
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
let ``S-Spin Double 2`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ] //3
            [ 1; 1; 1; 1; 0; 0; 1; 1; 1; 1 ]
            [ 1; 1; 1; 1; 0; 0; 1; 1; 1; 1 ]
            [ 1; 1; 1; 0; 0; 1; 1; 1; 1; 1 ]
        ]

    let mino =
        let x, y = 5, 3

        Tetrimino.create (x + 3, config.height - 4 - y) Shape.S
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
            [ 1; 1; 1; 1; 0; 0; 1; 1; 1; 1 ]
        ]

    actual |> should equal expectedBoard


[<Fact>]
let ``Z-Spin Double 2`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ] //3
            [ 1; 1; 1; 1; 0; 0; 1; 1; 1; 1 ]
            [ 1; 1; 1; 1; 0; 0; 1; 1; 1; 1 ]
            [ 1; 1; 1; 1; 1; 0; 0; 1; 1; 1 ]
        ]

    let mino =
        let x, y = 4, 3

        Tetrimino.create (x + 3, config.height - 4 - y) Shape.Z
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
            [ 1; 1; 1; 1; 0; 0; 1; 1; 1; 1 ]
        ]

    actual |> should equal expectedBoard

[<Fact>]
let ``S-Spin Triple`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 1; 0; 0; 0 ] //3
            [ 1; 1; 1; 1; 0; 1; 1; 1; 1; 1 ]
            [ 1; 1; 1; 1; 0; 0; 1; 1; 1; 1 ]
            [ 1; 1; 1; 1; 1; 0; 1; 1; 1; 1 ]
        ]

    let mino =
        let x, y = 5, 3

        Tetrimino.create (x + 3, config.height - 4 - y) Shape.S
        |> Tetrimino.rotateLeft board

    let actual =
        TetrisBoard.setTetrimino mino board
        |> fun res -> res.newBoard


    let expectedBoard =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 0; 0; 0; 1; 0; 0; 0 ] //3
        ]

    actual |> should equal expectedBoard


[<Fact>]
let ``Z-Spin Triple`` () =
    let board =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 1; 0; 0; 0; 0; 0; 0 ] // 3
            [ 1; 1; 1; 1; 1; 0; 1; 1; 1; 1 ]
            [ 1; 1; 1; 1; 0; 0; 1; 1; 1; 1 ]
            [ 1; 1; 1; 1; 0; 1; 1; 1; 1; 1 ]
        ]

    let mino =
        let x, y = 4, 3

        Tetrimino.create (x + 3, config.height - 4 - y) Shape.Z
        |> Tetrimino.rotateRight board

    let actual =
        TetrisBoard.setTetrimino mino board
        |> fun res -> res.newBoard


    let expectedBoard =
        createBoardMatrix [
            //0  1  2  3  4  5  6  7  8  9
            [ 0; 0; 0; 1; 0; 0; 0; 0; 0; 0 ] //3
        ]

    actual |> should equal expectedBoard
