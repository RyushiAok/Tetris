module ``Tetrimino Rotation``

open Xunit
open FsUnit.Xunit

open Tetris.Core
open Tetris.Core.Tetrimino

module TestRotation =
    let rotRightAsixs mino =
        match mino.shape with
        | Shape.T ->
            match mino.pos.[3] with
            | (0, 1) -> [ mino.pos.[0]; mino.pos.[3]; (-1, 1); (-1, 2) ]
            | (-1, 0) -> [ mino.pos.[0]; mino.pos.[3] ]
            | (0, -1) -> [ mino.pos.[0]; mino.pos.[2] ]
            | _ -> [ mino.pos.[0]; mino.pos.[3] ]
        | Shape.S ->
            match mino.pos.[1] with
            | (0, -1) -> [ mino.pos.[0]; (0, 1) ]
            | _ -> [ (0, 0) ]
        | Shape.Z ->
            match mino.pos.[3] with
            | (1, 0) -> [ mino.pos.[0]; (-1, -1) ]
            | _ -> [ (0, 0) ]
        | Shape.L ->
            match mino.pos.[2] with
            | (0, 1) -> [ mino.pos.[0]; (-1, 0) ]
            | (0, -1) -> [ mino.pos.[0]; mino.pos.[1] ]
            | (1, 0) -> [ mino.pos.[0]; mino.pos.[2]; (-1, 1) ]
            | _ -> [ (0, 0) ]
        | Shape.J ->
            match mino.pos.[1] with
            | (0, 1) -> [ mino.pos.[0]; mino.pos.[1] ]
            | _ -> [ (0, 0) ]
        | Shape.I ->
            match mino.pos.[1] with
            | (-1, 0) -> [ mino.pos.[0]; mino.pos.[1] ]
            | (0, 1) -> [ mino.pos.[0]; mino.pos.[1] ]
            | _ -> [ (0, 0) ]
        | _ -> [ (0, 0) ]


    let rotLeftAsixs mino =
        match mino.shape with
        | Shape.T ->
            match mino.pos.[3] with
            | (0, 1) -> [ mino.pos.[0]; mino.pos.[3]; (1, 1); (1, 2) ]
            | (-1, 0) -> [ mino.pos.[0]; mino.pos.[2] ]
            | (0, -1) -> [ mino.pos.[0]; mino.pos.[1] ]
            | _ -> [ mino.pos.[0]; mino.pos.[3] ] // (1,0)
        | Shape.S ->
            match mino.pos.[1] with
            | (-1, 0) -> [ mino.pos.[0]; (1, 1) ]
            | _ -> [ (0, 0) ]
        | Shape.Z ->
            match mino.pos.[3] with
            | (0, 1) -> [ mino.pos.[0]; (1, 1) ]
            | _ -> [ (0, 0) ]
        | Shape.L ->
            match mino.pos.[2] with
            | (0, 1) -> [ mino.pos.[0]; mino.pos.[2] ]
            | _ -> [ (0, 0) ]
        | Shape.J ->
            match mino.pos.[1] with
            | (0, 1) -> [ mino.pos.[0]; (1, 0) ]
            | (0, -1) -> [ mino.pos.[0]; mino.pos.[2] ]
            | (-1, 0) -> [ mino.pos.[0]; mino.pos.[1]; (1, 1) ]
            | _ -> [ (0, 0) ]
        | Shape.I ->
            match mino.pos.[1] with
            | (1, 0) -> [ mino.pos.[0]; mino.pos.[1] ]
            | (0, 1) -> [ mino.pos.[0]; mino.pos.[1] ]
            | _ -> [ (0, 0) ]
        | _ -> [ (0, 0) ]


let rotRight mino = {
    mino with
        pos = [| for (dx, dy) in mino.pos -> (-dy, dx) |]
}

let rotLeft mino = {
    mino with
        pos = [| for (dx, dy) in mino.pos -> (dy, -dx) |]
}


module ``Rotate Right`` =

    [<Fact>]
    let ``rotateRight O`` () =

        let o_0 = Tetrimino.create (7, 2) Shape.O
        let o_90 = rotRight o_0
        let o_180 = rotRight o_90
        let o_270 = rotRight o_180

        (rightRotAsixs o_0)
        |> should equal (TestRotation.rotRightAsixs o_0)

        (rightRotAsixs o_90)
        |> should equal (TestRotation.rotRightAsixs o_90)

        (rightRotAsixs o_180)
        |> should equal (TestRotation.rotRightAsixs o_180)

        (rightRotAsixs o_270)
        |> should equal (TestRotation.rotRightAsixs o_270)



    [<Fact>]
    let ``rotateRight J`` () =

        let j_0 = Tetrimino.create (7, 2) Shape.J
        let j_90 = rotRight j_0
        let j_180 = rotRight j_90
        let j_270 = rotRight j_180

        (rightRotAsixs j_0)
        |> should equal (TestRotation.rotRightAsixs j_0)

        (rightRotAsixs j_90)
        |> should equal (TestRotation.rotRightAsixs j_90)

        (rightRotAsixs j_180)
        |> should equal (TestRotation.rotRightAsixs j_180)

        (rightRotAsixs j_270)
        |> should equal (TestRotation.rotRightAsixs j_270)


    [<Fact>]
    let ``rotateRight I`` () =

        let i_0 = Tetrimino.create (7, 2) Shape.I
        let i_90 = rotRight i_0
        let i_180 = rotRight i_90
        let i_270 = rotRight i_180

        (rightRotAsixs i_0)
        |> should equal (TestRotation.rotRightAsixs i_0)

        (rightRotAsixs i_90)
        |> should equal (TestRotation.rotRightAsixs i_90)

        (rightRotAsixs i_180)
        |> should equal (TestRotation.rotRightAsixs i_180)

        (rightRotAsixs i_270)
        |> should equal (TestRotation.rotRightAsixs i_270)



    [<Fact>]
    let ``rotateRight T`` () =

        let t_0 = Tetrimino.create (7, 2) Shape.T
        let t_90 = rotRight t_0
        let t_180 = rotRight t_90
        let t_270 = rotRight t_180

        (rightRotAsixs t_0)
        |> should equal (TestRotation.rotRightAsixs t_0)

        (rightRotAsixs t_90)
        |> should equal (TestRotation.rotRightAsixs t_90)

        (rightRotAsixs t_180)
        |> should equal (TestRotation.rotRightAsixs t_180)

        (rightRotAsixs t_270)
        |> should equal (TestRotation.rotRightAsixs t_270)


    [<Fact>]
    let ``rotateRight S`` () =

        let s_0 = Tetrimino.create (7, 2) Shape.S
        let s_90 = rotRight s_0
        let s_180 = rotRight s_90
        let s_270 = rotRight s_180

        (rightRotAsixs s_0)
        |> should equal (TestRotation.rotRightAsixs s_0)

        (rightRotAsixs s_90)
        |> should equal (TestRotation.rotRightAsixs s_90)

        (rightRotAsixs s_180)
        |> should equal (TestRotation.rotRightAsixs s_180)

        (rightRotAsixs s_270)
        |> should equal (TestRotation.rotRightAsixs s_270)


    [<Fact>]
    let ``rotateRight Z`` () =

        let z_0 = Tetrimino.create (7, 2) Shape.Z
        let z_90 = rotRight z_0
        let z_180 = rotRight z_90
        let z_270 = rotRight z_180

        (rightRotAsixs z_0)
        |> should equal (TestRotation.rotRightAsixs z_0)

        (rightRotAsixs z_90)
        |> should equal (TestRotation.rotRightAsixs z_90)

        (rightRotAsixs z_180)
        |> should equal (TestRotation.rotRightAsixs z_180)

        (rightRotAsixs z_270)
        |> should equal (TestRotation.rotRightAsixs z_270)

    [<Fact>]
    let ``rotateRight L`` () =

        let l_0 = Tetrimino.create (7, 2) Shape.L
        let l_90 = rotRight l_0
        let l_180 = rotRight l_90
        let l_270 = rotRight l_180

        (rightRotAsixs l_0)
        |> should equal (TestRotation.rotRightAsixs l_0)

        (rightRotAsixs l_90)
        |> should equal (TestRotation.rotRightAsixs l_90)

        (rightRotAsixs l_180)
        |> should equal (TestRotation.rotRightAsixs l_180)

        (rightRotAsixs l_270)
        |> should equal (TestRotation.rotRightAsixs l_270)

module ``Rotate Left`` =

    [<Fact>]
    let ``rotateLeft O`` () =

        let o_0 = Tetrimino.create (7, 2) Shape.O
        let o_90 = rotLeft o_0
        let o_180 = rotLeft o_90
        let o_270 = rotLeft o_180

        (leftRotAsixs o_0)
        |> should equal (TestRotation.rotLeftAsixs o_0)

        (leftRotAsixs o_90)
        |> should equal (TestRotation.rotLeftAsixs o_90)

        (leftRotAsixs o_180)
        |> should equal (TestRotation.rotLeftAsixs o_180)

        (leftRotAsixs o_270)
        |> should equal (TestRotation.rotLeftAsixs o_270)

    [<Fact>]
    let ``rotateLeft J`` () =

        let j_0 = Tetrimino.create (7, 2) Shape.J
        let j_90 = rotLeft j_0
        let j_180 = rotLeft j_90
        let j_270 = rotLeft j_180

        (leftRotAsixs j_0)
        |> should equal (TestRotation.rotLeftAsixs j_0)

        (leftRotAsixs j_90)
        |> should equal (TestRotation.rotLeftAsixs j_90)

        (leftRotAsixs j_180)
        |> should equal (TestRotation.rotLeftAsixs j_180)

        (leftRotAsixs j_270)
        |> should equal (TestRotation.rotLeftAsixs j_270)

    [<Fact>]
    let ``rotateLeft I`` () =

        let i_0 = Tetrimino.create (7, 2) Shape.I
        let i_90 = rotLeft i_0
        let i_180 = rotLeft i_90
        let i_270 = rotLeft i_180

        (leftRotAsixs i_0)
        |> should equal (TestRotation.rotLeftAsixs i_0)

        (leftRotAsixs i_90)
        |> should equal (TestRotation.rotLeftAsixs i_90)

        (leftRotAsixs i_180)
        |> should equal (TestRotation.rotLeftAsixs i_180)

        (leftRotAsixs i_270)
        |> should equal (TestRotation.rotLeftAsixs i_270)

    [<Fact>]
    let ``rotateLeft T`` () =

        let t_0 = Tetrimino.create (7, 2) Shape.T
        let t_90 = rotLeft t_0
        let t_180 = rotLeft t_90
        let t_270 = rotLeft t_180

        (leftRotAsixs t_0)
        |> should equal (TestRotation.rotLeftAsixs t_0)

        (leftRotAsixs t_90)
        |> should equal (TestRotation.rotLeftAsixs t_90)

        (leftRotAsixs t_180)
        |> should equal (TestRotation.rotLeftAsixs t_180)

        (leftRotAsixs t_270)
        |> should equal (TestRotation.rotLeftAsixs t_270)

    [<Fact>]
    let ``rotateLeft S`` () =

        let s_0 = Tetrimino.create (7, 2) Shape.S
        let s_90 = rotLeft s_0
        let s_180 = rotLeft s_90
        let s_270 = rotLeft s_180

        (leftRotAsixs s_0)
        |> should equal (TestRotation.rotLeftAsixs s_0)

        (leftRotAsixs s_90)
        |> should equal (TestRotation.rotLeftAsixs s_90)

        (leftRotAsixs s_180)
        |> should equal (TestRotation.rotLeftAsixs s_180)

        (leftRotAsixs s_270)
        |> should equal (TestRotation.rotLeftAsixs s_270)

    [<Fact>]
    let ``rotateLeft Z`` () =

        let z_0 = Tetrimino.create (7, 2) Shape.Z
        let z_90 = rotLeft z_0
        let z_180 = rotLeft z_90
        let z_270 = rotLeft z_180

        (leftRotAsixs z_0)
        |> should equal (TestRotation.rotLeftAsixs z_0)

        (leftRotAsixs z_90)
        |> should equal (TestRotation.rotLeftAsixs z_90)

        (leftRotAsixs z_180)
        |> should equal (TestRotation.rotLeftAsixs z_180)

        (leftRotAsixs z_270)
        |> should equal (TestRotation.rotLeftAsixs z_270)

    [<Fact>]
    let ``rotateLeft L`` () =

        let l_0 = Tetrimino.create (7, 2) Shape.L
        let l_90 = rotLeft l_0
        let l_180 = rotLeft l_90
        let l_270 = rotLeft l_180

        (leftRotAsixs l_0)
        |> should equal (TestRotation.rotLeftAsixs l_0)

        (leftRotAsixs l_90)
        |> should equal (TestRotation.rotLeftAsixs l_90)

        (leftRotAsixs l_180)
        |> should equal (TestRotation.rotLeftAsixs l_180)

        (leftRotAsixs l_270)
        |> should equal (TestRotation.rotLeftAsixs l_270)
