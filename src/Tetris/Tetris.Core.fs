module Tetris.Core

[<RequireQualifiedAccess>]
type Shape =
    | I
    | O
    | S
    | Z
    | J
    | L
    | T

type Cell =
    | Empty
    | Guard
    | Mino of Shape

type TetrisBoard = Cell[,]

[<RequireQualifiedAccess>]
type Theta =
    | ``0``
    | ``90``
    | ``180``
    | ``270``

module private Vector =
    /// rotate -90deg
    let inline rotR (x, y) = (-y, x)

    /// rotate 90deg
    let inline rotL (x, y) = (y, -x)

    let inline add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

type Position = { XYs: (int * int)[]; theta: Theta }

module private Position =
    let private rotateRightTheta =
        function
        | Theta.``0`` -> Theta.``270``
        | Theta.``90`` -> Theta.``0``
        | Theta.``180`` -> Theta.``90``
        | Theta.``270`` -> Theta.``180``

    let private rotateLeftTheta =
        function
        | Theta.``0`` -> Theta.``90``
        | Theta.``90`` -> Theta.``180``
        | Theta.``180`` -> Theta.``270``
        | Theta.``270`` -> Theta.``0``

    let rotR position = {
        theta = rotateRightTheta position.theta
        XYs = position.XYs |> Array.map Vector.rotR
    }

    let rotL position = {
        theta = rotateLeftTheta position.theta
        XYs = position.XYs |> Array.map Vector.rotL
    }

type Tetrimino = {
    x: int
    y: int
    pos: Position
    shape: Shape
}

let config = {|
    width = 16
    height = 26
    heightLimit = 2
|}

module TetrisBoard =
    let init () =
        Array2D.init config.height config.width (fun y x ->
            if
                (3 <= x
                 && x <= config.width - 4
                 && 0 <= y
                 && y <= config.height - 4)
            then
                Empty
            else
                Guard)

    let isFilled (board: TetrisBoard) (x, y) = board[y, x] <> Empty

    let setTetrimino mino board =
        let nxt =
            let nxt = board |> Array2D.copy

            mino.pos.XYs
            |> Array.iter (fun (dx, dy) ->
                nxt[mino.y + dy, mino.x + dx] <- Mino mino.shape)

            nxt

        let mutable eraced = 0

        let dif =
            List.rev [
                yield! [ config.height - 1 .. - 1 .. config.height - 3 ]
                for y in config.height - 4 .. -1 .. 0 do
                    let isLineFilled =
                        [ 3 .. config.width - 4 ]
                        |> List.forall (fun x -> isFilled nxt (x, y))

                    if isLineFilled then
                        for x in 3 .. config.width - 4 do
                            nxt[y, x] <- Empty

                        yield y
                        eraced <- eraced + 1
                    else
                        yield y + eraced
            ]

        for y in config.height - 4 .. -1 .. 0 do
            for x in 3 .. config.width - 4 do
                nxt[dif[y], x] <- nxt[y, x]

        {| newBoard = nxt; eraced = eraced |}


module Tetrimino =

    let create (x, y) =
        function
        | Shape.O -> {
            x = x
            y = y
            shape = Shape.O
            // 2 3
            // 0 1
            pos = {
                theta = Theta.``0``
                XYs = [| (0, 0); (1, 0); (0, -1); (1, -1) |]
            }
          }
        | Shape.T -> {
            x = x
            y = y
            shape = Shape.T
            // 1 0 2
            //   3
            pos = {
                theta = Theta.``0``
                XYs = [| (0, 0); (-1, 0); (1, 0); (0, 1) |]
            }
          }
        | Shape.S -> {
            x = x
            y = y
            shape = Shape.S
            //   2 3
            // 1 0
            pos = {
                theta = Theta.``0``
                XYs = [| (0, 0); (-1, 0); (0, -1); (1, -1) |]
            }
          }
        | Shape.Z -> {
            x = x
            y = y
            shape = Shape.Z
            //  1 2
            //    0 3
            pos = {
                theta = Theta.``0``
                XYs = [| (0, 0); (-1, -1); (0, -1); (1, 0) |]
            }
          }
        | Shape.L -> {
            x = x
            y = y
            shape = Shape.L
            //     3
            // 2 0 1
            pos = {
                theta = Theta.``0``
                XYs = [| (0, 0); (1, 0); (-1, 0); (1, -1) |]
            }
          }
        | Shape.J -> {
            x = x
            y = y
            shape = Shape.J
            // 3
            // 2 0 1
            pos = {
                theta = Theta.``0``
                XYs = [| (0, 0); (1, 0); (-1, 0); (-1, -1) |]
            }
          }
        | Shape.I -> {
            x = x
            y = y
            shape = Shape.I
            // 3 2 0 1
            pos = {
                theta = Theta.``0``
                XYs = [| (0, 0); (1, 0); (-1, 0); (-2, 0) |]
            }
          }

    type private RotateDirection =
        | RotR
        | RotL

    let private rotAsixs direction mino =
        match direction with
        | RotR ->
            match mino.shape with
            | Shape.O -> [ (0, 0) ]
            | Shape.T ->
                match mino.pos.theta with
                | Theta.``0`` -> [ mino.pos.XYs[0]; mino.pos.XYs[3]; (-1, 1); (-1, 2) ]
                | Theta.``90`` -> [ mino.pos.XYs[0]; mino.pos.XYs[3] ]
                | Theta.``180`` -> [ mino.pos.XYs[0]; mino.pos.XYs[2] ]
                | Theta.``270`` -> [ mino.pos.XYs[0]; mino.pos.XYs[3] ]
            | Shape.S ->
                match mino.pos.theta with
                | Theta.``270`` -> [ mino.pos.XYs[0]; (0, 1) ]
                | _ -> [ (0, 0) ]
            | Shape.Z ->
                match mino.pos.theta with
                | Theta.``0`` -> [ mino.pos.XYs[0]; (-1, 1) ]
                | Theta.``270`` -> [ mino.pos.XYs[0]; mino.pos.XYs[3] ]
                | _ -> [ (0, 0) ]
            | Shape.L ->
                match mino.pos.theta with
                | Theta.``0`` -> [ mino.pos.XYs[0]; mino.pos.XYs[2] ]
                | Theta.``90`` -> [ mino.pos.XYs[0]; (-1, 0) ]
                | Theta.``180`` -> [ mino.pos.XYs[0]; mino.pos.XYs[2]; (-1, 1) ]
                | Theta.``270`` -> [ mino.pos.XYs[0]; mino.pos.XYs[1] ]
            | Shape.J ->
                match mino.pos.theta with
                | Theta.``270`` -> [ mino.pos.XYs[0]; mino.pos.XYs[1] ]
                | _ -> [ (0, 0) ]
            | Shape.I ->
                match mino.pos.theta with
                | Theta.``0`` -> [ mino.pos.XYs[0]; mino.pos.XYs[3] ]
                | Theta.``270`` -> [ mino.pos.XYs[0]; mino.pos.XYs[1] ]
                | _ -> [ (0, 0) ]
        | RotL ->
            match mino.shape with
            | Shape.O -> [ (0, 0) ]
            | Shape.T ->
                match mino.pos.theta with
                | Theta.``0`` -> [ mino.pos.XYs[0]; mino.pos.XYs[3]; (1, 1); (1, 2) ]
                | Theta.``90`` -> [ mino.pos.XYs[0]; mino.pos.XYs[3] ]
                | Theta.``180`` -> [ mino.pos.XYs[0]; mino.pos.XYs[1] ]
                | Theta.``270`` -> [ mino.pos.XYs[0]; mino.pos.XYs[2] ]
            | Shape.S ->
                match mino.pos.theta with
                | Theta.``0`` -> [ mino.pos.XYs[0]; (1, 1) ]
                | Theta.``90`` -> [ mino.pos.XYs[0]; mino.pos.XYs[1] ]
                | _ -> [ (0, 0) ]
            | Shape.Z ->
                match mino.pos.theta with
                | Theta.``90`` -> [ mino.pos.XYs[0]; (0, 1) ]
                | _ -> [ (0, 0) ]
            | Shape.L ->
                match mino.pos.theta with
                | Theta.``90`` -> [ mino.pos.XYs[0]; mino.pos.XYs[2] ]
                | _ -> [ (0, 0) ]
            | Shape.J ->
                match mino.pos.theta with
                | Theta.``0`` -> [ (0, 0) ]
                | Theta.``90`` -> [ mino.pos.XYs[0]; mino.pos.XYs[2] ]
                | Theta.``180`` -> [ mino.pos.XYs[0]; mino.pos.XYs[1]; (1, 1) ]
                | Theta.``270`` -> [ mino.pos.XYs[0]; (1, 0) ]
            | Shape.I ->
                match mino.pos.theta with
                | Theta.``90`` -> [ mino.pos.XYs[0]; mino.pos.XYs[3] ]
                | Theta.``180`` -> [ mino.pos.XYs[0]; mino.pos.XYs[3] ]
                | _ -> [ (0, 0) ]

    let existsOtherBlock board mino =
        mino.pos.XYs
        |> Array.exists (Vector.add (mino.x, mino.y) >> TetrisBoard.isFilled board)

    let private rotate direction board mino =
        match mino.shape with
        | Shape.O -> mino
        | _ ->
            let pos, rAsixs, rot =
                match direction with
                | RotL -> Position.rotL mino.pos, rotAsixs direction mino, Vector.rotR
                | RotR -> Position.rotR mino.pos, rotAsixs direction mino, Vector.rotL

            let rec loop =
                function
                | [] -> mino
                | shift :: t ->
                    let rec loop2 =
                        function
                        | [] -> loop t
                        | relRotAsix :: t2 ->
                            let (cx, cy) as center =
                                (mino.x, mino.y)
                                |> Vector.add relRotAsix
                                |> Vector.add (rot relRotAsix)
                                |> Vector.add shift

                            pos.XYs
                            |> Array.exists (
                                Vector.add center >> TetrisBoard.isFilled board
                            )
                            |> function
                                | true -> loop2 t2
                                | false -> { mino with x = cx; y = cy; pos = pos }

                    loop2 rAsixs

            let spinned =
                loop [
                    (0, 0)
                    (0, 1)
                    (-1, 0)
                    (1, 0)
                    (0, -1)
                    (2, 0)
                    (0, -2)
                    (-2, 0)
                    (0, 2)
                ]

            let adjusted =
                if spinned.y < mino.y then
                    { spinned with y = spinned.y + 1 }
                elif spinned.y > mino.y then
                    { spinned with y = spinned.y - 1 }
                else
                    spinned

            adjusted
            |> existsOtherBlock board
            |> function
                | false -> adjusted
                | true -> spinned

    let rotateRight board mino = rotate RotR board mino
    let rotateLeft board mino = rotate RotL board mino

    type private ShiftDirection =
        | Down
        | Right
        | Left

    let private tryShift direction board mino =
        let nxt =
            match direction with
            | Down -> { mino with y = mino.y + 1 }
            | Right -> { mino with x = mino.x + 1 }
            | Left -> { mino with x = mino.x - 1 }

        if existsOtherBlock board nxt then None else Some nxt

    let moveDown board mino =
        tryShift Down board mino |> Option.defaultValue mino

    let moveRight board mino =
        tryShift Right board mino |> Option.defaultValue mino

    let moveLeft board mino =
        tryShift Left board mino |> Option.defaultValue mino

    let isHighLimitOver mino =
        mino.pos.XYs
        |> Array.exists (fun (_, y) -> mino.y + y <= config.heightLimit)
