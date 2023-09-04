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

type Tetrimino = {
    x: int
    y: int
    pos: (int * int)[]
    shape: Shape
}

module TetrisBoard =

    let init () =
        Array2D.init 24 16 (fun y x ->
            if (3 <= x && x <= 12 && 0 <= y && y <= 21) then
                Cell.Empty
            else
                Cell.Guard)

    let isFilled x y (board: TetrisBoard) =
        match board[y, x] with
        | Empty -> false
        | _ -> true

    let setTetrimino mino board =
        let nxt = board |> Array2D.copy

        mino.pos
        |> Array.iter (fun (dx, dy) -> nxt[mino.y + dy, mino.x + dx] <- Mino mino.shape)

        let mutable dy = 0

        let dif =
            List.rev [
                yield 23
                yield 22
                for y in 21..-1..2 do
                    let isLineFilled =
                        [ 3..12 ] |> List.forall (fun x -> isFilled x y nxt)

                    if isLineFilled then
                        for x in 3..12 do
                            nxt[y, x] <- Empty

                        yield y
                        dy <- dy + 1
                    else
                        yield y + dy
                yield 1
                yield 0
            ]

        for y in 21..-1..0 do
            for x in 3..12 do
                nxt[dif[y], x] <- nxt[y, x]

        {| newBoard = nxt; eraced = dy |}


module Tetrimino =

    let create (x, y) =
        function
        | Shape.I -> {
            x = x
            y = y
            shape = Shape.I
            // 1 0 2 3
            pos = [| (0, 0); (-1, 0); (1, 0); (2, 0) |]
          }
        | Shape.O -> {
            x = x
            y = y
            shape = Shape.O
            // 2 3
            // 0 1
            pos = [| (0, 0); (1, 0); (0, -1); (1, -1) |]
          }
        | Shape.S -> {
            x = x
            y = y
            shape = Shape.S
            //   2 3
            // 1 0
            pos = [| (0, 0); (-1, 0); (0, -1); (1, -1) |]
          }
        | Shape.Z -> {
            x = x
            y = y
            shape = Shape.Z
            //  1 2
            //    0 3
            pos = [| (0, 0); (-1, -1); (0, -1); (1, 0) |]
          }
        | Shape.J -> {
            x = x
            y = y
            shape = Shape.J
            // 3
            // 2 0 1
            pos = [| (0, 0); (1, 0); (-1, 0); (-1, -1) |]
          }
        | Shape.L -> {
            x = x
            y = y
            shape = Shape.L
            //     3
            // 2 0 1
            pos = [| (0, 0); (1, 0); (-1, 0); (1, -1) |]
          }
        | Shape.T -> {
            x = x
            y = y
            shape = Shape.T
            // 1 0 2
            //   3
            pos = [| (0, 0); (-1, 0); (1, 0); (0, 1) |]
          }

    [<RequireQualifiedAccess>]
    type MinoTheta =
        | ``0``
        | ``90``
        | ``180``
        | ``270``

    let private getTheta mino =
        match mino.shape with
        | Shape.O -> MinoTheta.``0``
        | Shape.T ->
            match mino.pos[3] with
            | (0, 1) -> MinoTheta.``0``
            | (1, 0) -> MinoTheta.``90``
            | (0, -1) -> MinoTheta.``180``
            | _ -> MinoTheta.``270``
        | Shape.L ->
            match mino.pos[2] with
            | (0, 1) -> MinoTheta.``0``
            | (1, 0) -> MinoTheta.``90``
            | (0, -1) -> MinoTheta.``180``
            | _ -> MinoTheta.``270``
        | Shape.Z ->
            match mino.pos[3] with
            | (1, 0) -> MinoTheta.``0``
            | (0, 1) -> MinoTheta.``90``
            | (-1, 0) -> MinoTheta.``180``
            | _ -> MinoTheta.``270``
        | Shape.J ->
            match mino.pos[1] with
            | (1, 0) -> MinoTheta.``0``
            | (0, 1) -> MinoTheta.``90``
            | (-1, 0) -> MinoTheta.``180``
            | _ -> MinoTheta.``270``
        | Shape.S ->
            match mino.pos[1] with
            | (0, -1) -> MinoTheta.``0``
            | (0, 1) -> MinoTheta.``90``
            | (1, 0) -> MinoTheta.``180``
            | _ -> MinoTheta.``270``
        | Shape.I ->
            match mino.pos[1] with
            | (-1, 0) -> MinoTheta.``0``
            | (0, 1) -> MinoTheta.``90``
            | (1, 0) -> MinoTheta.``180``
            | _ -> MinoTheta.``270``

    let rightRotAsixs mino =
        match mino.shape with
        | Shape.T ->
            match getTheta mino with
            | MinoTheta.``0`` -> [ mino.pos[0]; mino.pos[3]; (-1, 1); (-1, 2) ]
            | MinoTheta.``90`` -> [ mino.pos[0]; mino.pos[3] ]
            | MinoTheta.``180`` -> [ mino.pos[0]; mino.pos[2] ]
            | MinoTheta.``270`` -> [ mino.pos[0]; mino.pos[3] ]
        | Shape.S ->
            match getTheta mino with
            | MinoTheta.``0`` -> [ mino.pos[0]; (0, 1) ]
            | _ -> [ (0, 0) ]
        | Shape.Z ->
            match getTheta mino with
            | MinoTheta.``0`` -> [ mino.pos[0]; (-1, -1) ]
            | _ -> [ (0, 0) ]
        | Shape.L ->
            match getTheta mino with
            | MinoTheta.``0`` -> [ mino.pos[0]; (-1, 0) ]
            | MinoTheta.``90`` -> [ mino.pos[0]; mino.pos[2]; (-1, 1) ]
            | MinoTheta.``180`` -> [ mino.pos[0]; mino.pos[1] ]
            | MinoTheta.``270`` -> [ (0, 0) ]
        | Shape.J ->
            match getTheta mino with
            | MinoTheta.``90`` -> [ mino.pos[0]; mino.pos[1] ]
            | _ -> [ (0, 0) ]
        | Shape.I ->
            match getTheta mino with
            | MinoTheta.``0`` -> [ mino.pos[0]; mino.pos[1] ]
            | MinoTheta.``90`` -> [ mino.pos[0]; mino.pos[1] ]
            | _ -> [ (0, 0) ]
        | _ -> [ (0, 0) ]


    let leftRotAsixs mino =
        match mino.shape with
        | Shape.T ->
            match getTheta mino with
            | MinoTheta.``0`` -> [ mino.pos[0]; mino.pos[3]; (1, 1); (1, 2) ]
            | MinoTheta.``90`` -> [ mino.pos[0]; mino.pos[3] ]
            | MinoTheta.``180`` -> [ mino.pos[0]; mino.pos[1] ]
            | MinoTheta.``270`` -> [ mino.pos[0]; mino.pos[3] ]
        | Shape.S ->
            match getTheta mino with
            | MinoTheta.``270`` -> [ mino.pos[0]; (1, 1) ]
            | _ -> [ (0, 0) ]
        | Shape.Z ->
            match getTheta mino with
            | MinoTheta.``90`` -> [ mino.pos[0]; (1, 1) ]
            | _ -> [ (0, 0) ]
        | Shape.L ->
            match getTheta mino with
            | MinoTheta.``0`` -> [ mino.pos[0]; mino.pos[2] ]
            | _ -> [ (0, 0) ]
        | Shape.J ->
            match getTheta mino with
            | MinoTheta.``0`` -> [ (0, 0) ]
            | MinoTheta.``90`` -> [ mino.pos[0]; (1, 0) ]
            | MinoTheta.``180`` -> [ mino.pos[0]; mino.pos[1]; (1, 1) ]
            | MinoTheta.``270`` -> [ mino.pos[0]; mino.pos[2] ]
        | Shape.I ->
            match getTheta mino with
            | MinoTheta.``90`` -> [ mino.pos[0]; mino.pos[1] ]
            | MinoTheta.``180`` -> [ mino.pos[0]; mino.pos[1] ]
            | _ -> [ (0, 0) ]
        | _ -> [ (0, 0) ]

    let isHighLimitOver mino =
        let highLimit = 3

        mino.pos
        |> Array.exists (fun (_, y) -> mino.y + y <= highLimit)

    let existsOtherBlock board mino =
        mino.pos
        |> Array.exists (fun (dx, dy) ->
            board |> TetrisBoard.isFilled (mino.x + dx) (mino.y + dy))

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

    let rotateRight board mino =
        if mino.shape = Shape.O then
            mino
        else
            let pos = [| for (dx, dy) in mino.pos -> (-dy, dx) |]

            let r_asixs = rightRotAsixs mino

            let rec loop =
                function
                | [] -> mino
                | (mx, my) :: t ->
                    let rec loop2 =
                        function
                        | [] -> loop t
                        | (hx, hy) :: t2 ->
                            let cx, cy = mino.x + hx + hy, mino.y + hy - hx
                            let nx, ny = cx + mx, cy + my

                            pos
                            |> Array.exists (fun (dx, dy) ->
                                board |> TetrisBoard.isFilled (nx + dx) (ny + dy))
                            |> function
                                | true -> loop2 t2
                                | false -> { mino with x = nx; y = ny; pos = pos }

                    loop2 r_asixs

            let moved =
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

            let modify = // adjust mino's height
                if moved.y < mino.y then { moved with y = moved.y + 1 }
                elif moved.y > mino.y then { moved with y = moved.y - 1 }
                else moved

            modify
            |> existsOtherBlock board
            |> function
                | false -> modify
                | true -> moved

    let rotateLeft board mino =
        if mino.shape = Shape.O then
            mino
        else
            let pos = [| for (dx, dy) in mino.pos -> (dy, -dx) |]

            let r_asixs = leftRotAsixs mino

            let rec loop =
                function
                | [] -> mino
                | (mx, my) :: t ->
                    let rec loop2 =
                        function
                        | [] -> loop t
                        | (hx, hy) :: t2 ->
                            let cx, cy = mino.x + hx - hy, mino.y + hy + hx
                            let nx, ny = cx + mx, cy + my

                            pos
                            |> Array.exists (fun (dx, dy) ->
                                board |> TetrisBoard.isFilled (nx + dx) (ny + dy))
                            |> function
                                | false -> { mino with x = nx; y = ny; pos = pos }
                                | true -> loop2 t2

                    loop2 r_asixs

            let moved =
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

            let modified =
                if moved.y < mino.y then { moved with y = moved.y + 1 }
                elif moved.y > mino.y then { moved with y = moved.y - 1 }
                else moved

            modified
            |> existsOtherBlock board
            |> function
                | false -> modified
                | true -> moved
