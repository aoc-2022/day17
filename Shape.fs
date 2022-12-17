module day17.Shape

type Shape = byte list
let LINE: Shape = [ 0b0011110uy ]
let PLUS: Shape = [ 0b0001000uy; 0b0011100uy; 0b0001000uy ]
let ANGLE: Shape = [ 0b0000100uy; 0b0000100uy; 0b0011100uy ]

let BAR: Shape = [ 0b0010000uy; 0b0010000uy; 0b0010000uy; 0b0010000uy ]

let BOX: Shape = [ 0b0011000uy; 0b0011000uy ]

let LEFTMOST = 0b1000000uy
let RIGHTMOST = 0b0000001uy

let shiftShapeLeft (shape: Shape) = shape |> List.map (fun b -> b <<< 1)

let shiftShapeRight (shape: Shape) = shape |> List.map (fun b -> b >>> 1)

let binToString (b: byte) : string =
    let bitAt (b: byte) (i: int) : string =
        if ((b >>> i) &&& 0b1uy) = 0b1uy then "#" else "."

    [ 6; 5; 4; 3; 2; 1; 0 ] |> List.map (bitAt b) |> (String.concat "")

let printShape (shape: Shape) =
    shape |> List.map binToString |> List.map (printfn "%s")

let rec countRocks (row: byte) =
    if row = 0b0uy then
        0
    else
        let lastbit = row &&& 0b1uy |> int
        lastbit + (countRocks (row >>> 1))
