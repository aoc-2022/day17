open System.IO

open day17.State
open day17.Shape

let lines = File.ReadAllLines "/tmp/aoc/input.t" |> Array.toList

let parse (s: string) = s.ToCharArray() |> Array.toList

let input = lines |> List.collect parse

// input |> List.map (printfn "%A")

let inputPlus = []

let collides (shape: Shape) (state: State) =
    // printfn $"collides {shape} {state}"
    if state.Rows.Length < shape.Length then
        // printfn "Too shallow"
        true
    else
        let rows = state.Rows |> List.take shape.Length
        let zipped = List.zip shape rows
        // printfn $"zipped={zipped}"
        zipped
        |> List.exists (fun (shapeRow, stateRow) -> (shapeRow &&& stateRow) > 0b0uy)

let blowLeft (shape: Shape) (state: State) =
    let atLeftBorder = shape |> List.exists (fun line -> LEFTMOST &&& line > 0b0uy)

    if atLeftBorder then
        shape
    else
        let shifted = shiftShapeLeft shape
        if collides shifted state then shape else shifted

let blowRight (shape: Shape) (state: State) =
    let atRightBorder = shape |> List.exists (fun line -> RIGHTMOST &&& line > 0b0uy)

    if atRightBorder then
        shape
    else
        let shifted = shiftShapeRight shape
        if collides shifted state then shape else shifted

let canMoveDown (shape: Shape) (state: State) =
    // printfn $"canMoveDown {shape} {state}"
    if state.Rows.Length < shape.Length then
        // printfn $"pit too shallow"
        false
    else
        // printfn "checking collision"
        let state = state.MoveDown()
        collides shape state |> not

let blow (shape: Shape) (state: State) : Shape * State =
    // printShape shape
    // printfn $"blow {shape} {state}"
    let wind, state = state.NextWind()

    let shape =
        match wind with
        | '<' -> blowLeft shape state
        | '>' -> blowRight shape state
        | _ -> shape

    // printfn $"AFTER Blow: {wind}"
    // printShape shape
    shape, state

let nextShape (curr: Shape) : Shape =
    match curr with
    | s when s = LINE -> PLUS
    | s when s = PLUS -> ANGLE
    | s when s = ANGLE -> BAR
    | s when s = BAR -> BOX
    | s when s = BOX -> LINE

let rec dropShape (shape: Shape) (state: State) : State =
    let shape, state = blow shape state

    if (canMoveDown shape state) then
        dropShape shape (state.MoveDown())
    else
        state.MergeOnTop shape

let rec dropShapes (n: int) (state: State) (shape: Shape) : State =
    if n < 1 then
        state
    else
        let state = state.PrepareForNewShape(shape.Length)
        let height = state.Rows |> List.skipWhile (fun i -> i = 0b0uy) |> List.length
        let state = dropShape shape state
        let shape = nextShape shape
        dropShapes (n - 1) state shape

let result = dropShapes 2022 (State.initState input) LINE

printfn "Result: {{{"
printShape result.Rows
printfn "}}}"
