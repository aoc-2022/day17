open System.IO

open day17.State
open day17.Shape

let lines = File.ReadAllLines "/tmp/aoc/input" |> Array.toList

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

let result = dropShapes 20220 (State.initState input) LINE

let rockCounts = result.Rows |> List.map countRocks

printfn "Result: {{{"
// printShape result.Rows
printfn "}}}"

let allShapeRocks = [LINE;PLUS;ANGLE;BAR;BOX] |> List.concat |> List.map countRocks |> List.sum 

// rockCounts |> List.map (printf "%A ")

printfn $"allShapeRocks ={allShapeRocks}"

let rec takeUntilMultiple (rocks: int list) (i:int) (lines:int) : Option<int*int> =
    if i > 0 && i % 22 = 0 then Some(i,lines)
    else 
    match rocks with
    | [] -> None 
    | a::rest -> takeUntilMultiple rest (i+a) (lines+1)

let rec divByMultiple (rocks:int list) =
    // printfn $"rocks = {rocks}"
    if rocks.Length < 20 then []
    else 
    match takeUntilMultiple rocks 0 0 with
    | Some (multi,lines) ->
        // rocks |> List.take lines  |> List.sum |> (printfn "Takes group: %A")
        let next = rocks |> List.skip lines
        // printfn $"({multi},{lines})"
        (multi,lines) :: (divByMultiple next)
    | None -> []
let multiplies = divByMultiple rockCounts |> List.map (fun (i,j) -> (i/22,j))

printfn $"m={multiplies} length={multiplies.Length}"

let bufferSizesToTry = seq { 1..multiplies.Length/2 } |> Seq.toList

let testBufferSize (rocks:(int*int) list) (size:int) : Option<(int*int) list>=
    let buffer1 = rocks |> List.take size
    let buffer2 = rocks |> List.skip size |> List.take size
    if buffer1 = buffer2 then Some(buffer1) else None
    
let firstRepeat = bufferSizesToTry |> List.tryFind (fun size -> testBufferSize multiplies size |> Option.isSome)

let buffer1 = multiplies |> List.take firstRepeat.Value
let buffer2 = multiplies |> List.skip firstRepeat.Value |> List.take firstRepeat.Value

buffer1 |> List.map (printf "%A ")
printfn ""
buffer2 |> List.map (printf "%A ")
printfn ""

let rocksInSequence = buffer1 |> List.map fst  |> List.sum |> (fun n -> n * 5)
let linesInSequence = buffer1 |> List.map snd |> List.sum

printfn $"Rocks = {rocksInSequence} lines={linesInSequence}"

// ok, time for fun

let ALL_THE_ROCKS : int64 = 1000000000000L
let findLinesFromSeq (rocksInSequence: int) (linesInSequence:int): int64 =
    let rocksInSequence = rocksInSequence |> int64
    let linesInSequence = linesInSequence |> int64
    ALL_THE_ROCKS / (rocksInSequence) * linesInSequence 

let leftoverRocks (rocksInSequence: int) : int64 =
    let rocksInSequence = rocksInSequence |> int64
    ALL_THE_ROCKS % rocksInSequence

let firstLines = findLinesFromSeq rocksInSequence linesInSequence
let moreRocks = leftoverRocks rocksInSequence

printfn $"RESULT2 firstLines={firstLines} more rocks={moreRocks}"

let lastLines =
    let rockCount = rocksInSequence + (moreRocks |> int)
    let state = State.initState input
    let result = dropShapes rockCount state LINE
    result.Rows |> List.skipWhile (fun l -> l = 0b0uy) |> List.length

let finalResult = lastLines - linesInSequence |> int64 |> (fun last -> last + firstLines)

printfn $"Result2: lastLines={lastLines} inSeq={linesInSequence} diff={lastLines - linesInSequence}"
printfn $"final result: {finalResult}"