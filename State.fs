module day17.State

open day17.Shape

type PlusLine(depth: int, x: int) =
    member this.Depth = depth
    member this.X = x
    override this.ToString() = $"PlusLine({depth}:{x})"

type Wind(winds: char list, all: char list, consumed: int) =
    member this.Consumed = consumed 
    member this.TakeNext() : char * Wind =
        match winds with
        | [] -> all.Head, Wind(all.Tail, all, consumed + 1)
        | a :: rest -> a, Wind(rest, all, consumed + 1)
        
    member this.ResetConsumed () = Wind(winds,all,0)

    override this.ToString() = $"Wind({winds})"

type State(wind: Wind, rowsAbove: byte list, rows: byte list) =
    member this.Wind = wind
    member this.Rows = rows

    member this.NextWind() : char * State =
        let char, wind = wind.TakeNext()
        char, State(wind, rowsAbove, rows)

    member this.MoveDown() : State =
        State(wind, rows.Head :: rowsAbove, rows.Tail)

    member this.MergeOnTop(piece: byte list) =
        let rec merge (piece: byte list) (rows: byte list) =
            match (piece, rows) with
            | [], rows -> rows
            | p :: piece, r :: rows -> (p ||| r) :: (merge piece rows)

        let rows = merge piece rows
        State(wind, rowsAbove, rows)

    member this.PrepareForNewShape(shapeLines: int) =
        let rec addAbove (above: byte list) (rows: byte list) =
            match above with
            | [] -> rows 
            | a :: rest -> addAbove rest (a :: rows)

        let rows = addAbove rowsAbove rows
        let rows = rows |> List.skipWhile (fun row -> row = 0x0uy) // clearing existing empty, just as well as counting
        let rec prependEmpty (n: int) (l: byte list) =
            if n = 0 then l else (0b0uy) :: l |> prependEmpty (n - 1)

        let rows = prependEmpty (shapeLines + 3) rows
        let wind = wind.ResetConsumed ()
        State(wind, [], rows)

    override this.ToString() = $"State({wind},{rowsAbove},{rows}"

    static member initState(input: char list) =
        let empty = 0b0uy
        State(Wind(input, input,0), [], [])
