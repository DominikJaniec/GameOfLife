namespace GameOfLife

module Universe =
    let evolve (state : string list) : string list =
        state |> List.map (fun line -> line.Replace('x', '-'))
