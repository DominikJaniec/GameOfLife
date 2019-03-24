namespace GameOfLife
open System

module Universe =

    [<Literal>]
    let Alive = 'X'

    [<Literal>]
    let Infant = 'x'

    [<Literal>]
    let Dead = '-'

    let private dead =
        String [| Dead |]

    let evolve (state : string list) : string list =
        let fateOf it neighbours =
            let rise = function
                | Infant -> Alive
                | cell -> cell

            let alives =
                neighbours
                    |> Seq.where (fun a -> a <> Dead)
                    |> Seq.length

            match (it, alives) with
            | (Dead, 3) -> Infant
            | (Alive, 2) -> Alive
            | (Alive, 3) -> Alive
            | (Infant, 2) -> Alive
            | (Infant, 3) -> Alive
            | _ -> Dead


        let patchitude = function
            | ( [| nw; n; ne |]
              , [|  w; x;  e |]
              , [| sw; s; se |]
              ) -> [ nw; n; ne; w; e; sw; s; se ] |> fateOf x
            | _ -> failwith "Unexpected Universe's patch shape."


        let flatitude (window : string []) =
            let border line = Seq.concat [ dead; line; dead ]
            let patched = border >> Seq.windowed 3

            let above = window.[0] |> patched
            let current = window.[1] |> patched
            let bottom = window.[2] |> patched

            Seq.zip3 above current bottom
                |> Seq.map patchitude
                |> Array.ofSeq
                |> String


        let bordered = function
            | None -> [ "" ]
            | Some (row : string) ->
                String.replicate (row.Length) dead
                    |> List.singleton

        let top = List.tryHead state |> bordered
        let down = List.tryLast state |> bordered

        Seq.concat [ top; state; down]
            |> Seq.windowed 3
            |> Seq.map flatitude
            |> List.ofSeq
