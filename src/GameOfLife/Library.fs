namespace GameOfLife
open System

module Universe =
    let evolve (state : string list) : string list =
        let fateOf it neighbours =
            let aliveCount =
                neighbours
                    |> Seq.where (fun a -> a = 'x')
                    |> Seq.length

            match aliveCount with
            | 2 | 3 -> it
            | _ -> '-'


        let patchitude = function
            | ( [| nw; n; ne |]
              , [|  w; x;  e |]
              , [| sw; s; se |]
              ) -> [ nw; n; ne; w; e; sw; s; se ] |> fateOf x
            | _ -> failwith "Unexpected Universe's patch shape."


        let flatitude (window : string []) =
            let border line = Seq.concat [ "-"; line; "-" ]
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
                String.replicate (row.Length) "-"
                    |> List.singleton

        let top = List.tryHead state |> bordered
        let down = List.tryLast state |> bordered

        Seq.concat [ top; state; down]
            |> Seq.windowed 3
            |> Seq.map flatitude
            |> List.ofSeq
