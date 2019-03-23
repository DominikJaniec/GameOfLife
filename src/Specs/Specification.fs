namespace GameOfLife.Specs

open Expecto
open Expecto.Flip
open GameOfLife

module Specification =

    [<Tests>]
    let tests =
        testList "Game of Life: Specification"
            [ testCase "Empty universe does not change" <| fun _ ->
                Universe.evolve []
                    |> Expect.isEmpty "Unexpected change"

            ; testCase "Dead universe does not spawn a life" <| fun _ ->
                Universe.evolve [ "-" ]
                    |> Expect.equal "dead universe" [ "-" ]

            ; testCase "Huge and dead universe stay daead" <| fun _ ->
                [ "------"
                ; "------"
                ; "------"
                ]
                    |> Universe.evolve
                    |> Expect.equal "huge & dead universe" <|
                        [ "------"
                        ; "------"
                        ; "------"
                        ]

            ; testCase "Cell without neighbours dies" <| fun _ ->
                Universe.evolve [ "-x-" ]
                    |> Expect.equal "dead universe" [ "---" ]

            ; testCase "Cells with single neighbour both die" <| fun _ ->
                Universe.evolve [ "-xx-" ]
                    |> Expect.equal "dead universe" [ "----" ]

            ; testCase "Cell with two neighbours stay alive" <| fun _ ->
                Universe.evolve [ "-xxx-" ]
                    |> Expect.equal "last survivor" [ "--x--" ]
            ; testCase "Cell with three neighbours stay alive" <| fun _ ->
                [ "--x--"
                ; "-xxx-"
                ]
                    |> Universe.evolve
                    |> Expect.equal "two are also good" <|
                        [ "--x--"
                        ; "-xxx-"
                        ]
            ]
