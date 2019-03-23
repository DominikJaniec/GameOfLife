namespace GameOfLife.Specs

open Expecto
open Expecto.Flip
open GameOfLife

module Specification =

    [<Tests>]
    let specification =
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


    [<Tests>]
    let knownStillLifes =
        let cases =
            [ ( "Block"
              , [ "----"
                ; "-xx-"
                ; "-xx-"
                ; "----"
                ]
              )
            ; ( "Bee-hive"
              , [ "------"
                ; "--xx--"
                ; "-x--x-"
                ; "--xx--"
                ; "------"
                ]
              )
            ; ( "Loaf"
              , [ "------"
                ; "--xx--"
                ; "-x--x-"
                ; "--x-x-"
                ; "---x--"
                ; "------"
                ]
              )
            ; ( "Boat"
              , [ "-----"
                ; "-xx--"
                ; "-x-x-"
                ; "--x--"
                ; "-----"
                ]
              )
            ; ( "Tub"
              , [ "-----"
                ; "--x--"
                ; "-x-x-"
                ; "--x--"
                ; "-----"
                ]
              )
            ]

        let example (name, shape) =
            testCase (sprintf "The '%s' does not change" name) <| fun _ ->
                Universe.evolve shape
                    |> Expect.equal "to not changed" shape

        testList "Game of Life's Known: Still Lifes" <|
            List.map example cases
