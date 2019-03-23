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
                Universe.evolve [ "-X-" ]
                    |> Expect.equal "dead universe" [ "---" ]

            ; testCase "Cells with single neighbour both die" <| fun _ ->
                Universe.evolve [ "-XX-" ]
                    |> Expect.equal "underpopulated" [ "----" ]

            ; testCase "Cell with two neighbours stay alive" <| fun _ ->
                Universe.evolve [ "-XXX-" ]
                    |> Expect.equal "last survivor" [ "--X--" ]

            ; testCase "Cell with three neighbours stay alive" <| fun _ ->
                [ "--X--"
                ; "-XXX-"
                ; "-----"
                ]
                    |> Universe.evolve
                    |> Expect.equal "two are also good" <|
                        [ "-xXx-"
                        ; "-XXX-"
                        ; "--x--"
                        ]

            ; testCase "Cell with more then three neighbours dies" <| fun _ ->
                [ "-X---"
                ; "-XXX-"
                ; "---X-"
                ]
                    |> Universe.evolve
                    |> Expect.equal "overpopulated" <|
                        [ "-X---"
                        ; "-X-X-"
                        ; "---X-"
                        ]

            ; testCase "Exactly three cells spawn new life" <| fun _ ->
                [ "---X-"
                ; "-X-X-"
                ]
                    |> Universe.evolve
                    |> Expect.equal "reproducted" <|
                        [ "--x--"
                        ; "--x--"
                        ]
            ]


    [<Tests>]
    let knownStillLifes =
        let cases =
            [ ( "Block"
              , [ "----"
                ; "-XX-"
                ; "-XX-"
                ; "----"
                ]
              )
            ; ( "Bee-hive"
              , [ "------"
                ; "--XX--"
                ; "-X--X-"
                ; "--XX--"
                ; "------"
                ]
              )
            ; ( "Loaf"
              , [ "------"
                ; "--XX--"
                ; "-X--X-"
                ; "--X-X-"
                ; "---X--"
                ; "------"
                ]
              )
            ; ( "Boat"
              , [ "-----"
                ; "-XX--"
                ; "-X-X-"
                ; "--X--"
                ; "-----"
                ]
              )
            ; ( "Tub"
              , [ "-----"
                ; "--X--"
                ; "-X-X-"
                ; "--X--"
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
