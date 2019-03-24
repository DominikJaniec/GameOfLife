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

        let toTestCase (name, shape) =
            let testName = sprintf "The '%s' does not change" name
            testCase testName <| fun _ ->
                Universe.evolve shape
                    |> Expect.equal "to not changed" shape

        List.map toTestCase cases
            |> testList "Game of Life's Known: Still Lifes"



    [<Tests>]
    let knownOscillators =
        let cases =
            [ ( "Blinker"
              , [ [ "-----"
                  ; "-----"
                  ; "-xXx-"
                  ; "-----"
                  ; "-----"
                  ]
                ; [ "-----"
                  ; "--x--"
                  ; "--X--"
                  ; "--x--"
                  ; "-----"
                  ]
                ]
              )
            ; ( "Toad"
              , [ [ "------"
                  ; "------"
                  ; "--xxX-"
                  ; "-Xxx--"
                  ; "------"
                  ; "------"
                  ]
                ; [ "------"
                  ; "---x--"
                  ; "-x--X-"
                  ; "-X--x-"
                  ; "--x---"
                  ; "------"
                  ]
                ]
              )
            ; ( "Beacon"
              , [ [ "------"
                  ; "-XX---"
                  ; "-X----"
                  ; "----X-"
                  ; "---XX-"
                  ; "------"
                  ]
                ; [ "------"
                  ; "-XX---"
                  ; "-Xx---"
                  ; "---xX-"
                  ; "---XX-"
                  ; "------"
                  ]
                ]
              )
            ; ( "Pulsar"
              , [ [ "-----------------"
                  ; "----XXx---xXX----"
                  ; "-----------------"
                  ; "--X----X-X----X--"
                  ; "--X----X-X----X--"
                  ; "--x----X-X----x--"
                  ; "----XXX---XXX----"
                  ; "-----------------"
                  ; "----XXX---XXX----"
                  ; "--x----X-X----x--"
                  ; "--X----X-X----X--"
                  ; "--X----X-X----X--"
                  ; "-----------------"
                  ; "----XXx---xXX----"
                  ; "-----------------"
                  ]
                ; [ "-----x-----x-----"
                  ; "-----X-----X-----"
                  ; "-----xx---xx-----"
                  ; "-----------------"
                  ; "-xXx--xX-Xx--xXx-"
                  ; "---x-x-X-X-x-x---"
                  ; "-----XX---XX-----"
                  ; "-----------------"
                  ; "-----XX---XX-----"
                  ; "---x-x-X-X-x-x---"
                  ; "-xXx--xX-Xx--xXx-"
                  ; "-----------------"
                  ; "-----xx---xx-----"
                  ; "-----X-----X-----"
                  ; "-----x-----x-----"
                  ]
                ; [ "-----------------"
                  ; "----xX-----Xx----"
                  ; "-----XX---XX-----"
                  ; "--x--x-x-x-x--x--"
                  ; "--XXx-XX-XX-xXX--"
                  ; "---X-X-X-X-X-X---"
                  ; "----xXX---XXx----"
                  ; "-----------------"
                  ; "----xXX---XXx----"
                  ; "---X-X-X-X-X-X---"
                  ; "--XXx-XX-XX-xXX--"
                  ; "--x--x-x-x-x--x--"
                  ; "-----XX---XX-----"
                  ; "----xX-----Xx----"
                  ; "-----------------"
                  ]
                ]
              )
            ]


        let toTestCases (name, turns) =
            let period = List.length turns

            let testName turn =
                let from = turn + 1
                let into = (from % period) + 1
                sprintf "The '%s' keeps oscillating with period %d at evolution %d -> %d" name period from into

            let stepCase turn source =
                let next = turns.[(turn + 1) % period]
                testCase (testName turn) <| fun _ ->
                    Universe.evolve source
                        |> Expect.equal "evolved" next

            List.mapi stepCase turns


        Seq.collect toTestCases cases
            |> List.ofSeq
            |> testList "Game of Life's Known: Oscillators"
