namespace GameOfLife.Specs

open Expecto
open Expecto.Flip
open GameOfLife

module Specification =

    [<Tests>]
    let tests =
        testList "Game of Life: Specification"
            [ testCase "universe exists (╭ರᴥ•́)" <| fun _ ->
                true |> Expect.isTrue "I compute, therefore I am."
            ; testCase "library is available" <| fun _ ->
                Say.hello "Test"
                    |> Expect.stringContains "" "Test"
            ]
