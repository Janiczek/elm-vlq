module Tests exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test)
import VLQ


examples : List ( List Int, String )
examples =
    [ ( [ 0 ], "A" )
    , ( [ 1 ], "C" )
    , ( [ -1 ], "D" )
    , ( [ 3 ], "G" )
    , ( [ 123 ], "2H" )
    , ( [ 123456789 ], "qxmvrH" )
    , ( [ 0, 0, 0, 0 ], "AAAA" )
    , ( [ 0, 0, 16, 1 ], "AAgBC" )
    , ( [ 3, 0, 0, 0 ], "GAAA" )
    , ( [ 123, 456, 789 ], "2HwcqxB" )
    , ( [ 8, 0, 4, 16 ], "QAIgB" )

    -- limits:
    , ( [ -2147483648 ], "B" )
    , ( [ 2147483647 ], "+/////D" )
    ]


suite : Test
suite =
    Test.describe "VLQ"
        [ Test.describe "Hardcoded examples"
            (List.map testExample examples)
        , Test.fuzz groupsFuzzer "Roundtrip: groups |> encode |> decode == Just groups" <|
            \groups ->
                groups
                    |> VLQ.encode
                    |> VLQ.decode
                    |> Expect.equal (Just groups)
        , Test.fuzz boundedIntFuzzer "encodeSingle == encode << List.singleton" <|
            \n ->
                VLQ.encodeSingle n
                    |> Expect.equal (VLQ.encode [ n ])
        ]


groupsFuzzer : Fuzzer (List Int)
groupsFuzzer =
    Fuzz.list boundedIntFuzzer


boundedIntFuzzer : Fuzzer Int
boundedIntFuzzer =
    Fuzz.intRange VLQ.minInt VLQ.maxInt


testExample : ( List Int, String ) -> Test
testExample ( input, output ) =
    let
        addEllipsis : String -> String
        addEllipsis string =
            if String.length string > 20 then
                String.left 19 string ++ "…"

            else
                string

        name : String
        name =
            [ Debug.toString input
            , Debug.toString output
            ]
                |> List.map addEllipsis
                |> String.join " ⇔ "
    in
    Test.test name <|
        \() ->
            let
                actualOutput : String
                actualOutput =
                    VLQ.encode input

                roundTrip : Maybe (List Int)
                roundTrip =
                    VLQ.decode actualOutput
            in
            ( actualOutput, roundTrip )
                |> Expect.all
                    [ Tuple.first >> Expect.equal output
                    , Tuple.second >> Expect.equal (Just input)
                    ]
