module Year2017.Day02 exposing (..)

import Advent exposing (Test)


main : Program Never ( Output, Output ) Never
main =
    Advent.program
        { input = input
        , parse1 = parse
        , parse2 = parse
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
        }


type alias Input =
    List (List Int)


type alias Output =
    Int


parse : String -> Input
parse input =
    [ [ -1 ] ]


compute1 : Input -> Output
compute1 input =
    -1


compute2 : Input -> Output
compute2 input =
    -1


tests1 : List (Test Input Output)
tests1 =
    [ Test "example"
        "input"
        [ [ 5, 1, 9, 5 ]
        , [ 7, 5, 3 ]
        , [ 2, 4, 6, 8 ]
        ]
        18
    ]


tests2 : List (Test Input Output)
tests2 =
    []


input : String
input =
    "input"
