module Year2017.Day06 exposing (..)

import Advent exposing (Test)
import Set exposing (Set)
import Dict exposing (Dict)


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
    List Int


type alias Output =
    Int


type alias Index =
    Int


type alias Max =
    ( Index, Int )


parse : String -> Input
parse input =
    input
        |> String.split "\t"
        |> List.map Advent.toInt
        |> Debug.log "parsed input"


getMax : List Int -> Int -> ( Int, Int ) -> ( Int, Int )
getMax inputList currIndex ( maxIndex, max ) =
    case inputList of
        [] ->
            ( maxIndex, max )

        val :: rest ->
            let
                nextMax =
                    if val > max then
                        ( currIndex, val )
                    else
                        ( maxIndex, max )

                _ =
                    Debug.log "pair" ( val, max, val > max )
            in
                getMax rest (currIndex + 1) nextMax


redistribute : Int -> Int -> List Int -> List Int
redistribute remainingBlocks currentBank banks =
    case remainingBlocks of
        0 ->
            banks

        _ ->
            let
                nextBank =
                    getNextBank currentBank banks

                updatedBanks =
                    List.indexedMap
                        (\index bankBlocks ->
                            if currentBank == index then
                                bankBlocks + 1
                            else
                                bankBlocks
                        )
                        banks
            in
                redistribute (remainingBlocks - 1)
                    nextBank
                    updatedBanks


nextCycle : Set.Set (List Int) -> List Int -> Int -> Int
nextCycle previousBanks banks numDistributions =
    if Set.member banks previousBanks && numDistributions /= 0 then
        numDistributions
    else
        let
            ( maxIndex, blocksToDistribute ) =
                getMax banks 0 ( 0, 0 )

            updatedBanks =
                redistribute blocksToDistribute
                    (getNextBank maxIndex banks)
                    (List.indexedMap
                        (\index blocks ->
                            if index == maxIndex then
                                0
                            else
                                blocks
                        )
                        banks
                    )

            updatedPreviousBanks =
                Set.insert banks previousBanks

            _ =
                Debug.log "updated banks" ( updatedBanks, numDistributions )
        in
            nextCycle updatedPreviousBanks updatedBanks (numDistributions + 1)


nextCycle2 : Dict (List Int) Int -> List Int -> Int -> Int
nextCycle2 previousBanks banks numDistributions =
    if Dict.member banks previousBanks && numDistributions /= 0 then
        Dict.get banks previousBanks
            |> Maybe.map (\distribution -> numDistributions - distribution)
            |> Maybe.withDefault 0
    else
        let
            ( maxIndex, blocksToDistribute ) =
                getMax banks 0 ( 0, 0 )

            updatedBanks =
                redistribute blocksToDistribute
                    (getNextBank maxIndex banks)
                    (List.indexedMap
                        (\index blocks ->
                            if index == maxIndex then
                                0
                            else
                                blocks
                        )
                        banks
                    )

            updatedPreviousBanks =
                Dict.insert banks numDistributions previousBanks

            _ =
                Debug.log "updated banks" ( updatedBanks, numDistributions )
        in
            nextCycle2 updatedPreviousBanks updatedBanks (numDistributions + 1)


getNextBank : Int -> List Int -> Int
getNextBank currentBank banks =
    if currentBank == (List.length banks - 1) then
        0
    else
        currentBank + 1


compute1 : Input -> Output
compute1 input =
    let
        ( maxIndex, blocksToDistribute ) =
            getMax input 0 ( 0, 0 )
                |> Debug.log "max"
    in
        nextCycle (Set.fromList []) input 0


compute2 : Input -> Output
compute2 input =
    let
        ( maxIndex, blocksToDistribute ) =
            getMax input 0 ( 0, 0 )
                |> Debug.log "max"
    in
        nextCycle2 (Dict.empty) input 0


tests1 : List (Test Input Output)
tests1 =
    [ Test "example"
        "0\t2\t7\t0"
        [ 0, 2, 7, 0 ]
        5
    ]


tests2 : List (Test Input Output)
tests2 =
    []


input : String
input =
    "11\t11\t13\t7\t0\t15\t5\t5\t4\t4\t1\t1\t7\t1\t15\t11"
