module View exposing (view)

import Array exposing (Array)
import CPU exposing (CPU)
import Html exposing (Html, div, h2, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, style)
import RAM exposing (RAM)
import Types exposing (Model, Msg(..))
import Util
import View.Instruction as Instruction exposing (Instruction(..))
import Word16 exposing (Word16)
import Word8 exposing (Word8)


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "CPU Registers" ]
        , viewRegisters model.gameBoy.cpu
        , div [ class "ram-row" ]
            [ div [] [ h2 [] [ text "RAM (Raw)" ], viewRamAsBytes (Word16.toInt model.gameBoy.cpu.pc) (List.range 0x24 0x30) 0x00 0x30 model.gameBoy.ram ]
            , div [] [ h2 [] [ text "RAM (As Instructions)" ], viewRamAsProgram model.gameBoy.cpu.pc model.gameBoy.ram ]
            ]
        ]


viewRegisters : CPU -> Html Msg
viewRegisters cpu =
    table []
        [ thead [ class "register-table" ]
            [ th [] [ text "A" ]
            , th [] [ text "B" ]
            , th [] [ text "C" ]
            , th [] [ text "D" ]
            , th [] [ text "E" ]
            , th [] [ text "F" ]
            , th [] [ text "H" ]
            , th [] [ text "L" ]
            , th [] [ text "PC" ]
            , th [] [ text "SP" ]
            ]
        , tbody []
            [ tr []
                [ td [] [ viewWord8WithDecimal cpu.a ]
                , td [] [ viewWord8WithDecimal cpu.b ]
                , td [] [ viewWord8WithDecimal cpu.c ]
                , td [] [ viewWord8WithDecimal cpu.d ]
                , td [] [ viewWord8 cpu.e ]
                , td [] [ viewWord8 cpu.f ]
                , td [] [ viewWord8 cpu.h ]
                , td [] [ viewWord8 cpu.l ]
                , td [] [ viewWord16 cpu.pc ]
                , td [] [ viewWord16 cpu.sp ]
                ]
            ]
        ]


viewRamAsBytes : Int -> List Int -> Int -> Int -> RAM -> Html msg
viewRamAsBytes pcAddress interestingDataAddresses startAddress length ram =
    let
        rows =
            RAM.toArray ram
                |> Array.toList
                |> List.drop startAddress
                |> List.take length
                |> List.indexedMap
                    (\index byte ->
                        let
                            className =
                                if startAddress + index == pcAddress then
                                    "pc-marker"

                                else if List.member (startAddress + index) interestingDataAddresses then
                                    "interesting-data-marker"

                                else
                                    ""
                        in
                        td [ class className ] [ viewWord8WithDecimal byte ]
                    )
                |> Util.chunkList 4
                |> List.indexedMap (\index list -> th [] [ (startAddress + (index * 4)) |> Word16.fromInt |> viewWord16 ] :: list)
                |> List.map (tr [])
    in
    table [ class "ram-table" ] [ tbody [] rows ]


viewRamAsProgram : Word16 -> RAM -> Html Msg
viewRamAsProgram pc ram =
    let
        rows =
            RAM.toArray ram
                |> Array.slice 0 0x16
                |> Instruction.decodeAll
                |> Instruction.calculateAddresses
                |> List.map
                    (\( address, maybeInstruction ) ->
                        let
                            className =
                                if address == Word16.toInt pc then
                                    "pc-marker"

                                else
                                    ""
                        in
                        case maybeInstruction of
                            Just instruction ->
                                tr [ class className ]
                                    [ th [] [ address |> Word16.fromInt |> viewWord16 ]
                                    , td [] [ Instruction.encodeBytes instruction |> viewBytes ]
                                    , td [] [ Instruction.toString instruction |> text ]
                                    ]

                            Nothing ->
                                tr [ class className ]
                                    [ td [] [ text "???" ]
                                    , td [] [ text "???" ]
                                    , td [] [ text "???" ]
                                    ]
                    )
    in
    table [ class "ram-table-program" ]
        [ thead [] []
        , tbody [] rows
        ]


viewWord8WithDecimal : Word8 -> Html msg
viewWord8WithDecimal value =
    let
        dec =
            span [ class "decimal" ] [ "(" ++ (value |> Word8.toInt |> String.fromInt) ++ ")" |> text ]
    in
    div [] [ Word8.toString value |> text, text " ", dec ]


viewWord8 : Word8 -> Html msg
viewWord8 =
    Word8.toString >> text


viewWord16 : Word16 -> Html msg
viewWord16 =
    Word16.toString >> text


viewBytes : Array Word8 -> Html msg
viewBytes =
    Array.toList >> List.map Word8.toString >> List.foldl (\s acc -> acc ++ " " ++ s) "" >> text
