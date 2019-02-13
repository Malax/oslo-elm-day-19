module Word16 exposing (Word16, add, fromInt, fromWord8, toInt, toString, toWord8)

import Bitwise
import Hex
import Word8 exposing (Word8)


type Word16
    = Word16 Int


fromInt : Int -> Word16
fromInt =
    Bitwise.and 0xFFFF >> Word16


toInt : Word16 -> Int
toInt (Word16 value) =
    value


add : Int -> Word16 -> Word16
add a (Word16 b) =
    fromInt (a + b)


fromWord8 : Word8 -> Word8 -> Word16
fromWord8 highWord8 lowWord8 =
    (Bitwise.shiftLeftBy 8 (Word8.toInt highWord8) + Word8.toInt lowWord8)
        |> fromInt


toWord8 : Word16 -> ( Word8, Word8 )
toWord8 (Word16 value) =
    let
        highWord8 =
            Bitwise.shiftRightZfBy 8 value |> Word8.fromInt

        lowWord8 =
            -- This value might be larger, but Word8.fromInt will ensure only the
            -- least significant bits will be used to construct the Word8
            value |> Word8.fromInt
    in
    ( highWord8, lowWord8 )


toString : Word16 -> String
toString (Word16 value) =
    value
        |> Hex.toString
        |> String.padLeft 4 '0'
        |> String.toUpper
        |> (++) "0x"
