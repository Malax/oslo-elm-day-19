module Word8 exposing (Word8, add, clearBit, fromInt, getBit, setBit, subtract, toInt, toString)

import Bitwise
import Hex


type Word8
    = Word8 Int


fromInt : Int -> Word8
fromInt =
    Bitwise.and 0xFF >> Word8


toInt : Word8 -> Int
toInt (Word8 value) =
    value


add : Word8 -> Word8 -> Word8
add (Word8 a) (Word8 b) =
    fromInt (a + b)


subtract : Word8 -> Int -> Word8
subtract (Word8 a) b =
    fromInt (a - b)


getBit : Int -> Word8 -> Bool
getBit index (Word8 value) =
    let
        pattern =
            Bitwise.shiftLeftBy index 1
    in
    Bitwise.and pattern value > 0


setBit : Int -> Word8 -> Word8
setBit index (Word8 value) =
    let
        pattern =
            Bitwise.shiftLeftBy index 1
    in
    Bitwise.or pattern value |> Word8


clearBit : Int -> Word8 -> Word8
clearBit index (Word8 value) =
    let
        pattern =
            Bitwise.shiftLeftBy index 1 |> Bitwise.complement
    in
    Bitwise.and pattern value |> Word8


toString : Word8 -> String
toString (Word8 value) =
    value
        |> Hex.toString
        |> String.padLeft 2 '0'
        |> String.toUpper
        |> (++) "0x"
