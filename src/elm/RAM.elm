module RAM exposing (RAM, initWithProgram, readWord16, readWord8, toArray, writeWord8)

import Array exposing (Array)
import Word16 exposing (Word16)
import Word8 exposing (Word8)


type RAM
    = RAM (Array Word8)


readWord8 : Word16 -> RAM -> Word8
readWord8 address (RAM bytes) =
    Array.get (Word16.toInt address) bytes
        |> Maybe.withDefault (Word8.fromInt 0xFF)


writeWord8 : Word16 -> Word8 -> RAM -> RAM
writeWord8 address value (RAM bytes) =
    Array.set (Word16.toInt address) value bytes
        |> RAM


initWithProgram : Array Word8 -> RAM
initWithProgram program =
    let
        programSize =
            Array.length program

        emptyRAM =
            Array.initialize (ramSize - programSize) (\_ -> Word8.fromInt 0x00)
    in
    Array.append program emptyRAM
        |> RAM


readWord16 : Word16 -> RAM -> Word16
readWord16 address ram =
    let
        highWord8 =
            readWord8 (Word16.add 1 address) ram

        lowWord8 =
            readWord8 address ram
    in
    Word16.fromWord8 highWord8 lowWord8


toArray : RAM -> Array Word8
toArray (RAM array) =
    array


ramSize : Int
ramSize =
    0xFFFF
