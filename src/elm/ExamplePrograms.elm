module ExamplePrograms exposing (fibonacci)

import Array exposing (Array)
import Word8 exposing (Word8)


fibonacci : Array Word8
fibonacci =
    [ -- Set up variables
      [ 0x21, 0x24, 0x00 ] -- LD HL,0x0024
    , [ 0x06, 0x01 ] -- LD B,0x01
    , [ 0x0E, 0x01 ] -- LD C,0x01
    , [ 0x16, 0x0C ] -- LD D,0x0C

    -- Loop Body
    -- Add last two values together into A
    , [ 0x79 ] -- LD A,C
    , [ 0x80 ] -- ADD A,B

    -- Update registers with new last two values
    , [ 0x41 ] -- LD B,C
    , [ 0x4F ] -- LD C,A

    -- Write last value to the memory address in HL, incrementing HL afterwards
    , [ 0x22 ] -- LD (HL+),A

    -- Decrement counter then jump to beginning of the loop if the result is not zero
    , [ 0x15 ] -- DEC D
    , [ 0x20, 0x09, 0x00 ] -- JP NZ,0x0009
    , [ 0xC3, 0x12, 0x00 ]
    ]
        |> List.concatMap identity
        |> List.map Word8.fromInt
        |> Array.fromList
