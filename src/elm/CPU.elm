module CPU exposing (CPU, Flag(..), Register16(..), Register8(..), clearFlag, getFlag, incrementPC, init, readRegister16, setFlag, updateFlag, writeRegister16, writeRegister8)

import Word16 exposing (Word16)
import Word8 exposing (Word8)


type Flag
    = Zero


type Register8
    = A
    | B
    | C
    | D
    | E
    | F
    | H
    | L


type Register16
    = AF
    | BC
    | DE
    | HL
    | SP
    | PC


type alias CPU =
    { a : Word8
    , b : Word8
    , c : Word8
    , d : Word8
    , e : Word8
    , f : Word8
    , h : Word8
    , l : Word8
    , pc : Word16
    , sp : Word16
    }


init : CPU
init =
    { a = Word8.fromInt 0x00
    , b = Word8.fromInt 0x00
    , c = Word8.fromInt 0x00
    , d = Word8.fromInt 0x00
    , e = Word8.fromInt 0x00
    , f = Word8.fromInt 0x00
    , h = Word8.fromInt 0x00
    , l = Word8.fromInt 0x00
    , pc = Word16.fromInt 0x00
    , sp = Word16.fromInt 0x00
    }


incrementPC : Int -> CPU -> CPU
incrementPC value cpu =
    { cpu | pc = Word16.add value cpu.pc }


writeRegister8 : Register8 -> Word8 -> CPU -> CPU
writeRegister8 register8 value cpu =
    case register8 of
        A ->
            { cpu | a = value }

        B ->
            { cpu | b = value }

        C ->
            { cpu | c = value }

        D ->
            { cpu | d = value }

        E ->
            { cpu | e = value }

        F ->
            { cpu | f = value }

        H ->
            { cpu | h = value }

        L ->
            { cpu | l = value }


writeRegister16 : Register16 -> Word16 -> CPU -> CPU
writeRegister16 register16 value cpu =
    case register16 of
        PC ->
            { cpu | pc = value }

        SP ->
            { cpu | sp = value }

        AF ->
            let
                ( highWord8, lowWord8 ) =
                    Word16.toWord8 value
            in
            { cpu | a = highWord8, f = lowWord8 }

        BC ->
            let
                ( highWord8, lowWord8 ) =
                    Word16.toWord8 value
            in
            { cpu | b = highWord8, c = lowWord8 }

        DE ->
            let
                ( highWord8, lowWord8 ) =
                    Word16.toWord8 value
            in
            { cpu | d = highWord8, e = lowWord8 }

        HL ->
            let
                ( highWord8, lowWord8 ) =
                    Word16.toWord8 value
            in
            { cpu | h = highWord8, l = lowWord8 }


readRegister16 : Register16 -> CPU -> Word16
readRegister16 register16 cpu =
    case register16 of
        PC ->
            cpu.pc

        SP ->
            cpu.sp

        AF ->
            Word16.fromWord8 cpu.a cpu.f

        BC ->
            Word16.fromWord8 cpu.b cpu.c

        DE ->
            Word16.fromWord8 cpu.d cpu.e

        HL ->
            Word16.fromWord8 cpu.h cpu.l


getFlag : Flag -> CPU -> Bool
getFlag flag cpu =
    Word8.getBit (flagBitIndex flag) cpu.f


setFlag : Flag -> CPU -> CPU
setFlag flag cpu =
    { cpu | f = Word8.setBit (flagBitIndex flag) cpu.f }


clearFlag : Flag -> CPU -> CPU
clearFlag flag cpu =
    { cpu | f = Word8.clearBit (flagBitIndex flag) cpu.f }


updateFlag : Flag -> Bool -> CPU -> CPU
updateFlag flag value =
    if value then
        setFlag flag

    else
        clearFlag flag



-- Internal


flagBitIndex : Flag -> Int
flagBitIndex flag =
    case flag of
        Zero ->
            0
