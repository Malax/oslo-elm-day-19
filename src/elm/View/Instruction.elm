module View.Instruction exposing
    ( Instruction(..)
    , calculateAddresses
    , decodeAll
    , decodeBytes
    , encodeBytes
    , encodeD16Instruction
    , encodeD8Instruction
    , encodeInstruction
    , length
    , toString
    )

import Array exposing (Array)
import Word16 exposing (Word16)
import Word8 exposing (Word8)


type Instruction
    = LD_HL_D16 Word16
    | LD_B_D8 Word8
    | LD_C_D8 Word8
    | LD_D_D8 Word8
    | LD_A_C
    | ADD_A_B
    | LD_B_C
    | LD_C_A
    | LD_HLPLUS_A
    | DEC_D
    | JP_NZ_A16 Word16
    | NOP
    | JP_A16 Word16


toString : Instruction -> String
toString instruction =
    case instruction of
        LD_HL_D16 d16 ->
            "LD HL," ++ Word16.toString d16

        LD_B_D8 d8 ->
            "LD B," ++ Word8.toString d8

        LD_C_D8 d8 ->
            "LD C," ++ Word8.toString d8

        LD_D_D8 d8 ->
            "LD D," ++ Word8.toString d8

        LD_A_C ->
            "LD A,C"

        ADD_A_B ->
            "ADD A,B"

        LD_B_C ->
            "LD B,C"

        LD_C_A ->
            "LD C,A"

        LD_HLPLUS_A ->
            "LD (HL+),A"

        DEC_D ->
            "DEC D"

        JP_NZ_A16 a16 ->
            "JP NZ " ++ Word16.toString a16

        JP_A16 a16 ->
            "JP " ++ Word16.toString a16

        NOP ->
            "NOP"


length : Instruction -> Int
length =
    encodeBytes >> Array.length


encodeBytes : Instruction -> Array Word8
encodeBytes instruction =
    case instruction of
        LD_HL_D16 d16 ->
            encodeD16Instruction 0x21 d16

        LD_B_D8 d8 ->
            encodeD8Instruction 0x06 d8

        LD_C_D8 d8 ->
            encodeD8Instruction 0x0E d8

        LD_D_D8 d8 ->
            encodeD8Instruction 0x16 d8

        LD_A_C ->
            encodeInstruction 0x79

        ADD_A_B ->
            encodeInstruction 0x80

        LD_B_C ->
            encodeInstruction 0x41

        LD_C_A ->
            encodeInstruction 0x4F

        LD_HLPLUS_A ->
            encodeInstruction 0x22

        DEC_D ->
            encodeInstruction 0x15

        JP_NZ_A16 d16 ->
            encodeD16Instruction 0x20 d16

        JP_A16 d16 ->
            encodeD16Instruction 0xC3 d16

        NOP ->
            encodeInstruction 0x00


decodeBytes : Array Word8 -> Maybe Instruction
decodeBytes bytes =
    let
        maybeConstructor =
            Maybe.andThen instructionConstructorByOpcode (Array.get 0 bytes)
    in
    Maybe.andThen
        (\constructor ->
            case constructor of
                Word8Operand f ->
                    Maybe.map f (Array.get 1 bytes)

                Word16Operand f ->
                    Maybe.map2 Word16.fromWord8 (Array.get 2 bytes) (Array.get 1 bytes)
                        |> Maybe.map f

                WithoutOperand instruction ->
                    Just instruction
        )
        maybeConstructor


decodeAll : Array Word8 -> List (Maybe Instruction)
decodeAll bytes =
    decodeAllHelper bytes []


calculateAddresses : List (Maybe Instruction) -> List ( Int, Maybe Instruction )
calculateAddresses list =
    List.foldl
        (\instuction ( accumulatedList, accumulatedAddress ) ->
            let
                instructionLength =
                    instuction |> Maybe.map length |> Maybe.withDefault 1
            in
            ( ( accumulatedAddress, instuction ) :: accumulatedList, accumulatedAddress + instructionLength )
        )
        ( [], 0 )
        list
        |> Tuple.first
        |> List.reverse



-- Internal


decodeAllHelper : Array Word8 -> List (Maybe Instruction) -> List (Maybe Instruction)
decodeAllHelper bytes acc =
    let
        maybeInstruction =
            decodeBytes bytes

        instructionLength =
            maybeInstruction |> Maybe.map length |> Maybe.withDefault 1

        slicedBytes =
            Array.slice instructionLength (Array.length bytes) bytes
    in
    if Array.length slicedBytes > 0 then
        decodeAllHelper slicedBytes (maybeInstruction :: acc)

    else
        maybeInstruction :: acc |> List.reverse


type InstructionConstructor
    = Word8Operand (Word8 -> Instruction)
    | Word16Operand (Word16 -> Instruction)
    | WithoutOperand Instruction


instructionConstructorByOpcode : Word8 -> Maybe InstructionConstructor
instructionConstructorByOpcode opcode =
    case Word8.toInt opcode of
        0x21 ->
            Just (Word16Operand LD_HL_D16)

        0x06 ->
            Just (Word8Operand LD_B_D8)

        0x0E ->
            Just (Word8Operand LD_C_D8)

        0x16 ->
            Just (Word8Operand LD_D_D8)

        0x79 ->
            Just (WithoutOperand LD_A_C)

        0x80 ->
            Just (WithoutOperand ADD_A_B)

        0x41 ->
            Just (WithoutOperand LD_B_C)

        0x4F ->
            Just (WithoutOperand LD_C_A)

        0x22 ->
            Just (WithoutOperand LD_HLPLUS_A)

        0x15 ->
            Just (WithoutOperand DEC_D)

        0x20 ->
            Just (Word16Operand JP_NZ_A16)

        0xC3 ->
            Just (Word16Operand JP_A16)

        0x00 ->
            Just (WithoutOperand NOP)

        _ ->
            Nothing


encodeInstruction : Int -> Array Word8
encodeInstruction opcode =
    Array.fromList [ Word8.fromInt opcode ]


encodeD8Instruction : Int -> Word8 -> Array Word8
encodeD8Instruction opcode d8 =
    Array.fromList [ Word8.fromInt opcode, d8 ]


encodeD16Instruction : Int -> Word16 -> Array Word8
encodeD16Instruction opcode d16 =
    let
        ( highByte, lowByte ) =
            Word16.toWord8 d16
    in
    Array.fromList [ Word8.fromInt opcode, lowByte, highByte ]
