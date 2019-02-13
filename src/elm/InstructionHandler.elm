module InstructionHandler exposing (instructionHandlerByOpcode)

import CPU exposing (Flag(..), Register16(..), Register8(..))
import GameBoy exposing (GameBoy)
import RAM
import Word16
import Word8 exposing (Word8)


instructionHandlerByOpcode : Word8 -> GameBoy -> GameBoy
instructionHandlerByOpcode opcode =
    case Word8.toInt opcode of
        0x21 ->
            ld_hl_d16

        0x06 ->
            ld_b_d8

        0x0E ->
            ld_c_d8

        0x16 ->
            ld_d_d8

        0x79 ->
            ld_a_c

        0x80 ->
            add_a_b

        0x41 ->
            ld_b_c

        0x4F ->
            ld_c_a

        0x22 ->
            ld_hlplus_a

        0x15 ->
            dec_d

        0x20 ->
            jp_nz_a16

        0xC3 ->
            jp_a16

        _ ->
            identity



-- Internal


ld_hl_d16 : GameBoy -> GameBoy
ld_hl_d16 gameBoy =
    let
        d16 =
            RAM.readWord16 gameBoy.cpu.pc gameBoy.ram

        updatedCPU =
            gameBoy.cpu
                |> CPU.incrementPC 2
                |> CPU.writeRegister16 HL d16
    in
    { gameBoy | cpu = updatedCPU }


ld_b_d8 : GameBoy -> GameBoy
ld_b_d8 gameBoy =
    let
        d8 =
            RAM.readWord8 gameBoy.cpu.pc gameBoy.ram

        updatedCPU =
            gameBoy.cpu
                |> CPU.incrementPC 1
                |> CPU.writeRegister8 B d8
    in
    { gameBoy | cpu = updatedCPU }


ld_c_d8 : GameBoy -> GameBoy
ld_c_d8 gameBoy =
    let
        d8 =
            RAM.readWord8 gameBoy.cpu.pc gameBoy.ram

        updatedCPU =
            gameBoy.cpu
                |> CPU.incrementPC 1
                |> CPU.writeRegister8 C d8
    in
    { gameBoy | cpu = updatedCPU }


ld_d_d8 : GameBoy -> GameBoy
ld_d_d8 gameBoy =
    let
        d8 =
            RAM.readWord8 gameBoy.cpu.pc gameBoy.ram

        updatedCPU =
            gameBoy.cpu
                |> CPU.incrementPC 1
                |> CPU.writeRegister8 D d8
    in
    { gameBoy | cpu = updatedCPU }


ld_a_c : GameBoy -> GameBoy
ld_a_c gameBoy =
    let
        updatedCPU =
            CPU.writeRegister8 A gameBoy.cpu.c gameBoy.cpu
    in
    { gameBoy | cpu = updatedCPU }


ld_b_c : GameBoy -> GameBoy
ld_b_c gameBoy =
    let
        updatedCPU =
            CPU.writeRegister8 B gameBoy.cpu.c gameBoy.cpu
    in
    { gameBoy | cpu = updatedCPU }


ld_c_a : GameBoy -> GameBoy
ld_c_a gameBoy =
    let
        updatedCPU =
            CPU.writeRegister8 C gameBoy.cpu.a gameBoy.cpu
    in
    { gameBoy | cpu = updatedCPU }


add_a_b : GameBoy -> GameBoy
add_a_b gameBoy =
    let
        updatedCPU =
            CPU.writeRegister8 A (Word8.add gameBoy.cpu.a gameBoy.cpu.b) gameBoy.cpu
    in
    { gameBoy | cpu = updatedCPU }


dec_d : GameBoy -> GameBoy
dec_d gameBoy =
    let
        updatedD =
            Word8.subtract gameBoy.cpu.d 1

        updatedCPU =
            gameBoy.cpu
                |> CPU.writeRegister8 D (Word8.subtract gameBoy.cpu.d 1)
                |> CPU.updateFlag Zero (Word8.toInt updatedD == 0)
    in
    { gameBoy | cpu = updatedCPU }


ld_hlplus_a : GameBoy -> GameBoy
ld_hlplus_a gameBoy =
    let
        hl =
            CPU.readRegister16 HL gameBoy.cpu

        updatedHL =
            Word16.add 1 hl

        updatedCPU =
            CPU.writeRegister16 HL updatedHL gameBoy.cpu

        updatedRAM =
            RAM.writeWord8 hl gameBoy.cpu.a gameBoy.ram
    in
    { gameBoy | ram = updatedRAM, cpu = updatedCPU }


jp_nz_a16 : GameBoy -> GameBoy
jp_nz_a16 gameBoy =
    if CPU.getFlag Zero gameBoy.cpu then
        let
            updatedCPU =
                CPU.incrementPC 2 gameBoy.cpu
        in
        { gameBoy | cpu = updatedCPU }

    else
        let
            a16 =
                RAM.readWord16 gameBoy.cpu.pc gameBoy.ram

            updatedCPU =
                gameBoy.cpu
                    |> CPU.writeRegister16 PC a16
        in
        { gameBoy | cpu = updatedCPU }


jp_a16 : GameBoy -> GameBoy
jp_a16 gameBoy =
    let
        a16 =
            RAM.readWord16 gameBoy.cpu.pc gameBoy.ram

        updatedCPU =
            gameBoy.cpu
                |> CPU.writeRegister16 PC a16
    in
    { gameBoy | cpu = updatedCPU }
