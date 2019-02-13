module Emulator exposing (emulateInstruction)

import CPU
import GameBoy exposing (GameBoy)
import InstructionHandler exposing (instructionHandlerByOpcode)
import RAM


emulateInstruction : GameBoy -> GameBoy
emulateInstruction gameBoy =
    let
        opcode =
            RAM.readWord8 gameBoy.cpu.pc gameBoy.ram

        instructionHandler =
            instructionHandlerByOpcode opcode

        updatedGameBoy =
            { gameBoy | cpu = CPU.incrementPC 1 gameBoy.cpu }
    in
    instructionHandler updatedGameBoy
