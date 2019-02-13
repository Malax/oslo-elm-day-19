module GameBoy exposing (GameBoy, initWithProgram)

import Array exposing (Array)
import CPU exposing (CPU)
import RAM exposing (RAM)
import Word8 exposing (Word8)


type alias GameBoy =
    { cpu : CPU
    , ram : RAM

    --, apu : APU -- Audio Processing Unit
    --, ppu : PPU -- Pixel Processing Unit
    --, timer : Timer
    --, cartridge : Cartridge
    --, bootRomEnabled : Bool
    --, joypad : Joypad
    --, serialHandler : SerialHandler
    }


initWithProgram : Array Word8 -> GameBoy
initWithProgram program =
    { cpu = CPU.init
    , ram = RAM.initWithProgram program
    }
