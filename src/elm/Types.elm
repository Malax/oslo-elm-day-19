module Types exposing (Model, Msg(..))

import GameBoy exposing (GameBoy)


type alias Model =
    { gameBoy : GameBoy }


type Msg
    = NextInstruction
