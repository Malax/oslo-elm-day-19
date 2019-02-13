module Main exposing (main)

import Browser
import Browser.Events
import Emulator
import ExamplePrograms
import GameBoy
import Json.Decode as Decode
import Types exposing (Model, Msg(..))
import View


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { gameBoy = GameBoy.initWithProgram ExamplePrograms.fibonacci }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextInstruction ->
            ( { model | gameBoy = Emulator.emulateInstruction model.gameBoy }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown decodeKey


decodeKey : Decode.Decoder Msg
decodeKey =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "PageDown" ->
                        Decode.succeed NextInstruction

                    "ArrowRight" ->
                        Decode.succeed NextInstruction

                    _ ->
                        Decode.fail "Uninteresting Key"
            )
