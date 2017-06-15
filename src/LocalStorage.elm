port module LocalStorage exposing (..)

import Json.Decode as JD
import Json.Encode as JE

port storeState : JE.Value -> Cmd msg
port getState : () -> Cmd msg

port haveState : (JD.Value -> msg) -> Sub msg
